# Title: Generate dataframes
# Description: Imports raw data and create tidy dataframes for all recorded variables
# Author: Toby Kramer
# Date: 2026-01-05

source(here::here("src", "R", "x_setup.R"))
source(here::here("src", "R", "x_func.R"))


# Metadata: Subjects + Sessions -------------------------------------------

subjects <- read_csv(
  here::here("data", "01-processed", "metadata", "subject_metadata.csv"),
  col_types = col_subjects
) %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    height_m = round((height_ft * 12 + height_in) * 0.0254, 2),
    weight_kg = round(weight_lbs * 0.45359237, 0),
    age = 2025 - birth_year
  ) %>%
  dplyr::select(-height_ft, -height_in, -weight_lbs, -birth_year) %>%
  dplyr::select(subject_id, age, height_m, weight_kg, everything())


sessions <- read_csv(here::here("data", "00-raw", "metadata", "session_metadata.csv"), col_types = col_sessions) %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    session_time_start = lubridate::mdy_hm(session_time_start, tz = "America/Los_Angeles"),
    session_time_end   = lubridate::mdy_hm(session_time_end, tz = "America/Los_Angeles")
  )


# Environmental Measurements ----------------------------------------------

env <- read_csv(here::here("data", "01-processed", "env", "df_env.csv")) %>%
  janitor::clean_names() %>%
  dplyr::mutate(timestamp = lubridate::as_datetime(timestamp, tz = "America/Los_Angeles"))


# Limit env to session times
setDT(env)
setDT(sessions)

env[, `:=`(
  session_time_start = timestamp,
  session_time_end = timestamp
)]

setkey(sessions, session_time_start, session_time_end)
setkey(env, session_time_start, session_time_end)

env <- foverlaps(env, sessions, nomatch = 0L) %>%
  dplyr::select("timestamp",
                "session_id",
                "session_type",
                "t_air_c",
                "rh_percent",
                "co2_ppm",
                "pm25_ug_m3",
                "voc_ppm",
                "noise_db",
                "light_lux")

env_sessions <- env %>%
  group_by(session_id) %>%
  summarise(
    across(
      .cols = c(t_air_c, rh_percent, co2_ppm),
      .fns = mean,
      na.rm = TRUE
    )
  )


# Skin Temperature Measurements ----------------------------------------------

tsk <- read_csv(here::here("data", "01-processed", "tsk", "tsk_combined.csv"), col_types = col_tsk) %>%
  janitor::clean_names() %>%
  dplyr::mutate(timestamp = lubridate::force_tz(timestamp, tz = "America/Los_Angeles"))

# Add session metadata
setDT(tsk)
setDT(sessions)

tsk[, timestamp := as.POSIXct(timestamp)]
sessions[, c("session_time_start", "session_time_end") := lapply(.SD, as.POSIXct),
  .SDcols = c("session_time_start", "session_time_end")
]

tsk[, session_id := NA_character_]
tsk[sessions,
  on = .(
    timestamp >= session_time_start,
    timestamp <= session_time_end
  ),
  session_id := i.session_id
]

tsk <- tsk %>%
  dplyr::left_join(sessions %>% select(session_id, session_type) %>% distinct(),
    by = "session_id"
  ) %>%
  dplyr::arrange(session_id, subject_id, tsk_sensing_location, timestamp) %>%
  # add running_time_s column
  dplyr::group_by(subject_id, session_id) %>%
  dplyr::mutate(
    delta = as.numeric(difftime(timestamp, lag(timestamp), units = "secs")),
    delta = replace_na(delta, 0),
    running_time_s = cumsum(delta)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(running_time = if_else(is.na(session_id), NA_real_, running_time_s)) %>%
  dplyr::select(-delta, -running_time) %>%
  dplyr::filter(running_time_s > 0) %>% 
  dplyr::select(timestamp, session_id, session_type, running_time_s, everything())


# Questionnaire Responses -------------------------------------------------

# Read raw survey data
survey <- read_csv(here::here("data", "01-processed", "survey", "survey_combined.csv"), col_types = col_survey) %>%
  janitor::clean_names() %>%
  dplyr::mutate(timestamp = lubridate::as_datetime(timestamp, tz = "America/Los_Angeles"))

# more preprocessing required here
