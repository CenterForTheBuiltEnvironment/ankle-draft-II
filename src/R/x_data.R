# Title: Generate dataframes
# Description: Imports raw data and create tidy dataframes for all recorded variables
# Author: Toby Kramer and Junmeng (Patrick) Lyu
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
    session_time_end   = lubridate::mdy_hm(session_time_end, tz = "America/Los_Angeles"),
    block_index = as.integer(sub(".*//", "", session_block)),
    session_block_time_start =
      session_time_start + lubridate::minutes((block_index) * 20),
    session_block_time_end =
      session_block_time_start + lubridate::minutes(20)
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


# Skin Temperature Measurements -------------------------------------------

tsk <- read_csv(
  here::here("data", "01-processed", "tsk", "tsk_combined.csv"),
  col_types = col_tsk
) %>%
  janitor::clean_names() %>%
  dplyr::mutate(timestamp = lubridate::force_tz(timestamp, tz = "America/Los_Angeles"))

# Align tsk measurements with session times
data.table::setDT(tsk)

# Ensure consistent datetime format
tsk[, timestamp := as.POSIXct(timestamp)]
sessions[, c("session_time_start", "session_time_end",
             "session_block_time_start", "session_block_time_end") :=
           lapply(.SD, as.POSIXct),
         .SDcols = c("session_time_start", "session_time_end",
                     "session_block_time_start", "session_block_time_end")]

# Assign each measurement to a session
tsk[, session_id := NA_character_]
tsk[sessions,
    on = .(timestamp >= session_time_start,
           timestamp <= session_time_end),
    session_id := i.session_id]
tsk <- tsk[!is.na(session_id)]

# Assign blocks and workstations within sessions
sessions_block <- sessions[, .(
  session_id,
  session_type,
  block_index,
  session_block_time_start,
  session_block_time_end,
  ws_1, ws_2, ws_3
)]

# Interval join to match measurements with their block and workstation
tsk[sessions_block,
    on = .(session_id,
           timestamp >= session_block_time_start,
           timestamp <= session_block_time_end),
    `:=`(
      session_type = i.session_type,
      block_index  = i.block_index,
      ws_1 = i.ws_1,
      ws_2 = i.ws_2,
      ws_3 = i.ws_3
    )]

# Map subject ID to workstation
tsk[, workstation := data.table::fcase(
  subject_id == ws_1, "ws_1",
  subject_id == ws_2, "ws_2",
  subject_id == ws_3, "ws_3",
  default = NA_character_
)]
tsk <- tsk[!is.na(workstation)]

# Calculate cumulative time within each group
tsk <- dplyr::as_tibble(tsk) %>%
  dplyr::arrange(subject_id, session_id, block_index, tsk_sensing_location, timestamp) %>%
  dplyr::group_by(subject_id, session_id, block_index, tsk_sensing_location) %>%
  dplyr::mutate(
    delta = as.numeric(difftime(timestamp, dplyr::lag(timestamp), units = "secs")),
    delta = tidyr::replace_na(delta, 0),
    running_time_s = cumsum(delta)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-delta) %>%
  dplyr::filter(running_time_s > 0) %>%
  dplyr::select(
    timestamp,
    session_id,
    session_type,
    block_index,
    workstation,
    running_time_s,
    dplyr::everything()
  )


# Air flow Measurements ----------------------------------------------
air <- read_csv(here::here("data", "01-processed", "air_flow", "airflow_combined.csv"), col_types = col_tsk) %>%
  janitor::clean_names() %>%
  dplyr::mutate(timestamp = lubridate::force_tz(timestamp, tz = "America/Los_Angeles"))
air_daily_mean <- air %>%
  mutate(session_date = as.Date(timestamp)) %>%
  group_by(session_date) %>%
  summarise(
    across(
      where(is.numeric) & !time_elapsed_s,
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -session_date,
    names_to = c("level", "variable"),
    names_pattern = "(low|med|high)_(.*)",
    values_to = "value"
  ) %>%
  mutate(
    workstation = case_when(
      level == "low"  ~ "ws02",
      level == "med"  ~ "ws03",
      level == "high" ~ "ws01"
    )
  ) %>%
  select(-level) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  )

# Questionnaire Responses -------------------------------------------------

survey <- read_csv(
  here::here("data", "01-processed", "survey", "survey_combined.csv"),
  col_types = cols(.default = col_guess(), timestamp = col_character()),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(
    timestamp = ymd_hm(gsub("/", "-", timestamp), tz = "America/Los_Angeles")
  )

# Data standardization and aggregation
survey_clean <- survey %>%
  mutate(
    session_date = format(lubridate::ymd(session_date), "%Y/%m/%d"),
    subject_id  = str_to_lower(str_trim(as.character(subject_id))),
    session_id = sprintf("%03d", as.integer(session_id)),
    workstation = str_trim(as.character(workstation)),
    question    = str_trim(as.character(question))
  ) %>%
  arrange(timestamp) %>%
  mutate(question2 = if_else(is_open_text, "open_text", question)) %>%
  group_by(session_id, session_date, workstation, subject_id, question2) %>%
  summarise(response_value = dplyr::last(response_value), .groups = "drop") %>%
  pivot_wider(names_from = question2, values_from = response_value)

# Join with session and airflow metadata
sessions_lookup <- sessions %>%
  mutate(session_id = str_trim(as.character(session_id))) %>%
  select(session_id, session_type, session_diffusor_sat) %>%
  distinct()

air_daily_mean_fmt <- air_daily_mean %>%
  mutate(
    session_date = format(session_date, "%Y/%m/%d"),
    workstation  = tolower(str_trim(workstation))
  )

survey_combine <- survey_clean %>%
  left_join(sessions_lookup, by = "session_id") %>%
  filter(tolower(str_trim(workstation)) != "adaptation") %>%
  left_join(air_daily_mean_fmt, by = c("session_date", "workstation"))

# Calculate derived satisfaction variables
id_cols <- c(
  "session_id", "session_date", "workstation", "subject_id",
  "session_type", "session_diffusor_sat", "open_text"
)

survey_combine <- survey_combine %>%
  mutate(
    dissatisfied_with_draft_ankles = if_else(
      thermal_sensation_ankles < 0 & air_movement_acceptability_ankles < 0 & air_movement_preference_ankles < 0,
      1L, 0L
    ),
    disacceptability_with_draft_ankles = if_else(
      air_movement_acceptability_ankles < 0,
      1L, 0L
    ),
    dispreference_with_draft_ankles = if_else(
      air_movement_preference_ankles < 0,
      1L, 0L
    )
  )

# Convert numeric columns and add subject gender
numeric_cols <- setdiff(names(survey_combine), id_cols)

survey_combine <- survey_combine %>%
  mutate(across(all_of(numeric_cols), ~ as.numeric(as.character(.x)))) %>%
  mutate(
    air_movement_acceptability_ankles = as.numeric(air_movement_acceptability_ankles),
    v_air_s = as.numeric(v_air_s),
    turbulence = as.numeric(turbulence_per_cent) / 100,
    thermal_sensation = as.numeric(thermal_sensation),
    session_diffusor_sat = as.numeric(gsub("[^0-9.]", "", as.character(session_diffusor_sat))),
    session_id = factor(session_id)
  ) %>%
  left_join(
    subjects %>% select(subject_id, gender),
    by = "subject_id"
  ) %>%
  mutate(
    workstation = factor(
      workstation,
      levels = c("ws02", "ws03", "ws01"),
      labels = c("Low", "Medium", "High")
    )
  )

# Final filtering
survey_combine <- survey_combine %>%
  filter(
    is.finite(air_movement_acceptability_ankles),
    is.finite(v_air_s),
    is.finite(session_diffusor_sat),
    !is.na(session_id)
  )

# Cleanup
rm(survey_clean, sessions_lookup, air_daily_mean_fmt, sessions_block, id_cols, numeric_cols)