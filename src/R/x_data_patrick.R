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


# Skin Temperature Measurements -------------------------------------------

tsk <- read_csv(here::here("data", "01-processed", "tsk", "tsk_combined.csv"), 
                col_types = col_tsk) %>%
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



# Air flow Measurements ----------------------------------------------
air <- read_csv(here::here("data", "01-processed", "air_flow", "airflow_combined.csv")) %>%
  janitor::clean_names() %>%
  dplyr::mutate(timestamp = lubridate::force_tz(timestamp, tz = "America/Los_Angeles"))

air_daily_mean <- air %>%
  mutate(session_date = as.Date(timestamp)) %>%
  
  # Each day × measurement_point
  group_by(session_date, measurement_point) %>%
  summarise(
    across(
      where(is.numeric) & !any_of("time_elapsed_s"),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  # In each day，calculate average of all measurement_points
  group_by(session_date) %>%
  summarise(
    across(
      where(is.numeric),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )%>%
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

survey <- read_csv(here::here("data", "01-processed", "survey", "survey_combined.csv"), 
  col_types = cols(
    session_id = col_character(),
    session_date = col_date(format = "%Y-%m-%d"),
    question = col_character(),
    timestamp    = col_character(),
    is_open_text = col_logical()
    )
  )%>%
  janitor::clean_names() %>%
  mutate(timestamp = lubridate::ymd_hms(timestamp, tz = "America/Los_Angeles"))

# --- Standardize Subject ID ---

survey <- survey %>%
  mutate(
    subject_id  = str_to_lower(str_trim(as.character(subject_id)))
  ) 

# --- Reshape survey responses from long to wide format --- 
#     1. Reshape survey responses from long to wide format.
#     2. Create a unified question label (question2), grouping open-text responses
#        under a single column name ("open_text").
# NOTE:
# values_fn = list is intentionally used to preserve all responses per cell.
# If a subject answered the same question multiple times within a session,
# the cell becomes a list-column rather than silently dropping or aggregating values.
# These list-columns are handled explicitly in the next processing step.

survey_processed <- survey %>%
  mutate(question2 = if_else(is_open_text, "open_text", question)) %>%
  select(session_id, session_date, workstation, subject_id, timestamp, 
         question2, response_value) %>%
  pivot_wider(
    id_cols     = c(session_id, session_date, workstation, subject_id),
    names_from  = question2,
    values_from = response_value,
    values_fn   = list
  )

# --- Join with session and airflow metadata ---
# Note: Keep only formal experimental data and exclude data from "adaptation"

survey_processed <- survey_processed %>%
  left_join(
    sessions %>%
      select(session_id, session_type, session_diffusor_sat) %>%
      distinct(),
    by = "session_id"
  )%>%
  filter(workstation != "adaptation") %>%
  left_join(air_daily_mean, by = c("session_date", "workstation"))

# --- Clean dataset due to incomplete participation ---
# Note:
# 1. The experiment consists of 9 sessions in total. A subject who
#    completed all experimental conditions should therefore have 9 records.
# 2. Subjects who did not complete all 9 sessions are identified and excluded.
# Reason:
# Paired-sample statistical tests require strictly matched observations
# across conditions; incomplete cases would violate this requirement.

subject_excluded<- survey_processed %>%
  count(subject_id) %>%
  filter(n != 9)
survey_processed <- survey_processed %>%
  filter(!subject_id %in% subject_excluded$subject_id)

# --- Calculate derived satisfaction variables ---
# Note:
# 1. `dissatisfied_with_draft_ankles` is the primary outcome variable
#    used in Liu’s study and serves as the main modeling target.
# 2. `disacceptability_with_draft_ankles` and
#    `dispreference_with_draft_ankles` are two additional derived variables
#    that may be considered as alternative.

survey_processed <- survey_processed %>%
  mutate(
    dissatisfied_with_draft_ankles = if_else(
      thermal_sensation_ankles < 0 & air_movement_acceptability_ankles < 0 
      & air_movement_preference_ankles < 0,
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

# --- Convert votes to numeric ---
# After `pivot_wider()`, some cells may become list-columns because multiple votes
# can exist for the same subject-condition record due to experimental operating mistakes.
# (ip043 submitted two votes for the same workstation).
# Logic:
# 1. If a cell is a list with a single value → safely coerce to numeric.
# 2. If a cell is empty (NULL or length 0) → set to NA.
# 3. If a cell contains multiple values → stop execution to prevent silent data corruption.
# 4. If the column is already atomic (not a list) → parse directly to numeric.

num_cols <- c(
  "air_movement_acceptability_ankles",
  "thermal_acceptability",
  "thermal_sensation",
  "thermal_preference",
  "thermal_comfort",
  "thermal_sensation_ankles",
  "thermal_preference_ankles",
  "air_movement_preference_ankles",
  "thermal_satisfaction",
  "iaq_preference",
  "thermal_pleasantness",
  "clothing_change"
)
survey_processed <- survey_processed %>%
  mutate(
    across(
      all_of(num_cols),
      ~ if (is.list(.x)) {
        purrr::map_dbl(.x, function(v) {
          if (is.null(v) || length(v) == 0) {
            NA_real_
          } else if (length(v) == 1) {
            readr::parse_double(as.character(v))
          } else {
            stop("Multiple values detected in a cell; cannot coerce to numeric.")
          }
        })
      } else {
        readr::parse_double(as.character(.x))
      }
    )
  )

# --- Derive Workstation ---
# Logic: ws02 → Low; ws03 → Medium; ws01 → High
survey_processed <- survey_processed %>%
  mutate(
    workstation = factor(
      workstation,
      levels = c("ws02", "ws03", "ws01"),
      labels = c("Low", "Medium", "High")
    )
  )
# --- Remove unit labels from supply air temperature variables for subsequent analysis and visualization---
survey_processed <- survey_processed %>%
  mutate(
    session_diffusor_sat = str_remove(session_diffusor_sat, "C")
  )

# Cleanup
rm(subject_excluded, air_daily_mean)
