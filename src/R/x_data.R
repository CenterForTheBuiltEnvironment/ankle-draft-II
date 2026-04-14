# ==============================================================================
# Title: Data Import
# Description: Import raw data and create tidy dataframes for all variables
# Author: Toby Kramer, Junmeng Lyu
# Date: 2026-01-05
# ==============================================================================

source(here::here("src", "R", "x_setup.R"))
source(here::here("src", "R", "x_func.R"))


# Metadata =====================================================================

# Subject metadata -------------------------------------------------------------

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
  dplyr::rename(sex = gender) %>%
  dplyr::select(-height_ft, -height_in, -weight_lbs, -birth_year) %>%
  dplyr::select(subject_id, age, height_m, weight_kg, everything())


# Session metadata -------------------------------------------------------------

sessions <- read_csv(
  here::here("data", "00-raw", "metadata", "session_metadata.csv"),
  col_types = col_sessions
) %>%
  janitor::clean_names() %>%
  dplyr::rename(session_sat = session_diffusor_sat) %>%
  dplyr::mutate(
    session_time_start = lubridate::mdy_hm(
      session_time_start, tz = "America/Los_Angeles"
    ),
    session_time_end = lubridate::mdy_hm(
      session_time_end, tz = "America/Los_Angeles"
    )
  )


# Environmental Measurements ===================================================

env <- read_csv(
  here::here("data", "01-processed", "env", "df_env.csv")
) %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    timestamp = lubridate::as_datetime(timestamp, tz = "America/Los_Angeles")
  )

# Limit env to session times using interval overlap join
setDT(env)
setDT(sessions)

env[, `:=`(
  session_time_start = timestamp,
  session_time_end = timestamp
)]

setkey(sessions, session_time_start, session_time_end)
setkey(env, session_time_start, session_time_end)

env <- foverlaps(env, sessions, nomatch = 0L) %>%
  dplyr::select(
    "timestamp", "session_id", "session_type", "session_sat",
    "t_air_c", "rh_percent", "co2_ppm", "pm25_ug_m3",
    "voc_ppm", "noise_db", "light_lux"
  )

# Session-level environmental summaries
env_sessions <- env %>%
  group_by(session_id, session_type, session_sat) %>%
  summarise(
    across(
      .cols = c(t_air_c, rh_percent, co2_ppm),
      .fns = mean,
      na.rm = TRUE
    ) %>%
      dplyr::mutate(across(where(is.numeric), \(x) round(x, 2)))
  )


# Airflow Measurements =========================================================
#
# Airflow measurements were taken at 6 positions (dot1-dot6) before sessions
# to characterize the air velocity profile at different supply conditions.
#
# Matching logic: Each airflow measurement set is matched to ALL sessions that:
# 1. Occur on the same date as the measurement
# 2. Start AFTER the measurement timestamp

airflow <- read_csv(
  here::here("data", "01-processed", "air_flow", "airflow_combined.csv"),
  show_col_types = FALSE
) %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    # Timestamps are labeled UTC but are actually local Pacific time
    timestamp = lubridate::force_tz(
      lubridate::as_datetime(timestamp),
      tzone = "America/Los_Angeles"
    ),
    date = lubridate::as_date(timestamp)
  )

# Prepare sessions for matching
sessions_for_airflow <- sessions %>%
  as.data.frame() %>%
  dplyr::select(session_id, session_time_start, session_type, session_sat) %>%
  dplyr::distinct() %>%
  dplyr::filter(session_id != "CANCEL") %>%
  dplyr::mutate(date = lubridate::as_date(session_time_start))

# Join airflow to sessions by date where session starts after measurement
airflow_with_sessions <- airflow %>%
  dplyr::inner_join(
    sessions_for_airflow, by = "date", relationship = "many-to-many"
  ) %>%
  dplyr::filter(session_time_start > timestamp) %>%
  dplyr::select(-date, -session_time_start, -timestamp, -time_elapsed_s) %>%
  # Convert turbulence from percentage to decimal (to match Liu et al. format)
  dplyr::mutate(across(contains("turbulence"), \(x) x / 100)) %>%
  dplyr::rename_with(\(x) gsub("turbulence_per_cent", "turbulence_intensity", x))

# Mean by session_id and measurement_point
airflow_sessions_dot_mean <- airflow_with_sessions %>%
  dplyr::group_by(session_id, measurement_point) %>%
  dplyr::summarise(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
  dplyr::arrange(session_id, measurement_point)

# Mean by session_id only (averaged across all dots)
airflow_sessions_all_mean <- airflow_with_sessions %>%
  dplyr::group_by(session_id, session_type, session_sat) %>%
  dplyr::summarise(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
  dplyr::arrange(session_id)

# Save summary files
write_csv(
  airflow_sessions_dot_mean,
  here::here("data", "01-processed", "air_flow", "airflow_sessions_dot_mean.csv")
)

write_csv(
  airflow_sessions_all_mean,
  here::here("data", "01-processed", "air_flow", "airflow_sessions_all_mean.csv")
)

rm(sessions_for_airflow, airflow_with_sessions, airflow_sessions_dot_mean)


# Skin Temperature Measurements ================================================

tsk <- read_csv(
  here::here("data", "01-processed", "tsk", "tsk_combined.csv"),
  col_types = col_tsk
) %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    timestamp = lubridate::force_tz(timestamp, tz = "America/Los_Angeles")
  )

# Add session metadata via interval join
setDT(tsk)
setDT(sessions)

tsk[, timestamp := as.POSIXct(timestamp)]
sessions[, c("session_time_start", "session_time_end") :=
           lapply(.SD, as.POSIXct),
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
  dplyr::left_join(
    sessions %>% select(session_id, session_type) %>% distinct(),
    by = "session_id"
  ) %>%
  dplyr::arrange(session_id, subject_id, tsk_sensing_location, timestamp) %>%
  # Add running_time_s column
  dplyr::group_by(subject_id, session_id) %>%
  dplyr::mutate(
    delta = as.numeric(difftime(timestamp, lag(timestamp), units = "secs")),
    delta = replace_na(delta, 0),
    running_time_s = cumsum(delta)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    running_time = if_else(is.na(session_id), NA_real_, running_time_s)
  ) %>%
  dplyr::select(-delta, -running_time) %>%
  dplyr::filter(running_time_s > 0) %>%
  dplyr::select(
    timestamp, session_id, session_type, running_time_s, everything()
  )


# Questionnaire Responses ======================================================

survey <- read_csv(
  here::here("data", "01-processed", "survey", "survey_combined.csv"),
  col_types = col_survey
) %>%
  janitor::clean_names() %>%
  # Adjust workstation levels/labels
  dplyr::mutate(
    workstation = factor(
      workstation,
      levels = workstation_levels,
      labels = workstation_labels
    )
  ) %>%
  # Add session_sat
  dplyr::left_join(
    sessions %>%
      as.data.frame() %>%
      dplyr::select(session_id, session_sat) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        session_sat = factor(
          session_sat,
          levels = session_sat_levels,
          labels = session_sat_labels
        )
      ) %>%
      dplyr::select(session_id, session_sat),
    by = "session_id"
  ) %>%
  dplyr::select(
    session_id, session_date, session_sat, workstation, everything()
  ) %>%
  dplyr::mutate(
    timestamp = lubridate::as_datetime(timestamp, tz = "America/Los_Angeles")
  )


# Analysis Dataframe ===========================================================
#
# Combines survey responses with environmental and airflow measurements.
#
# Airflow workstation mapping:
# - ws01 -> high_* columns (high air speed position)
# - ws02 -> low_* columns (low air speed position)
# - ws03 -> med_* columns (medium air speed position)
# - adaptation -> NA (no airflow data for adaptation period)

# Join survey with environmental data (session-level means)
analysis <- survey %>%
  dplyr::left_join(env_sessions, by = "session_id")

# Join with airflow data
analysis <- analysis %>%
  dplyr::left_join(airflow_sessions_all_mean, by = "session_id")

# Join with subject metadata (sex)
analysis <- analysis %>%
  dplyr::left_join(subjects %>% dplyr::select(subject_id, sex), by = "subject_id")

# Select correct airflow columns based on workstation
analysis <- analysis %>%
  dplyr::mutate(
    # Air velocity (m/s)
    v_air_m_s = dplyr::case_when(
      workstation == "high" ~ high_v_air_s,
      workstation == "low" ~ low_v_air_s,
      workstation == "medium" ~ med_v_air_s,
      TRUE ~ NA_real_
    ),
    # Supply air temperature (C)
    t_supply_c = dplyr::case_when(
      workstation == "high" ~ high_t_supply_c,
      workstation == "low" ~ low_t_supply_c,
      workstation == "medium" ~ med_t_supply_c,
      TRUE ~ NA_real_
    ),
    # Air velocity standard deviation (m/s)
    v_air_sd_m_s = dplyr::case_when(
      workstation == "high" ~ high_v_air_sd_m_s,
      workstation == "low" ~ low_v_air_sd_m_s,
      workstation == "medium" ~ med_v_air_sd_m_s,
      TRUE ~ NA_real_
    ),
    # Turbulence intensity (decimal)
    turbulence_intensity = dplyr::case_when(
      workstation == "high" ~ high_turbulence_intensity,
      workstation == "low" ~ low_turbulence_intensity,
      workstation == "medium" ~ med_turbulence_intensity,
      TRUE ~ NA_real_
    ),
    # Dynamic range (%)
    dynamic_range_pct = dplyr::case_when(
      workstation == "high" ~ high_dynamic_range_per_cent,
      workstation == "low" ~ low_dynamic_range_per_cent,
      workstation == "medium" ~ med_dynamic_range_per_cent,
      TRUE ~ NA_real_
    )
  ) %>%
  # Remove the prefixed airflow columns
  dplyr::select(
    -starts_with("low_"),
    -starts_with("med_"),
    -starts_with("high_")
  ) %>%
  # Round measurements to 2 decimal places
  dplyr::mutate(
    across(
      c(t_air_c, rh_percent, co2_ppm, v_air_m_s, t_supply_c,
        v_air_sd_m_s, turbulence_intensity, dynamic_range_pct),
      \(x) round(x, 2)
    )
  ) %>%
  # Reorder columns
  dplyr::select(
    timestamp, session_id, session_date, session_sat, subject_id, sex, workstation,
    t_air_c, rh_percent, co2_ppm,
    v_air_m_s, t_supply_c, v_air_sd_m_s, turbulence_intensity, dynamic_range_pct,
    question, is_open_text, response_value
  )


# Handle Duplicate Votes -------------------------------------------------------
# Subject ip043 submitted duplicate votes for the same workstation in some
# sessions due to an experimental operating mistake. We detect true duplicates
# as votes for the same subject-session-workstation-question.
# Kept the first observation and removed the second one.

analysis <- analysis %>%
  dplyr::arrange(session_id, subject_id, workstation, question, timestamp) %>%
  dplyr::group_by(session_id, subject_id, workstation, question) %>%
  dplyr::mutate(row_in_group = dplyr::row_number()) %>%
  dplyr::ungroup() 

n_duplicates <- sum(
  analysis$row_in_group > 1 & analysis$workstation != "adaptation",
  na.rm = TRUE
)

if (n_duplicates > 0) {
  message(
    "  - Duplicate votes detected: ", n_duplicates,
    " (keeping first response only)"
  )
}

analysis <- analysis %>%
  dplyr::filter(workstation == "adaptation" | row_in_group == 1) %>%
  dplyr::select(-row_in_group)

rm(n_duplicates)


# Derived Dissatisfaction Variables --------------------------------------------
# Computed following Liu et al. methodology for draft discomfort.
#
# Definitions:
# - dissatisfied_with_draft_ankles: thermal_sensation_ankles < 0 AND
#     air_movement_acceptability_ankles

derived_source_questions <- c(
  "thermal_sensation_ankles",
  "air_movement_acceptability_ankles",
  "air_movement_preference_ankles"
)

# Pivot to wide format temporarily to compute derived variables
analysis_wide_temp <- analysis %>%
  dplyr::filter(question %in% derived_source_questions) %>%
  dplyr::select(
    session_id, session_date, session_sat, subject_id, sex, workstation,
    t_air_c, rh_percent, co2_ppm, v_air_m_s, t_supply_c,
    v_air_sd_m_s, turbulence_intensity, dynamic_range_pct,
    question, response_value
  ) %>%
  dplyr::mutate(response_value = as.numeric(response_value)) %>%
  tidyr::pivot_wider(
    id_cols = c(
      session_id, session_date, session_sat, subject_id, sex, workstation,
      t_air_c, rh_percent, co2_ppm, v_air_m_s, t_supply_c,
      v_air_sd_m_s, turbulence_intensity, dynamic_range_pct
    ),
    names_from = question,
    values_from = response_value,
    values_fn = mean
  )

# Compute derived variables
derived_vars <- analysis_wide_temp %>%
  dplyr::mutate(
    dissatisfied_with_draft_ankles = dplyr::if_else(
      thermal_sensation_ankles < 0 &
        air_movement_acceptability_ankles < 0,
      1L, 0L
    )
  ) %>%
  dplyr::select(
    session_id, session_date, session_sat, subject_id, sex, workstation,
    t_air_c, rh_percent, co2_ppm, v_air_m_s, t_supply_c,
    v_air_sd_m_s, turbulence_intensity, dynamic_range_pct,
    dissatisfied_with_draft_ankles
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      dissatisfied_with_draft_ankles
    ),
    names_to = "question",
    values_to = "response_value"
  ) %>%
  dplyr::mutate(
    timestamp = NA_POSIXct_,
    is_open_text = FALSE,
    response_value = as.character(response_value)
  ) %>%
  dplyr::select(names(analysis))

# Append derived variables to analysis
analysis <- dplyr::bind_rows(analysis, derived_vars)

rm(analysis_wide_temp, derived_vars, derived_source_questions)

# Save analysis dataframe
write_csv(
  analysis,
  here::here("data", "02-export", "df_analysis.csv")
)


# Prior Work: Liu et al. 2017 ==================================================
#
# Preprocesses data from Liu et al. 2017 to match the current study's
# conventions. The original dataset contains thermal comfort survey responses
# and environmental measurements from a controlled laboratory experiment.
#
# Key decisions:
# - Subject IDs: Prefixed with "liu_" and zero-padded to 3 digits (e.g., liu_018)
# - Temperature mapping: Thead (head-level) -> t_air_c; Tsp (supply) -> t_supply_c
# - Workstation: Derived from Seat + Seq columns

# Column mapping: Q_name to question
liu_qname_mapping <- c(
  "LabThermalSensation"           = "thermal_sensation",
  "LabThermalAcceptability"       = "thermal_acceptability",
  "ThermalPreference"             = "thermal_preference",
  "LabThermalSensationAnkles"     = "thermal_sensation_ankles",
  "LabThermalSensationTorso"      = "thermal_sensation_torso",
  "LabThermalSensationHands"      = "thermal_sensation_hands",
  "LabThermalSensationHead"       = "thermal_sensation_head",
  "LabThermalAcceptabilityAnkles" = "thermal_acceptability_ankles",
  "LabThermalAcceptabilityTorso"  = "thermal_acceptability_torso",
  "LabThermalAcceptabilityHands"  = "thermal_acceptability_hands",
  "LabThermalAcceptabilityHead"   = "thermal_acceptability_head",
  "LabThermalComfortAcceptability" = "thermal_comfort",
  "LabComfortableNow"             = "thermal_comfort_binary",
  "LabAcceptability"              = "acceptance_longterm_thermal_environment",
  "LabAnkleAirMovement"           = "air_movement_acceptability",
  "AirMovementPreference"         = "air_movement_preference",
  "LabIAQNow"                     = "iaq_preference"
)


# Read Liu dataset -------------------------------------------------------------

liu_raw <- read_csv(
  here::here("data", "00-raw", "prior_work", "liu_et_al_2017.csv"),
  col_types = cols(
    Date = col_character(),
    Time = col_double(),
    Seat = col_integer(),
    Q_name = col_character(),
    Vote = col_double(),
    Seq = col_character(),
    Tsp = col_double(),
    Thead = col_double(),
    Vsp = col_double(),
    Tisp = col_double(),
    SubID = col_integer(),
    Sex = col_character(),
    Age = col_integer(),
    Weight = col_double(),
    Height = col_double(),
    BMI = col_double(),
    Berstay.sixm = col_integer(),
    ColdSens = col_integer(),
    ColdExp = col_character(),
    Workhr = col_double(),
    Coffeeintake = col_double(),
    Dress = col_character()
  )
)


# Transform Liu data -----------------------------------------------------------

# Subject ID: liu_XXX (3-digit zero-padded)
liu_raw <- liu_raw %>%
  dplyr::mutate(subject_id = sprintf("liu_%03d", SubID))

# Create timestamp from Date (M/D/YYYY) and Time (decimal hours)
liu_raw <- liu_raw %>%
  dplyr::mutate(
    date_parsed = lubridate::mdy(Date),
    hours = floor(Time),
    minutes = round((Time - hours) * 60),
    timestamp = lubridate::make_datetime(
      year = lubridate::year(date_parsed),
      month = lubridate::month(date_parsed),
      day = lubridate::day(date_parsed),
      hour = hours,
      min = minutes,
      tz = "America/Los_Angeles"
    )
  ) %>%
  dplyr::select(-date_parsed, -hours, -minutes)

# Derive workstation from Seq and Seat
liu_raw <- liu_raw %>%
  dplyr::mutate(
    workstation = dplyr::case_when(
      Seq == "Adapt" ~ "adaptation",
      Seat == 1 ~ "ws01_liu",
      Seat == 2 ~ "ws02_liu",
      Seat == 3 ~ "ws03_liu",
      TRUE ~ NA_character_
    )
  )

# Apply factor conversions
liu_raw <- liu_raw %>%
  dplyr::mutate(
    sex = tolower(Sex),
    clothing_type = tolower(Dress),
    # Caffeine: numeric to categorical
    health_routines_caffein = dplyr::case_when(
      is.na(Coffeeintake) ~ NA_character_,
      Coffeeintake == 0 ~ "never",
      Coffeeintake <= 0.5 ~ "rarely",
      Coffeeintake <= 1.5 ~ "sometimes",
      TRUE ~ "daily"
    )
  )

# Rename columns
liu_raw <- liu_raw %>%
  dplyr::mutate(
    t_supply_c = Tsp,
    t_air_c = Thead,
    v_air_m_s = Vsp,
    turbulence_intensity = Tisp,
    question = liu_qname_mapping[Q_name],
    response_value = Vote,
    birth_year = 2015L - Age,
    height_m = Height,
    weight_kg = Weight,
    bmi = BMI,
    sensitivity_cold = ColdSens,
    cold_exposure = ColdExp
  )

# Filter out unmapped Q_names
liu_processed <- liu_raw %>%
  dplyr::filter(!is.na(question))

# Recode response scales to match current study
# thermal_preference: Liu uses 1,2,3 -> transform to -1,0,1
liu_processed <- liu_processed %>%
  dplyr::mutate(
    response_value = dplyr::case_when(
      question == "thermal_preference" ~ response_value - 2,
      TRUE ~ response_value
    )
  )


# Extract Liu subject metadata -------------------------------------------------

subjects_liu <- liu_processed %>%
  dplyr::select(
    subject_id, sex, birth_year, height_m, weight_kg, bmi,
    sensitivity_cold, cold_exposure, health_routines_caffein
  ) %>%
  dplyr::distinct(subject_id, .keep_all = TRUE) %>%
  dplyr::mutate(
    # Add columns from current study structure (set to NA where not available)
    timestamp = NA_character_,
    ethnicity = NA_character_,
    ethnicity_other_text = NA_character_,
    height_ft = NA_real_,
    height_in = NA_real_,
    weight_lbs = NA_real_,
    living_location = NA_character_,
    living_location_past = NA_character_,
    health_routines_exercise = NA_character_,
    health_routines_alcohol = NA_character_,
    health_routines_smoking = NA_character_,
    health_problems = NA_character_,
    health_problems_other_text = NA_character_,
    sensitivity_thermal = NA_integer_,
    sensitivity_hands = NA_character_,
    sensitivity_feet = NA_character_
  ) %>%
  dplyr::select(
    timestamp, subject_id, sex, birth_year, ethnicity, ethnicity_other_text,
    height_ft, height_in, weight_lbs, living_location, living_location_past,
    health_routines_exercise, health_routines_caffein, health_routines_alcohol,
    health_routines_smoking, health_problems, health_problems_other_text,
    sensitivity_thermal, sensitivity_hands, sensitivity_feet,
    height_m, weight_kg, bmi, sensitivity_cold, cold_exposure
  )


# Create Liu analysis dataset --------------------------------------------------

analysis_liu <- liu_processed %>%
  dplyr::select(
    subject_id, timestamp, workstation, clothing_type,sex,
    t_supply_c, t_air_c, v_air_m_s, turbulence_intensity,
    question, response_value
  ) %>%
  dplyr::arrange(subject_id, timestamp, question)


# Save Liu output files --------------------------------------------------------

write_csv(
  subjects_liu,
  here::here("data", "01-processed", "metadata", "subject_metadata_liu.csv")
)

write_csv(
  analysis_liu,
  here::here("data", "02-export", "df_analysis_liu.csv")
)

rm(liu_raw, liu_processed, liu_qname_mapping)


# Combined Subject Metadata ====================================================
#
# Combines metadata from current study and Liu et al. 2017 into a single file.
# Uses current study structure as base, with metric units for body measurements.

# Read current study metadata and convert to metric
current_subjects <- read_csv(
  here::here("data", "01-processed", "metadata", "subject_metadata.csv"),
  show_col_types = FALSE
) %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    height_m = round((height_ft * 12 + height_in) * 0.0254, 2),
    weight_kg = round(weight_lbs * 0.45359237, 1),
    study = "current"
  ) %>%
  dplyr::select(
    subject_id, gender, birth_year, height_m, weight_kg,
    ethnicity, ethnicity_other_text, living_location, living_location_past,
    health_routines_exercise, health_routines_caffein, health_routines_alcohol,
    health_routines_smoking, health_problems, health_problems_other_text,
    sensitivity_thermal, sensitivity_hands, sensitivity_feet,
    study
  )

# Prepare Liu data with matching columns
liu_subjects_combined <- subjects_liu %>%
  dplyr::mutate(study = "liu_2017") %>%
  dplyr::select(
    subject_id, sex, birth_year, height_m, weight_kg,
    ethnicity, ethnicity_other_text, living_location, living_location_past,
    health_routines_exercise, health_routines_caffein, health_routines_alcohol,
    health_routines_smoking, health_problems, health_problems_other_text,
    sensitivity_thermal, sensitivity_hands, sensitivity_feet,
    study
  )

# Combine both datasets
subjects_combined <- dplyr::bind_rows(current_subjects, liu_subjects_combined) %>%
  dplyr::arrange(study, subject_id)

rm(liu_subjects_combined, subjects_liu)

# Save combined metadata
write_csv(
  subjects_combined,
  here::here("data", "01-processed", "metadata", "subject_metadata_combined.csv")
)

rm(current_subjects)