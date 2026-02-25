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




# Prior Work: Liu et al. 2017 ---------------------------------------------
#
# This section preprocesses data from Liu et al. 2017 to match the current study's
# conventions. The original dataset contains thermal comfort survey responses and
# environmental measurements from a controlled laboratory experiment.
#
# KEY DECISIONS:
# - Subject IDs: Prefixed with "liu_" and zero-padded to 3 digits (e.g., liu_018)
#   to avoid overlap with current study's alphanumeric IDs
# - Temperature mapping: Thead (head-level) → t_air_c (equivalent to current study's
#   ambient measurement); Tsp (supply) → t_supply_c
# - Workstation: Derived from Seat + Seq columns. "Adapt" phase → "adaptation",
#   otherwise "ws01_liu"/"ws02_liu"/"ws03_liu" based on seat number
#
# DROPPED COLUMNS: Workhr, Berstay.sixm, LabImprovementMeasures (not relevant for analysis)
#
# FACTOR CONVERSIONS:
# - gender: "Female"/"Male" → "female"/"male"
# - clothing_type: "Long"/"Short" → "long"/"short"
# - health_routines_caffein: numeric (cups/day) → categorical (never/rarely/sometimes/daily)
#
# METADATA EXTENSIONS:
# - New columns added to metadata schema: height_m, weight_kg, bmi, sensitivity_cold, cold_exposure

# --- Column Mapping Definitions ---

# Q_name to question mapping (survey responses)
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
  "LabThermalComfortAcceptability"= "thermal_comfort",
  "LabComfortableNow"             = "thermal_comfort_binary",
  "LabAcceptability"              = "acceptance_longterm_thermal_environment",
  "LabAnkleAirMovement"           = "air_movement_acceptability",
  "AirMovementPreference"         = "air_movement_preference",
  "LabIAQNow"                     = "iaq_preference"
  # LabImprovementMeasures is dropped
)

# --- Read Liu Dataset ---

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

# --- Transform Subject ID ---
# Format: liu_XXX (3-digit zero-padded)

liu_raw <- liu_raw %>%
  dplyr::mutate(
    subject_id = sprintf("liu_%03d", SubID)
  )

# --- Create Timestamp ---
# Combine Date (M/D/YYYY) and Time (decimal hours) into POSIXct
# Study was conducted around 2015, dates are in M/D/YYYY format

liu_raw <- liu_raw %>%
  dplyr::mutate(
    # Parse date
    date_parsed = lubridate::mdy(Date),
    # Convert decimal hours to hours and minutes
    hours = floor(Time),
    minutes = round((Time - hours) * 60),
    # Combine into timestamp
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

# --- Derive Workstation ---
# Logic: If Seq="Adapt" → "adaptation", otherwise "ws01_liu"/"ws02_liu"/"ws03_liu" based on Seat

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

# --- Apply Factor Conversions ---

liu_raw <- liu_raw %>%
  dplyr::mutate(
    # Gender: lowercase
    gender = tolower(Sex),
    # Clothing type: lowercase
    clothing_type = tolower(Dress),
    # Caffeine: numeric to categorical
    # 0 → "never", 0.1-0.5 → "rarely", 0.6-1.5 → "sometimes", >1.5 → "daily"
    health_routines_caffein = dplyr::case_when(
      is.na(Coffeeintake) ~ NA_character_,
      Coffeeintake == 0 ~ "never",
      Coffeeintake <= 0.5 ~ "rarely",
      Coffeeintake <= 1.5 ~ "sometimes",
      TRUE ~ "daily"
    )
  )

# --- Rename Columns ---

liu_raw <- liu_raw %>%
  dplyr::mutate(
    # Environmental columns
    t_supply_c = Tsp,
    t_air_c = Thead,
    v_air_m_s = Vsp,
    turbulence_intensity = Tisp,
    # Survey columns
    question = liu_qname_mapping[Q_name],
    response_value = Vote,
    # Metadata columns (for extraction)
    birth_year = 2015L - Age,
    height_m = Height,
    weight_kg = Weight,
    bmi = BMI,
    sensitivity_cold = ColdSens,
    cold_exposure = ColdExp
  )

# --- Filter out dropped Q_names ---

liu_processed <- liu_raw %>%
  dplyr::filter(!is.na(question))  # Drops LabImprovementMeasures (not in mapping)

# --- Recode Response Scales ---
# thermal_preference: Liu uses 1,2,3 (cooler, no change, warmer)
#                     Current study uses -1,0,1 (same meaning)
#                     Transform: 1→-1, 2→0, 3→1

liu_processed <- liu_processed %>%
  dplyr::mutate(
    response_value = dplyr::case_when(
      question == "thermal_preference" ~ response_value - 2,  # 1→-1, 2→0, 3→1
      TRUE ~ response_value
    )
  )

# --- Extract Subject Metadata ---
# One row per unique subject, matching subject_metadata.csv structure plus new columns

liu_subjects <- liu_processed %>%
  dplyr::select(
    subject_id, gender, birth_year, height_m, weight_kg, bmi,
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
  # Reorder to match current study structure, then add new columns
  dplyr::select(
    timestamp, subject_id, gender, birth_year, ethnicity, ethnicity_other_text,
    height_ft, height_in, weight_lbs, living_location, living_location_past,
    health_routines_exercise, health_routines_caffein, health_routines_alcohol,
    health_routines_smoking, health_problems, health_problems_other_text,
    sensitivity_thermal, sensitivity_hands, sensitivity_feet,
    # New columns specific to Liu data
    height_m, weight_kg, bmi, sensitivity_cold, cold_exposure
  )

# --- Create Analysis Dataset ---
# Combined survey + environmental data in long format

liu_analysis <- liu_processed %>%
  dplyr::select(
    subject_id,
    timestamp,
    workstation,
    clothing_type,
    # Environmental measurements
    t_supply_c,
    t_air_c,
    v_air_m_s,
    turbulence_intensity,
    # Survey data (long format)
    question,
    response_value
  ) %>%
  dplyr::arrange(subject_id, timestamp, question)

# --- Save Output Files ---

# Create output directories if they don't exist
dir.create(here::here("data", "02-export"), showWarnings = FALSE, recursive = TRUE)

# Save subject metadata
write_csv(
  liu_subjects,
  here::here("data", "01-processed", "metadata", "subject_metadata_liu.csv")
)

# Save analysis data
write_csv(
  liu_analysis,
  here::here("data", "02-export", "analysis_liu.csv")
)

# Print summary
message("Liu et al. 2017 preprocessing complete:")
message("  - Subjects: ", n_distinct(liu_subjects$subject_id))
message("  - Observations: ", nrow(liu_analysis))
message("  - Questions: ", n_distinct(liu_analysis$question))
message("  - Output: data/01-processed/metadata/subject_metadata_liu.csv")
message("
  - Output: data/02-export/analysis_liu.csv")


# Combined Subject Metadata -----------------------------------------------
#
# Combines metadata from current study and Liu et al. 2017 into a single file.
# Uses current study structure as base, with metric units for body measurements.
# Liu data only includes columns that exist in the current study structure.
# Timestamp column is dropped as it's not meaningful for combined analysis.

# Read current study metadata and convert to metric
current_subjects <- read_csv(
  here::here("data", "01-processed", "metadata", "subject_metadata.csv"),
  show_col_types = FALSE
) %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    # Convert imperial to metric
    height_m = round((height_ft * 12 + height_in) * 0.0254, 2),
    weight_kg = round(weight_lbs * 0.45359237, 1),
    # Add study source identifier
    study = "current"
  ) %>%
  # Select columns for combined file (drop timestamp and imperial units)
  dplyr::select(
    subject_id,
    gender,
    birth_year,
    height_m,
    weight_kg,
    ethnicity,
    ethnicity_other_text,
    living_location,
    living_location_past,
    health_routines_exercise,
    health_routines_caffein,
    health_routines_alcohol,
    health_routines_smoking,
    health_problems,
    health_problems_other_text,
    sensitivity_thermal,
    sensitivity_hands,
    sensitivity_feet,
    study
  )

# Prepare liu data with matching columns (NA for columns not available)
liu_subjects_combined <- liu_subjects %>%
  dplyr::mutate(
    study = "liu_2017"
  ) %>%
  dplyr::select(
    subject_id,
    gender,
    birth_year,
    height_m,
    weight_kg,
    ethnicity,
    ethnicity_other_text,
    living_location,
    living_location_past,
    health_routines_exercise,
    health_routines_caffein,
    health_routines_alcohol,
    health_routines_smoking,
    health_problems,
    health_problems_other_text,
    sensitivity_thermal,
    sensitivity_hands,
    sensitivity_feet,
    study
  )

# Combine both datasets
subjects_combined <- dplyr::bind_rows(current_subjects, liu_subjects_combined) %>%
  dplyr::arrange(study, subject_id)

# Save combined metadata
write_csv(
  subjects_combined,
  here::here("data", "01-processed", "metadata", "subject_metadata_combined.csv")
)

message("\nCombined metadata created:")
message("  - Current study subjects: ", sum(subjects_combined$study == "current"))
message("  - Liu et al. subjects: ", sum(subjects_combined$study == "liu_2017"))
message("  - Total subjects: ", nrow(subjects_combined))
message("  - Output: data/01-processed/metadata/subject_metadata_combined.csv")

