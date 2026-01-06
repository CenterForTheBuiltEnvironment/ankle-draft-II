
pacman::p_load(
  here, dplyr, ggplot2, readr, tidyr, purrr, stringr, lubridate, arrow,
  janitor, scales, glue, yaml, data.table, rstatix, patchwork, pwr
)


# Global settings ---------------------------------------------------------

single_col_width = 89 #mm
double_col_width = 183 #mm



# Column levels and dtypes ------------------------------------------------

thermal_sensation_levels = c("cold","cool","slightly cool", "neutral","slightly warm", "warm", "hot")
thermal_preference_levels = c("cooler", "no change", "warmer")
air_speed_preference_levels = c("lower", "no change", "higher")
acceptability_levels = c("very unacceptable", "somewhat unacceptable", "neither acceptable nor unacceptable", "somewhat acceptable", "very acceptable")

sex_levels = c("female", "male", "third gender / other")
session_type_levels = c("yosemite", "yellowstone", "sequoia")
session_diffusor_sat_levels = c("15C", "17C", "19C")

col_subjects <- readr::cols(
  subject_id = readr::col_character(),
  gender = readr::col_factor(levels=sex_levels),
)

col_sessions <- readr::cols(
  session_id = readr::col_character(),
  session_type = readr::col_factor(levels = session_type_levels, ordered = TRUE),
  session_diffusor_sat = readr::col_factor(levels = session_diffusor_sat_levels, ordered = TRUE),
)

col_survey <- readr::cols(
  subject_id = readr::col_character(),
  workstation = readr::col_factor(levels = c("adaptation", "ws01", "ws02", "ws03")),
)

col_tsk <- readr::cols(
  subject_id = readr::col_character()
)


# Color palettes ----------------------------------------------------------

thermal_sensation_palette <- c("#6b8dd6", "#82c6ed","#b8e4f7","#c9e0b0","#f7a9b7","#eb6b58","#b44a4a")

thermal_preference_palette <- c("#82c6ed","#c9e0b0","#eb6b58")

acceptability_palette <- c('#bc3e4d','#d99fa8','#f3e8e7','#c1e0b9','#38a257')

session_type_palette <- c("#FFBE0B", "#FB5607", "#FF006E", "#8338EC")