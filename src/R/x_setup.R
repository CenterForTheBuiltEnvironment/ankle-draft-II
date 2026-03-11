# ==============================================================================
# Title: Setup
# Description: Global settings, factor levels, column types, and color palettes
# Author: Toby Kramer, Junmeng Lyu
# Date: 2026-01-05
# ==============================================================================


# Packages =====================================================================

pacman::p_load(
  here, dplyr, ggplot2, readr, tidyr, purrr, stringr, lubridate, arrow,
  ggeffects, janitor, scales, glue, yaml, data.table, rstatix, patchwork,
  pwr, broom, lme4, ggpubr, openxlsx, gt
)


# Global Settings ==============================================================

single_col_width <- 89   # mm
double_col_width <- 183  # mm


# Factor Levels ================================================================

# Survey response scales -------------------------------------------------------

thermal_sensation_levels <- c(
  "cold", "cool", "slightly cool", "neutral",
  "slightly warm", "warm", "hot"
)

thermal_preference_levels <- c("cooler", "no change", "warmer")

air_speed_preference_levels <- c("lower", "no change", "higher")

acceptability_levels <- c(
 "very unacceptable", "somewhat unacceptable",
  "neither acceptable nor unacceptable",
  "somewhat acceptable", "very acceptable"
)

# Demographic and experimental factors -----------------------------------------

sex_levels <- c("female", "male", "third gender / other")

session_type_levels <- c("yosemite", "yellowstone", "sequoia")

session_sat_levels <- c("15C", "17C", "19C")
session_sat_labels <- c("15C", "17C", "19C")

workstation_levels <- c("adaptation", "ws02", "ws03", "ws01")
workstation_labels <- c("adaptation", "low", "medium", "high")


# Column Type Specifications ===================================================

col_subjects <- readr::cols(
  subject_id = readr::col_character(),
  gender = readr::col_factor(levels = sex_levels)
)

col_sessions <- readr::cols(
  session_id = readr::col_character(),
  session_type = readr::col_factor(levels = session_type_levels, ordered = TRUE),
  session_diffusor_sat = readr::col_factor(levels = session_sat_levels, ordered = TRUE)
)

col_survey <- readr::cols(
  subject_id = readr::col_character(),
  workstation = readr::col_factor(levels = workstation_levels, ordered = FALSE)
)

col_tsk <- readr::cols(
  subject_id = readr::col_character()
)


# Color Palettes ===============================================================

thermal_sensation_palette <- c(
  "#6b8dd6", "#82c6ed", "#b8e4f7", "#c9e0b0",
  "#f7a9b7", "#eb6b58", "#b44a4a"
)

thermal_preference_palette <- c("#82c6ed", "#c9e0b0", "#eb6b58")

air_movement_preference_palette <- c("#6b8dd6", "#82c6ed", "#b8e4f7")

acceptability_palette <- c(
  "#bc3e4d", "#d99fa8", "#f3e8e7", "#c1e0b9", "#38a257"
)

session_type_palette <- c("#FFBE0B", "#FB5607", "#FF006E", "#8338EC")

workstation_palette <- c(
  "low" = "#c9e0b0",
  "medium" = "#b8e4f7",
  "high" = "#6b8dd6"
)
