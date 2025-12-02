
pacman::p_load(
  here, dplyr, ggplot2, readr, tidyr, purrr, stringr, lubridate, arrow,
  janitor, scales, glue, yaml, data.table, rstatix, patchwork, pwr
)


# Global settings ---------------------------------------------------------

single_col_width = 89 #mm
double_col_width = 183 #mm



# Color palettes ----------------------------------------------------------

thermal_sensation_palette <- c("#6b8dd6", "#82c6ed","#b8e4f7","#c9e0b0","#f7a9b7","#eb6b58","#b44a4a")

thermal_preference_palette <- c("#82c6ed","#c9e0b0","#eb6b58")

acceptability_palette <- c('#bc3e4d','#d99fa8','#f3e8e7','#c1e0b9','#38a257')

session_type_palette <- c("#FFBE0B", "#FB5607", "#FF006E", "#8338EC")