# Title: Main Analysis
# Description: Main analysis of experimental data and development of the PPD with ankle draft model
# Author: Toby Kramer, Junmeng Lyu
# Date: 2026-01-05


# ---- Setup ----

source(here::here("src", "R", "x_setup.R"))
source(here::here("src", "R", "x_func.R"))
source(here::here("src", "R", "x_data.R"))
# source(here::here("src", "R", "x_stat.R"))




# 1. Demographics & Environmental Conditions ---------------------------------

# Sample size, demographics (age, sex distribution) -> Table

demographic_d <- subjects %>%
  dplyr::mutate(bmi = weight_kg / (height_m^2)) %>%
  group_by(sex) %>%
  summarise(
    n = n(),
    age = sprintf("%.1f ± %.1f", mean(age, na.rm = TRUE), sd(age, na.rm = TRUE)),
    height = sprintf("%.2f ± %.2f", mean(height_m, na.rm = TRUE), sd(height_m, na.rm = TRUE)),
    weight = sprintf("%.1f ± %.1f", mean(weight_kg, na.rm = TRUE), sd(weight_kg, na.rm = TRUE)),
    bmi = sprintf("%.1f ± %.1f", mean(bmi, na.rm = TRUE), sd(bmi, na.rm = TRUE))
  ) %>% # add top row with general information and SD values
  bind_rows(
    subjects %>%
      dplyr::mutate(bmi = weight_kg / (height_m^2)) %>%
      summarise(
        sex = "all",
        n = n(),
        age = sprintf("%.1f ± %.1f", mean(age, na.rm = TRUE), sd(age, na.rm = TRUE)),
        height = sprintf("%.2f ± %.2f", mean(height_m, na.rm = TRUE), sd(height_m, na.rm = TRUE)),
        weight = sprintf("%.1f ± %.1f", mean(weight_kg, na.rm = TRUE), sd(weight_kg, na.rm = TRUE)),
        bmi = sprintf("%.1f ± %.1f", mean(bmi, na.rm = TRUE), sd(bmi, na.rm = TRUE))
      ),
    .
  )


# Environmental conditions by SAT level and air speed condition
# (mean ± SD for air temp, SAT, air velocity at ankle, RH (?)) -> Table

# Join airflow and env measurements
env_airflow <- env_sessions %>%
  dplyr::left_join(airflow_sessions_all_mean, 
                   by = c("session_id", "session_diffusor_sat")) %>%
  dplyr::group_by(session_diffusor_sat) %>%
  dplyr::summarise(
    across(
      .cols = c(t_air_c, rh_percent,
                low_v_air_s, med_v_air_s, high_v_air_s,
                low_t_supply_c, med_t_supply_c, high_t_supply_c,
                low_turbulence_intensity, med_turbulence_intensity, high_turbulence_intensity),
      .fns = \(x) paste0(round(mean(x, na.rm = TRUE), 2), " ± ", round(sd(x, na.rm = TRUE), 2))
    )
  )

# ---------------------------

# Table, flipped
env_airflow_t <- env_airflow %>%
  tibble::column_to_rownames("session_diffusor_sat") %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("measurement")

# ---------------------------

# Table, stratified by SAT -> V_AIR
env_airflow_stratified <- env_sessions %>%
  dplyr::left_join(airflow_sessions_all_mean,
                   by = c("session_id", "session_diffusor_sat")) %>%
  dplyr::group_by(session_diffusor_sat) %>%
  dplyr::summarise(
    across(
      .cols = c(t_air_c, rh_percent,
                low_v_air_s, med_v_air_s, high_v_air_s,
                low_t_supply_c, med_t_supply_c, high_t_supply_c,
                low_turbulence_intensity, med_turbulence_intensity, high_turbulence_intensity),
      .fns = \(x) paste0(round(mean(x, na.rm = TRUE), 2), " ± ", round(sd(x, na.rm = TRUE), 2))
    )
  ) %>%
  # Pivot longer so each metric+level is a row
  tidyr::pivot_longer(-session_diffusor_sat, names_to = "col", values_to = "value") %>%
  # Split prefix (low/med/high) from variable name
  dplyr::mutate(
    level    = stringr::str_extract(col, "^(low|med|high)"),
    variable = stringr::str_remove(col, "^(low|med|high)_"),
    level    = ifelse(is.na(level), "none", level)
  ) %>%
  dplyr::select(-col) %>%
  # Pivot wider: columns are session_diffusor_sat x level combos
  tidyr::pivot_wider(
    names_from  = c(session_diffusor_sat, level),
    values_from = value
  )




# 2. Thermal Sensation / Comfort Responses -----------------------------------


# Distribution of whole-body and ankle thermal sensation by condition -> Boxplots
# Stat: Test effect of SAT and air speed (conditions) on whole-body / ankle thermal sensation (mixed model or repeated measures)





# 3. Air Movement Acceptability at Ankles ------------------------------------

# Distribution of ankle air movement acceptability by condition -> Stacked bar chart
# - Stat: Test effect of SAT and air speed (conditions) on acceptability (ordinal mixed model or Friedman)
# - Proportion reporting unacceptable air movement by condition






# 4. Dissatisfaction with Ankle Draft --- Model Comparison -------------------

# - Define binary outcome (matching Liu et al. definition) -> Observed dissatisfaction
#  rates by condition (table or figure) + comparison with Liu et al. predictions:
# - Apply Liu et al. model to observed conditions → predicted PPD
# - Compare predicted vs. observed dissatisfaction rates
# - Figure: Predicted vs. observed, or overlay on Liu et al. PPD contour plot





# 5. Updated Model inc. Winter Conditions ------------------------------------






