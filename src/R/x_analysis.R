# ==============================================================================
# Title: Main Analysis
# Description: Analysis of experimental data and figure generation
# Author: Toby Kramer, Junmeng Lyu
# Date: 2026-01-05
# ==============================================================================

source(here::here("src", "R", "x_setup.R"))
source(here::here("src", "R", "x_func.R"))
source(here::here("src", "R", "x_data.R"))
# source(here::here("src", "R", "x_stat.R"))


# 1. Demographics & Environmental Conditions ==================================

# Subject demographics table ---------------------------------------------------

demographic_d <- subjects %>%
  dplyr::mutate(bmi = weight_kg / (height_m^2)) %>%
  group_by(sex) %>%
  summarise(
    n = n(),
    age = sprintf("%.1f +/- %.1f", mean(age, na.rm = TRUE), sd(age, na.rm = TRUE)),
    height = sprintf("%.2f +/- %.2f", mean(height_m, na.rm = TRUE), sd(height_m, na.rm = TRUE)),
    weight = sprintf("%.1f +/- %.1f", mean(weight_kg, na.rm = TRUE), sd(weight_kg, na.rm = TRUE)),
    bmi = sprintf("%.1f +/- %.1f", mean(bmi, na.rm = TRUE), sd(bmi, na.rm = TRUE))
  ) %>%
  bind_rows(
    subjects %>%
      dplyr::mutate(bmi = weight_kg / (height_m^2)) %>%
      summarise(
        sex = "all",
        n = n(),
        age = sprintf("%.1f +/- %.1f", mean(age, na.rm = TRUE), sd(age, na.rm = TRUE)),
        height = sprintf("%.2f +/- %.2f", mean(height_m, na.rm = TRUE), sd(height_m, na.rm = TRUE)),
        weight = sprintf("%.1f +/- %.1f", mean(weight_kg, na.rm = TRUE), sd(weight_kg, na.rm = TRUE)),
        bmi = sprintf("%.1f +/- %.1f", mean(bmi, na.rm = TRUE), sd(bmi, na.rm = TRUE))
      ),
    .
  )


# Environmental conditions table -----------------------------------------------

env_airflow <- env_sessions %>%
  dplyr::left_join(
    airflow_sessions_all_mean,
    by = c("session_id", "session_sat")
  ) %>%
  dplyr::group_by(session_sat) %>%
  dplyr::summarise(
    across(
      .cols = c(
        t_air_c, rh_percent,
        low_v_air_s, med_v_air_s, high_v_air_s,
        low_t_supply_c, med_t_supply_c, high_t_supply_c,
        low_turbulence_intensity, med_turbulence_intensity, high_turbulence_intensity
      ),
      .fns = \(x) paste0(
        round(mean(x, na.rm = TRUE), 2), " +/- ",
        round(sd(x, na.rm = TRUE), 2)
      )
    )
  )

# Transposed table
env_airflow_t <- env_airflow %>%
  tibble::column_to_rownames("session_sat") %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("measurement")

# Stratified table (SAT x air speed)
env_airflow_stratified <- env_sessions %>%
  dplyr::left_join(
    airflow_sessions_all_mean,
    by = c("session_id", "session_sat")
  ) %>%
  dplyr::group_by(session_sat) %>%
  dplyr::summarise(
    across(
      .cols = c(
        t_air_c, rh_percent,
        low_v_air_s, med_v_air_s, high_v_air_s,
        low_t_supply_c, med_t_supply_c, high_t_supply_c,
        low_turbulence_intensity, med_turbulence_intensity, high_turbulence_intensity
      ),
      .fns = \(x) paste0(
        round(mean(x, na.rm = TRUE), 2), " +/- ",
        round(sd(x, na.rm = TRUE), 2)
      )
    )
  ) %>%
  tidyr::pivot_longer(-session_sat, names_to = "col", values_to = "value") %>%
  dplyr::mutate(
    level = stringr::str_extract(col, "^(low|med|high)"),
    variable = stringr::str_remove(col, "^(low|med|high)_"),
    level = ifelse(is.na(level), "none", level)
  ) %>%
  dplyr::select(-col) %>%
  tidyr::pivot_wider(
    names_from = c(session_sat, level),
    values_from = value
  )


# 2. Thermal Sensation / Comfort / Preference ==================================

# Thermal sensation (overall vs ankles) ----------------------------------------

thermal_sensation_p <- survey %>%
  dplyr::filter(
    question %in% c("thermal_sensation", "thermal_sensation_ankles"),
    is_open_text == FALSE
  ) %>%
  dplyr::filter(workstation != "adaptation") %>%
  dplyr::mutate(
    response_value = as.numeric(response_value),
    question = factor(
      dplyr::recode(
        question,
        "thermal_sensation" = "Overall",
        "thermal_sensation_ankles" = "Ankles"
      ),
      levels = c("Overall", "Ankles")
    )
  ) %>%
  ggplot(aes(
    x = workstation,
    y = response_value,
    group = interaction(workstation, question)
  )) +
  geom_boxplot(
    aes(fill = question),
    color = "grey70",
    outlier.shape = NA,
    alpha = 0.5,
    position = position_dodge(width = 0.8)
  ) +
  geom_jitter(
    aes(color = response_value, group = question),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    alpha = 0.7,
    size = 1.5
  ) +
  scale_fill_manual(values = c("Overall" = "grey70", "Ankles" = "grey90")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.3) +
  facet_wrap(~ session_sat) +
  scale_color_gradientn(
    colors = thermal_sensation_palette,
    limits = c(-3, 3)
  ) +
  scale_y_continuous(
    breaks = c(-3, 0, 3),
    labels = c("Cold", "Neutral", "Hot"),
    limits = c(-3, 3)
  ) +
  labs(x = "Air speed (m/s)", y = "Thermal sensation") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(10, "mm"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.y = element_line(color = "grey", linewidth = 0.25),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )


# Thermal preference (overall) -------------------------------------------------

thermal_preference_overall_p <- survey %>%
  dplyr::filter(
    question %in% c("thermal_preference"),
    is_open_text == FALSE
  ) %>%
  dplyr::filter(workstation != "adaptation") %>%
  dplyr::mutate(
    response_value = as.numeric(response_value),
    response_value = factor(
      dplyr::case_when(
        response_value < 0.0 ~ "Cooler",
        response_value > 0.0 ~ "Warmer",
        response_value == 0.0 ~ "No change"
      ),
      levels = c("Cooler", "No change", "Warmer")
    )
  ) %>%
  plot_stacked_pct_ws(response_value, thermal_preference_palette) +
  labs(subtitle = "Thermal preference  |  Overall", x = NULL)


# Thermal preference (ankles) --------------------------------------------------

thermal_preference_ankles_p <- survey %>%
  dplyr::filter(
    question %in% c("thermal_preference_ankles"),
    is_open_text == FALSE
  ) %>%
  dplyr::filter(workstation != "adaptation") %>%
  dplyr::mutate(
    response_value = as.numeric(response_value),
    response_value = factor(
      dplyr::case_when(
        response_value < 0.0 ~ "Cooler",
        response_value > 0.0 ~ "Warmer",
        response_value == 0.0 ~ "No change"
      ),
      levels = c("Cooler", "No change", "Warmer")
    )
  ) %>%
  plot_stacked_pct_ws(response_value, thermal_preference_palette) +
  labs(subtitle = "Thermal preference  |  Ankles", x = "Air speed (m/s)")


# Combined thermal preference figure -------------------------------------------

thermal_preference_p <- (thermal_preference_overall_p / thermal_preference_ankles_p) +
  plot_layout(axes = "collect_x") +
  plot_annotation(tag_levels = "a", tag_suffix = ".") &
  theme(
    plot.subtitle = element_text(hjust = 0.05, margin = margin(b = 3, unit = "mm")),
    plot.tag = element_text(size = 7, face = "bold"),
    plot.margin = margin(b = 5, unit = "mm"),
    axis.title = element_text(margin = margin(r = 2, unit = "mm")),
    legend.margin = margin(l = 3, unit = "mm"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  here::here("manuscript", "figs", "thermal_preference.png"),
  plot = thermal_preference_p,
  dpi = 500,
  width = single_col_width,
  height = 120,
  units = "mm",
  bg = "transparent"
)


# Thermal acceptability --------------------------------------------------------

vertical_gap <- 0.2

thermal_acceptability_p <- survey %>%
  dplyr::filter(
    question %in% c("thermal_acceptability"),
    is_open_text == FALSE
  ) %>%
  dplyr::filter(workstation != "adaptation") %>%
  dplyr::mutate(
    response_value = as.numeric(response_value),
    response_plot = ifelse(
      response_value > 0,
      response_value + vertical_gap,
      response_value
    )
  ) %>%
  ggplot(aes(x = workstation, y = response_plot)) +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = 0.1 + vertical_gap, ymax = 3 + vertical_gap,
    fill = "#c1e0b9", alpha = 0.2
  ) +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = -3, ymax = -0.1,
    fill = "#d99fa8", alpha = 0.2
  ) +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = -0.1, ymax = 0.1 + vertical_gap,
    fill = "white"
  ) +
  geom_hline(
    yintercept = 0.1 + vertical_gap,
    linetype = "dashed", color = "grey50", linewidth = 0.3
  ) +
  geom_hline(
    yintercept = -0.1,
    linetype = "dashed", color = "grey50", linewidth = 0.3
  ) +
  geom_boxplot(color = "grey50", outlier.shape = NA, fill = "grey95") +
  geom_jitter(aes(color = response_value), width = 0.1, alpha = 0.5, size = 1.5) +
  facet_wrap(~ session_sat) +
  scale_color_gradientn(
    colors = c("#bc3e4d", "#d99fa8", "#d99fa8", "#c1e0b9", "#38a257"),
    values = scales::rescale(c(-3, -0.1, 0, 0.1, 3)),
    limits = c(-3, 3)
  ) +
  scale_y_continuous(
    breaks = c(
      -3, -2, -1, -0.1,
      0.1 + vertical_gap, 1 + vertical_gap, 2 + vertical_gap, 3 + vertical_gap
    ),
    labels = c(
      "Clearly unacceptable", "", "", "Just unacceptable",
      "Just acceptable", "", "", "Clearly acceptable"
    )
  ) +
  labs(x = "Air speed (m/s)", y = "Thermal acceptability", color = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(10, "mm"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )


# Combined thermal perception figure -------------------------------------------

thermal_perception_p <- (thermal_sensation_p / thermal_acceptability_p) +
  plot_layout(axes = "collect_x") +
  plot_annotation(tag_levels = "a", tag_suffix = ".") &
  theme(
    plot.subtitle = element_text(hjust = 0.05, margin = margin(b = 3, unit = "mm")),
    plot.tag = element_text(size = 7, face = "bold"),
    plot.margin = margin(b = 5, unit = "mm"),
    axis.title = element_text(margin = margin(r = 2, unit = "mm")),
    legend.margin = margin(l = 3, unit = "mm"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  here::here("manuscript", "figs", "thermal_perception.png"),
  plot = thermal_perception_p,
  dpi = 500,
  width = double_col_width,
  height = 180,
  units = "mm",
  bg = "transparent"
)


# 3. Air Movement Acceptability at Ankles ======================================

# Air movement acceptability ---------------------------------------------------

air_movement_acceptability_p <- survey %>%
  dplyr::filter(
    question %in% c("air_movement_acceptability_ankles"),
    is_open_text == FALSE
  ) %>%
  dplyr::filter(workstation != "adaptation") %>%
  dplyr::mutate(
    response_value = as.numeric(response_value),
    acceptability = factor(
      dplyr::case_when(
        response_value > 0 ~ "Acceptable",
        response_value < 0 ~ "Unacceptable",
        response_value == 0 ~ NA
      ),
      levels = c("Unacceptable", "Acceptable")
    )
  ) %>%
  plot_stacked_pct_ws(
    acceptability,
    palette = c("Unacceptable" = "#d99fa8", "Acceptable" = "#c1e0b9")
  ) +
  labs(subtitle = "Ankle air movement acceptability", x = NULL)


# Air movement preference ------------------------------------------------------

air_movement_preference_p <- survey %>%
  dplyr::filter(
    question %in% c("air_movement_preference_ankles"),
    is_open_text == FALSE
  ) %>%
  dplyr::filter(workstation != "adaptation") %>%
  dplyr::mutate(
    response_value = as.numeric(response_value),
    acceptability = factor(
      dplyr::case_when(
        response_value > 0 ~ "More",
        response_value < 0 ~ "Less",
        response_value == 0 ~ "No change"
      ),
      levels = c("More", "No change", "Less")
    )
  ) %>%
  plot_stacked_pct_ws(acceptability, palette = air_movement_preference_palette) +
  labs(subtitle = "Ankle air movement preference", x = "Air speed (m/s)")


# Combined air movement figure -------------------------------------------------

air_movement_p <- (air_movement_acceptability_p / air_movement_preference_p) +
  plot_layout(axes = "collect_x") +
  plot_annotation(tag_levels = "a", tag_suffix = ".") &
  theme(
    plot.subtitle = element_text(hjust = 0.05, margin = margin(b = 3, unit = "mm")),
    plot.tag = element_text(size = 7, face = "bold"),
    plot.margin = margin(b = 5, unit = "mm"),
    axis.title = element_text(margin = margin(r = 2, unit = "mm")),
    legend.position = "right",
    legend.direction = "vertical",
    legend.justification = "left",
    legend.margin = margin(l = 3, unit = "mm"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  here::here("manuscript", "figs", "air_movement_acc_pref.png"),
  plot = air_movement_p,
  dpi = 500,
  width = single_col_width,
  height = 120,
  units = "mm",
  bg = "transparent"
)


# 4. Dissatisfaction with Ankle Draft - Model Comparison =======================
#
# TODO: Integrate analysis from analysis_old.R
#
# - Define binary outcome (matching Liu et al. definition)
# - Observed dissatisfaction rates by condition (table or figure)
# - Compare predicted vs. observed dissatisfaction rates
# - Apply Liu et al. model to observed conditions -> predicted PPD
# - Figure: Predicted vs. observed, or overlay on Liu et al. PPD contour plot


# 5. Updated Model inc. Winter Conditions ======================================
#
# TODO: Integrate analysis from analysis_old.R
#
# - Develop new or adjusted model using combined dataset (Liu + new)
# - Interesting variables might be: air speed, SAT, room air temperature,
# thermal perception variables, clothing levels, vertical temperature gradient
# - Important to keep it simple (so it remains usable by non-experts)
# - might be worth to start with a short Exploratory Data Analysis before
# proposing new model
