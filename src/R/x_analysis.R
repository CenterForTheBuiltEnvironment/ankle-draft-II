# ==============================================================================
# Title: Main Analysis
# Description: Analysis of experimental data and figure generation
# Author: Toby Kramer, Junmeng Lyu
# Date: 2026-01-05
# ==============================================================================

source(here::here("src", "R", "x_setup.R"))
source(here::here("src", "R", "x_func.R"))
source(here::here("src", "R", "x_data.R"))
source(here::here("src", "R", "x_stat.R"))

# ==============================================================================
# 0. Round Counts (per subject)
# ==============================================================================
#
# Definition (per latest processing rule):
# - Workstation round = one `clothing_change` response (workstation != adaptation)
# - Adaptation round  = one `thermal_comfort` response during `adaptation`
#
# We count unique timestamps to avoid double-counting duplicates.

rounds_by_subject_session <- dplyr::full_join(
  survey %>%
    dplyr::filter(question == "clothing_change", workstation != "adaptation") %>%
    dplyr::group_by(subject_id, session_id) %>%
    dplyr::summarise(
      n_workstation_rounds = dplyr::n_distinct(timestamp),
      .groups = "drop"
    ),
  survey %>%
    dplyr::filter(question == "thermal_comfort", workstation == "adaptation") %>%
    dplyr::group_by(subject_id, session_id) %>%
    dplyr::summarise(
      n_adaptation_rounds = dplyr::n_distinct(timestamp),
      .groups = "drop"
    ),
  by = c("subject_id", "session_id")
) %>%
  dplyr::mutate(
    n_workstation_rounds = tidyr::replace_na(n_workstation_rounds, 0L),
    n_adaptation_rounds = tidyr::replace_na(n_adaptation_rounds, 0L),
    n_rounds_total = n_workstation_rounds + n_adaptation_rounds
  ) %>%
  dplyr::arrange(subject_id, session_id)

rounds_by_subject <- rounds_by_subject_session %>%
  dplyr::group_by(subject_id) %>%
  dplyr::summarise(
    n_sessions = dplyr::n_distinct(session_id),
    n_workstation_rounds = sum(n_workstation_rounds, na.rm = TRUE),
    n_adaptation_rounds = sum(n_adaptation_rounds, na.rm = TRUE),
    n_rounds_total = sum(n_rounds_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(n_rounds_total), subject_id)

write_csv(
  rounds_by_subject_session,
  here::here("manuscript", "tables", "rounds_by_subject_session.csv")
)

write_csv(
  rounds_by_subject,
  here::here("manuscript", "tables", "rounds_by_subject.csv")
)


# ==============================================================================
# 1. Demographics & Environmental Conditions
# ==============================================================================

# 1.1 Subject Demographics -----------------------------------------------------

demographic_d <- subjects %>%
  dplyr::mutate(bmi = weight_kg / (height_m^2)) %>%
  group_by(sex) %>%
  summarise(
    n = n(),
    age = sprintf("%.1f (+/- %.1f)", mean(age, na.rm = TRUE), sd(age, na.rm = TRUE)),
    height = sprintf("%.2f (+/- %.2f)", mean(height_m, na.rm = TRUE), sd(height_m, na.rm = TRUE)),
    weight = sprintf("%.1f (+/- %.1f)", mean(weight_kg, na.rm = TRUE), sd(weight_kg, na.rm = TRUE)),
    bmi = sprintf("%.1f (+/- %.1f)", mean(bmi, na.rm = TRUE), sd(bmi, na.rm = TRUE))
  ) %>%
  bind_rows(
    subjects %>%
      dplyr::mutate(bmi = weight_kg / (height_m^2)) %>%
      summarise(
        sex = "all",
        n = n(),
        age = sprintf("%.1f (+/- %.1f)", mean(age, na.rm = TRUE), sd(age, na.rm = TRUE)),
        height = sprintf("%.2f (+/- %.2f)", mean(height_m, na.rm = TRUE), sd(height_m, na.rm = TRUE)),
        weight = sprintf("%.1f (+/- %.1f)", mean(weight_kg, na.rm = TRUE), sd(weight_kg, na.rm = TRUE)),
        bmi = sprintf("%.1f (+/- %.1f)", mean(bmi, na.rm = TRUE), sd(bmi, na.rm = TRUE))
      ),
    .
  ) %>%
  mutate(sex = stringr::str_to_title(sex))  # capitalize for table rendering

# 1.2 Environmental Conditions -------------------------------------------------

env_airflow <- env_sessions %>%
  dplyr::left_join(
    airflow_sessions_all_mean,
    by = c("session_id", "session_sat")
  ) %>%
  dplyr::mutate(
    t_supply_c = rowMeans(cbind(low_t_supply_c, med_t_supply_c, high_t_supply_c), na.rm = TRUE)
  ) %>%
  dplyr::group_by(session_sat) %>%
  dplyr::summarise(
    # Two decimal places
    across(
      .cols = c(
        low_v_air_s, med_v_air_s, high_v_air_s,
        low_turbulence_intensity, med_turbulence_intensity, high_turbulence_intensity
      ),
      .fns = \(x) paste0(round(mean(x, na.rm = TRUE), 2), " ( +/- ", round(sd(x, na.rm = TRUE), 2), ")")
    ),
    # One decimal place
    across(
      .cols = c(t_air_c, rh_percent, t_supply_c),
      .fns = \(x) paste0(round(mean(x, na.rm = TRUE), 1), " ( +/- ", round(sd(x, na.rm = TRUE), 1), ")")
    )
  ) %>%
  dplyr::select(t_air_c, rh_percent, t_supply_c, everything())

# 1.2.1 Transposed table (for manuscript) ----

label_map <- c(
  t_air_c                    = "Air temperature, head level (°C)",
  rh_percent                 = "Relative humidity (%)",
  t_supply_c                 = "Air temperature, ankle level (°C)",
  low_v_air_s                = "Air speed, low (m/s)",
  med_v_air_s                = "Air speed, medium (m/s)",
  high_v_air_s               = "Air speed, high (m/s)",
  low_turbulence_intensity   = "Turbulence, low (%)",
  med_turbulence_intensity   = "Turbulence, medium (%)",
  high_turbulence_intensity  = "Turbulence, high (%)"
)

env_airflow_t <- env_airflow %>%
  tibble::column_to_rownames("session_sat") %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("measurement") %>%
  dplyr::mutate(measurement = dplyr::recode(measurement, !!!label_map))

# 1.2.2 Stratified table (SAT x air speed) ----

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





# ==============================================================================
# 2. Overall Thermal Perception (Whole Body)
# ==============================================================================

# 2.1 Data Preparation ---------------------------------------------------------

# 2.1.1 Thermal sensation ----

thermal_sensation_d <- analysis %>%
  dplyr::filter(
    question %in% c("thermal_sensation", "thermal_sensation_ankles"),
    is_open_text == FALSE
  ) %>%
  dplyr::filter(workstation != "adaptation") %>%
  dplyr::mutate(
    response_value_num = as.numeric(response_value),
    response_value = factor(
      dplyr::case_when(
        response_value_num < -2.5  ~ "Cold",
        response_value_num < -1.5  ~ "Cool",
        response_value_num < -0.5  ~ "Slightly cool",
        response_value_num <= 0.5  ~ "Neutral",
        response_value_num <= 1.5  ~ "Slightly warm",
        response_value_num <= 2.5  ~ "Warm",
        response_value_num > 2.5   ~ "Hot"
      ),
      levels = thermal_sensation_levels
    ),
    question = factor(
      dplyr::recode(
        question,
        "thermal_sensation"        = "Overall",
        "thermal_sensation_ankles" = "Ankles"
      ),
      levels = c("Overall", "Ankles")
    )
  ) %>%
  tidyr::drop_na(response_value)

thermal_sensation_overall_summary <- thermal_sensation_d %>%
  dplyr::filter(question == "Overall") %>%
  dplyr::count(session_sat, workstation, response_value, name = "n") %>%
  dplyr::group_by(session_sat, workstation) %>%
  dplyr::mutate(
    pct       = round(n / sum(n), 2),
    pct_label = scales::percent(pct, accuracy = 1)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(session_sat, workstation, response_value, n, pct, pct_label)

thermal_sensation_ankle_summary <- thermal_sensation_d %>%
  dplyr::filter(question == "Ankles") %>%
  dplyr::count(session_sat, workstation, response_value, name = "n") %>%
  dplyr::group_by(session_sat, workstation) %>%
  dplyr::mutate(
    pct       = round(n / sum(n), 2),
    pct_label = scales::percent(pct, accuracy = 1)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(session_sat, workstation, response_value, n, pct, pct_label)

# 2.1.2 Thermal preference ----

thermal_preference_d <- analysis %>%
  dplyr::filter(
    question %in% c("thermal_preference", "thermal_preference_ankles"),
    is_open_text == FALSE
  ) %>%
  dplyr::filter(workstation != "adaptation") %>%
  dplyr::mutate(
    response_value_num = as.numeric(response_value),
    response_value = factor(
      dplyr::case_when(
        response_value_num > 0  ~ "Warmer",
        response_value_num < 0  ~ "Cooler",
        response_value_num == 0 ~ "No change"
      ),
      levels = thermal_preference_levels
    ),
    question = factor(
      dplyr::recode(
        question,
        "thermal_preference"        = "Overall",
        "thermal_preference_ankles" = "Ankles"
      ),
      levels = c("Overall", "Ankles")
    )
  ) %>%
  tidyr::drop_na(response_value)

thermal_preference_overall_summary <- thermal_preference_d %>%
  dplyr::filter(question == "Overall") %>%
  dplyr::count(session_sat, workstation, response_value, name = "n") %>%
  dplyr::group_by(session_sat, workstation) %>%
  dplyr::mutate(
    pct       = round(n / sum(n), 2),
    pct_label = scales::percent(pct, accuracy = 1)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(session_sat, workstation, response_value, n, pct, pct_label)

thermal_preference_ankle_summary <- thermal_preference_d %>%
  dplyr::filter(question == "Ankles") %>%
  dplyr::count(session_sat, workstation, response_value, name = "n") %>%
  dplyr::group_by(session_sat, workstation) %>%
  dplyr::mutate(
    pct       = round(n / sum(n), 2),
    pct_label = scales::percent(pct, accuracy = 1)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(session_sat, workstation, response_value, n, pct, pct_label)

# 2.1.3 Thermal acceptability ----

vertical_gap <- 0.2  # visual gap between acceptable/unacceptable regions

thermal_acceptability_d <- analysis %>%
  dplyr::filter(
    question == "thermal_acceptability",
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
  )

# 2.2 Thermal Sensation (Overall) ----------------------------------------------

thermal_sensation_overall_d_raw <- dplyr::filter(thermal_sensation_d, question == "Overall")

thermal_sensation_overall_stats <- paired_wilcox_test(
  data         = thermal_sensation_overall_d_raw,
  group_by_var = "session_sat",
  subject_var  = "subject_id",
  question_var = "workstation",
  value_var    = "response_value_num",
  order_var    = "timestamp"
)

thermal_sensation_overall_d <- stacked_pct_data_ws(
  thermal_sensation_overall_d_raw, response_value
)

thermal_sensation_overall_p <- thermal_sensation_overall_d_raw %>%
  plot_stacked_pct_ws(response_value, thermal_sensation_palette) +
  labs(subtitle = "Thermal sensation, whole body", x = NULL)

# 2.3 Thermal Preference (Overall) ---------------------------------------------

thermal_preference_overall_d_raw <- dplyr::filter(thermal_preference_d, question == "Overall")

thermal_preference_overall_stats <- paired_wilcox_test(
  data         = thermal_preference_overall_d_raw,
  group_by_var = "session_sat",
  subject_var  = "subject_id",
  question_var = "workstation",
  value_var    = "response_value_num",
  order_var    = "timestamp"
)

thermal_preference_overall_d <- stacked_pct_data_ws(
  thermal_preference_overall_d_raw, response_value
)

thermal_preference_overall_p <- thermal_preference_overall_d_raw %>%
  plot_stacked_pct_ws(response_value, thermal_preference_palette) +
  labs(subtitle = "Thermal preference, whole body", x = NULL)

# 2.4 Combined Sensation/Preference Figure -------------------------------------

thermal_perception_overall_p <- (thermal_sensation_overall_p / thermal_preference_overall_p) +
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
  here::here("manuscript", "figs", "thermal_perception_overall.png"),
  plot = thermal_perception_overall_p,
  dpi = 500,
  width = single_col_width,
  height = 120,
  units = "mm",
  bg = "transparent"
)

# 2.5 Thermal Acceptability ----------------------------------------------------

thermal_acceptability_stats <- paired_t_test(
  data         = thermal_acceptability_d,
  group_by_var = "session_sat",
  subject_var  = "subject_id",
  question_var = "workstation",
  value_var    = "response_value",
  order_var    = "timestamp"
)

thermal_acceptability_p <- thermal_acceptability_d %>%
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

ggsave(
  here::here("manuscript", "figs", "thermal_acceptability_overall.png"),
  plot = thermal_acceptability_p,
  dpi = 500,
  width = double_col_width,
  height = 80,
  units = "mm",
  bg = "transparent"
)


# ==============================================================================
# 3. Ankle-Level Thermal Perception
# ==============================================================================

# 3.1 Thermal Sensation (Ankles) -----------------------------------------------

thermal_sensation_ankles_d_raw <- dplyr::filter(thermal_sensation_d, question == "Ankles")

thermal_sensation_ankles_stats <- paired_wilcox_test(
  data         = thermal_sensation_ankles_d_raw,
  group_by_var = "session_sat",
  subject_var  = "subject_id",
  question_var = "workstation",
  value_var    = "response_value_num",
  order_var    = "timestamp"
)

thermal_sensation_ankles_d <- stacked_pct_data_ws(
  thermal_sensation_ankles_d_raw, response_value
)

thermal_sensation_ankles_p <- thermal_sensation_ankles_d_raw %>%
  plot_stacked_pct_ws(response_value, thermal_sensation_palette) +
  labs(subtitle = "Thermal sensation, ankles", x = "Air speed (m/s)")

# 3.2 Thermal Preference (Ankles) ----------------------------------------------

thermal_preference_ankles_d_raw <- dplyr::filter(thermal_preference_d, question == "Ankles")

thermal_preference_ankles_stats <- paired_wilcox_test(
  data         = thermal_preference_ankles_d_raw,
  group_by_var = "session_sat",
  subject_var  = "subject_id",
  question_var = "workstation",
  value_var    = "response_value_num",
  order_var    = "timestamp"
)

thermal_preference_ankles_d <- stacked_pct_data_ws(
  thermal_preference_ankles_d_raw, response_value
)

thermal_preference_ankles_p <- thermal_preference_ankles_d_raw %>%
  plot_stacked_pct_ws(response_value, thermal_preference_palette) +
  labs(subtitle = "Thermal preference, ankles", x = "Air speed (m/s)")

# 3.3 Combined Ankle Sensation/Preference Figure ------------------------------

thermal_perception_ankle_p <- (thermal_sensation_ankles_p / thermal_preference_ankles_p) +
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
  here::here("manuscript", "figs", "thermal_perception_ankle.png"),
  plot = thermal_perception_ankle_p,
  dpi = 500,
  width = single_col_width,
  height = 120,
  units = "mm",
  bg = "transparent"
)

# 3.4 Air Movement Acceptability (Ankles) --------------------------------------

air_movement_acceptability_d <- analysis %>%
  dplyr::filter(
    question == "air_movement_acceptability_ankles",
    is_open_text == FALSE
  ) %>%
  dplyr::filter(workstation != "adaptation") %>%
  dplyr::mutate(
    response_value_num = as.numeric(response_value),
    acceptability = factor(
      dplyr::case_when(
        response_value_num > 0  ~ "Acceptable",
        response_value_num < 0  ~ "Unacceptable",
        response_value_num == 0 ~ NA
      ),
      levels = c("Acceptable", "Unacceptable"),
      ordered = TRUE
    )
  )

air_movement_acceptability_summary <- air_movement_acceptability_d %>%
  dplyr::count(session_sat, workstation, acceptability, name = "n") %>%
  dplyr::group_by(session_sat, workstation) %>%
  dplyr::mutate(
    pct       = round(n / sum(n), 2),
    pct_label = scales::percent(pct, accuracy = 1)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(session_sat, workstation, acceptability, n, pct, pct_label)

air_movement_acceptability_stats <- paired_wilcox_test(
  data = dplyr::mutate(
    air_movement_acceptability_d,
    acceptability_num = dplyr::case_when(
      acceptability == "Unacceptable" ~ 0,
      acceptability == "Acceptable"   ~ 1,
      TRUE                            ~ NA_real_
    )
  ),
  group_by_var = "session_sat",
  subject_var  = "subject_id",
  question_var = "workstation",
  value_var    = "acceptability_num",
  order_var    = "timestamp"
)

air_movement_acceptability_p <- air_movement_acceptability_d %>%
  plot_stacked_pct_ws(
    acceptability,
    palette = c("Unacceptable" = "#d99fa8", "Acceptable" = "#c1e0b9")
  ) +
  labs(subtitle = "Air movement acceptability, ankles", x = NULL)

# 3.5 Air Movement Preference (Ankles) -----------------------------------------

air_movement_preference_d <- analysis %>%
  dplyr::filter(
    question == "air_movement_preference_ankles",
    is_open_text == FALSE
  ) %>%
  dplyr::filter(workstation != "adaptation") %>%
  dplyr::mutate(
    response_value_num = as.numeric(response_value),
    acceptability = factor(
      dplyr::case_when(
        response_value_num > 0  ~ "More",
        response_value_num < 0  ~ "Less",
        response_value_num == 0 ~ "No change"
      ),
      levels = c("Less", "No change", "More")
    )
  )

air_movement_preference_summary <- air_movement_preference_d %>%
  dplyr::count(session_sat, workstation, acceptability, name = "n") %>%
  dplyr::group_by(session_sat, workstation) %>%
  dplyr::mutate(
    pct       = round(n / sum(n), 2),
    pct_label = scales::percent(pct, accuracy = 1)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(session_sat, workstation, acceptability, n, pct, pct_label)

air_movement_preference_stats <- paired_wilcox_test(
  data         = air_movement_preference_d,
  group_by_var = "session_sat",
  subject_var  = "subject_id",
  question_var = "workstation",
  value_var    = "response_value_num",
  order_var    = "timestamp"
)

air_movement_preference_p <- air_movement_preference_d %>%
  plot_stacked_pct_ws(acceptability, palette = air_movement_preference_palette) +
  labs(subtitle = "Air movement preference, ankles", x = "Air speed (m/s)")

# 3.6 Combined Air Movement Figure ---------------------------------------------

air_movement_p <- (air_movement_acceptability_p / air_movement_preference_p) +
  plot_layout(axes = "collect_x") +
  plot_annotation(tag_levels = "a", tag_suffix = ".") &
  theme(
    plot.subtitle    = element_text(hjust = 0.05, margin = margin(b = 3, unit = "mm")),
    plot.tag         = element_text(size = 7, face = "bold"),
    plot.margin      = margin(b = 5, unit = "mm"),
    axis.title       = element_text(margin = margin(r = 2, unit = "mm")),
    legend.position  = "right",
    legend.direction = "vertical",
    legend.justification = "left",
    legend.margin    = margin(l = 3, unit = "mm"),
    axis.text.x      = element_text(angle = 45, hjust = 1)
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
# - Define binary outcome (matching Liu et al. definition) ----------------

dissatisfied_with_draft_ankles <- analysis %>%
  dplyr::filter(
    question %in% c("thermal_sensation", "thermal_sensation_ankles", "air_movement_acceptability_ankles", "dissatisfied_with_draft_ankles"),
    is_open_text == FALSE
  ) %>%
  dplyr::filter(workstation != "adaptation") %>%
  dplyr::mutate(response_value = as.numeric(response_value)) %>%
  dplyr::group_by(session_sat,t_air_c, t_supply_c, v_air_m_s, session_id, 
                  subject_id, workstation) %>%
  dplyr::summarise(
    thermal_sensation = response_value[question == "thermal_sensation"],
    thermal_sensation_ankles  = response_value[question == "thermal_sensation_ankles"],
    air_movement_acceptability_ankles = response_value[question == "air_movement_acceptability_ankles"],
    dissatisfied_with_draft_ankles = response_value[question == "dissatisfied_with_draft_ankles"],
    .groups = "drop"
  )

# - Apply Liu et al. model to observed conditions -> predicted PPD --------

dissatisfied_with_draft_ankles <- dissatisfied_with_draft_ankles %>%
  dplyr::mutate(
    ppd_liu = plogis(-2.58 + 3.05 * v_air_m_s - 1.06 * thermal_sensation)
  )


# - Observed dissatisfaction rates by condition & Compare predicted -------

dissatisfied_with_draft_ankles_rate <- dissatisfied_with_draft_ankles %>%
  dplyr::group_by(session_sat, workstation) %>%
  dplyr::summarise(
    ppd_liu_mean = mean(ppd_liu, na.rm = TRUE),
    ppd_liu_sd   = sd(ppd_liu, na.rm = TRUE),
    dissatisfied_rate = mean(dissatisfied_with_draft_ankles, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    diff = ppd_liu_mean - dissatisfied_rate,
    diff_label = sprintf("%.2f", diff),
    x_mid = (ppd_liu_mean + dissatisfied_rate) / 2
  )


# Dumbell plot
# https://r-graph-gallery.com/web-extended-dumbbell-plot-ggplot2.html

df <- dissatisfied_with_draft_ankles_rate %>%
  # 1. Define the custom, non-alphabetical order for workstation
  mutate(workstation = factor(workstation, levels = c("high", "medium", "low"))) %>%
  # 2. Sort by workstation FIRST, then by session_sat
  arrange(workstation, session_sat) %>%
  mutate(
    # 3. Create the label
    condition = paste(workstation, session_sat, sep = ", "),
    # 4. Lock in the order and reverse it so ggplot plots it top-to-bottom
    condition = fct_rev(fct_inorder(condition))
  )

liu_color <- "#4361ee"
new_color <- "#f72585"
bandwidth <- 3.5

model_error_p <- ggplot(df, aes(y = condition)) +
  
  # connecting lines
  geom_segment(aes(x = dissatisfied_rate, xend = ppd_liu_mean, yend = condition),
               color = "gray80", size = bandwidth, alpha = 0.5) +
  
  # observed points (mapped for legend)
  geom_point(aes(x = dissatisfied_rate, color = "Observed"),
             size = bandwidth) +
  
  # model points (mapped for legend)
  geom_point(aes(x = ppd_liu_mean, color = "Predicted"),
             size = bandwidth) +
  
  # midpoint labels (wrapped in sprintf to force 2 decimals)
  geom_text(aes(x = x_mid, label = sprintf("%.2f", as.numeric(diff_label))),
            color = "grey30", size = 2) +
  
  # model labels (dynamic alignment, forced to 2 decimals)
  geom_text(aes(
    x = ppd_liu_mean,
    label = sprintf("%.2f", ppd_liu_mean),
    hjust = ifelse(ppd_liu_mean > dissatisfied_rate, -0.7, 1.7)
  ),
  color = liu_color, size = 2) +
  
  # observed labels (dynamic alignment, forced to 2 decimals)
  geom_text(aes(
    x = dissatisfied_rate,
    label = sprintf("%.2f", dissatisfied_rate),
    hjust = ifelse(dissatisfied_rate < ppd_liu_mean, 1.7, -0.7)
  ),
  color = new_color, size = 2) +
  
  scale_color_manual(
    name = NULL,
    values = c("Observed" = new_color, "Predicted" = liu_color)
  ) +
  
  scale_x_continuous(
    labels = percent_format(accuracy = 2),
    limits = c(0, 0.6), # Updated to cap
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  
  labs(
    x = "Percentage of People Dissatisfied (%)",
    y = "Condition",
  ) +
  
  theme_minimal(base_size = 7) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 7),
    axis.title.y = element_text(size = 8, margin = margin(r = 3, unit = "mm" )),
    axis.title.x = element_text(size = 8, margin = margin(t = 3, unit = "mm" )),
    
    legend.text = element_text(size = 7),
    legend.background = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    # legend.position = "inside",
    # legend.position.inside = c(0.8,0.1),
    # legend.direction = "horizontal",
  )

rm(df)

ggsave(
  here::here("manuscript", "figs", "model_error_raw.png"),
  plot = model_error_p,
  dpi = 500,
  width = double_col_width,
  height = 100,
  units = "mm",
  bg = "transparent"
)

model_comparison_d <- dissatisfied_with_draft_ankles_rate %>%
  mutate(across(where(is.numeric), \(x) round(x, digits = 2))) %>% 
  rename(
    session_air_flow = workstation
  )



# - Calibration curve of Liu's model on Toby's dataset ----------------------

Liumodel_calibrationcurve_obj <- valProbggplot(
  p = dissatisfied_with_draft_ankles$ppd_liu,
  y = dissatisfied_with_draft_ankles$dissatisfied_with_draft_ankles,
  smooth = "none",
  logistic.cal = TRUE,
  col.log = "#f72585",
  lwd.log = 1.2,
  col.ideal = "grey40",
  lwd.ideal = 1,
  xlab = "Predicted Probability (-)",
  ylab = "Observed Proportion (-)",
  xlim = c(0.2, 1),
  ylim = c(0.1, 1),
  statloc = c(0.05, 0.85),
  dostats = c("Intercept", "Slope", "C (ROC)", "Brier"),
  roundstats = 2,
  d0lab = "Satisfied",
  d1lab = "Dissatisfied",
  size.d01 = 2.2, # Scaled down for base_size 7
  size = 2.2,     # Scaled down for base_size 7
  dist.label = -0.1,
  line.bins = -0.05,
  dist.label2 = 0.05,
  allowPerfectPredictions = FALSE,
  legendloc = c(0.1, 0.95)
)

liu_calibration_stats <- Liumodel_calibrationcurve_obj$stats

# Apply compatible theme
axis_grey <- "gray90"

Liumodel_calibrationcurve <- Liumodel_calibrationcurve_obj$ggPlot +
  geom_line(lineend = "round", linejoin = "round") + 
  theme_minimal(base_size = 7) + 
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.15, 1), clip = "on") +
  scale_x_continuous(
    expand = c(0, 0), 
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1)
  ) +
  scale_y_continuous(
    expand = c(0, 0), 
    breaks = seq(0, 1, 0.2),
    limits = c(-0.1, 1)
  ) +
  theme(
    axis.title.x = element_text(size = 8, margin = margin(t = 3, unit = "mm")),
    axis.title.y = element_text(size = 8, margin = margin(r = 3, unit = "mm")),
    axis.text = element_text(size = 7),
    
    # Remove grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Add ticks and color the axis lines to match the first plot
    axis.ticks = element_line(color = axis_grey, size = 0.3),
    axis.ticks.length = unit(1, "mm"),
    axis.line = element_line(color = axis_grey, size = 0.3, lineend = "round"),
    
    # Legend and Padding
    legend.position = "top",
    legend.direction = "horizontal",
    legend.text = element_text(size = 7),
    legend.margin = margin(b = -2),
    plot.margin = margin(l = 5, t = 2, b = 5, unit = "mm")
  )


ggsave(
  here::here("manuscript", "figs", "liu_calibration_curve.png"), 
  plot = Liumodel_calibrationcurve,
  dpi = 500,
  width = 89,
  height = 89,
  units = "mm",
  bg = "transparent"
)


# Combined figure

model_performance_p <- (Liumodel_calibrationcurve | model_error_p ) +
  plot_layout(widths = c(1, 2)) +
  plot_annotation(tag_levels = "a", tag_suffix = ".") &
  theme(
    plot.subtitle    = element_text(hjust = 0.05, margin = margin(b = 3, unit = "mm")),
    plot.tag         = element_text(size = 7, face = "bold"),
    plot.margin      = margin(b = 2, r = 5, unit = "mm"),
  )

ggsave(
  here::here("manuscript", "figs", "model_performance_combined_raw.png"),
  plot = model_performance_p,
  dpi = 500,
  width = double_col_width,
  height = 100,
  units = "mm",
  bg = "transparent"
)


# 5. Updated Model for Ankle exposed/unexposed Conditions ======================================
# combine dataset -------------------------------------------------------

analysis_all <- bind_rows(
  analysis %>%
    dplyr::filter(
      is_open_text == FALSE
    ) %>%
    dplyr::filter(workstation != "adaptation") %>%
    dplyr::mutate(response_value = as.numeric(response_value)) %>%
    dplyr::select(timestamp,subject_id,workstation,t_supply_c, t_air_c, v_air_m_s,question,response_value, turbulence_intensity) %>%
    dplyr::mutate(source = "toby",clothing_type = "long"),
  
  analysis_liu %>%
    dplyr::filter(workstation != "adaptation") %>%
    dplyr::mutate(response_value = as.numeric(response_value)) %>%
    dplyr::select(timestamp,subject_id,workstation,t_supply_c, t_air_c, v_air_m_s,question,response_value, clothing_type, turbulence_intensity) %>%
    mutate(source = "liu")
)

new_model <- analysis_all %>%
  dplyr::arrange(subject_id, workstation, t_supply_c, v_air_m_s, t_air_c, question, timestamp) %>%
  dplyr::group_by(t_supply_c, v_air_m_s, t_air_c, subject_id, workstation, question) %>%
  dplyr::mutate(rep_id = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    id_cols = c(t_supply_c, v_air_m_s, t_air_c, subject_id, workstation, rep_id, source, clothing_type, turbulence_intensity),
    names_from = question,
    values_from = response_value
  ) %>%
  dplyr::mutate(
    dissatisfied_with_draft_ankles = dplyr::if_else(
      source == "liu",
      as.integer(thermal_sensation_ankles < 0 & air_movement_acceptability < 0),
      dissatisfied_with_draft_ankles
    )
  ) %>%
  dplyr::filter(!is.na(dissatisfied_with_draft_ankles),
                !is.na(thermal_sensation))

# Center continuous predictors on the full-dataset mean -------------------
# Centering reference is computed once from new_model.
ctr_vars <- c("v_air_m_s", "thermal_sensation", "t_air_c", "t_supply_c", "turbulence_intensity")
ctr_means <- colMeans(new_model[ctr_vars], na.rm = TRUE)
new_model_c <- new_model %>%
  dplyr::mutate(across(all_of(ctr_vars), \(x) x- mean(x, na.rm = TRUE)))
# Print centering reference for reproducibility
tibble::enframe(ctr_means, name = "variable", value = "mean (centering reference)")

# Full model for predictor screening (not the final model) ----------------
model_input <- glmer(
  dissatisfied_with_draft_ankles~ v_air_m_s + t_air_c + t_supply_c + thermal_sensation + turbulence_intensity + clothing_type + source + (1 |subject_id),
  data = new_model_c,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)
summary(model_input)

# Fit separate mixed-effects models for "Ankle exposed" and "Ankle unexposed" --------

# 1. fit models
m_glmm_exposed <- glmer(
  dissatisfied_with_draft_ankles ~ v_air_m_s + thermal_sensation + (1 | subject_id),
  data = new_model_c %>% filter(clothing_type == "short"),
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

m_glmm_unexposed <- glmer(
  dissatisfied_with_draft_ankles ~ v_air_m_s + thermal_sensation + (1 | subject_id),
  data = new_model_c %>% filter(clothing_type == "long"),
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

summary(m_glmm_exposed)
summary(m_glmm_unexposed)

# 2. extract coefficients
fe_exposed <- fixef(m_glmm_exposed)
a_exposed <- unname(fe_exposed["(Intercept)"])
b_exposed <- unname(fe_exposed["v_air_m_s"])
c_exposed <- unname(fe_exposed["thermal_sensation"])
sd_b0_exposed <- sqrt(as.numeric(VarCorr(m_glmm_exposed)$subject_id[1, 1]))

fe_unexposed <- fixef(m_glmm_unexposed)
a_unexposed <- unname(fe_unexposed["(Intercept)"])
b_unexposed <- unname(fe_unexposed["v_air_m_s"])
c_unexposed <- unname(fe_unexposed["thermal_sensation"])
sd_b0_unexposed <- sqrt(as.numeric(VarCorr(m_glmm_unexposed)$subject_id[1, 1]))

# 3. common V-TS grid
grid_base <- expand_grid(
  V  = seq(0, 1, length.out = 100),
  TS = seq(-3, 3, length.out = 100)
)

# 4. Monte Carlo marginal probs
set.seed(1)
K <- 10000
b0_draw_exposed  <- rnorm(K, mean = 0, sd = sd_b0_exposed)
b0_draw_unexposed <- rnorm(K, mean = 0, sd = sd_b0_unexposed)

grid_exposed <- grid_base %>%
  mutate(
    eta = a_exposed + b_exposed * (V - ctr_means["v_air_m_s"]) + c_exposed * (TS - ctr_means["thermal_sensation"]),
    p_marg = vapply(
      eta,
      function(e) mean(plogis(e + b0_draw_exposed)),
      numeric(1)
    ),
    p_marg_clip = pmin(pmax(p_marg, 1e-8), 1 - 1e-8),
    logit_p = qlogis(p_marg_clip)
  )

grid_unexposed <- grid_base %>%
  mutate(
    eta = a_unexposed + b_unexposed * (V - ctr_means["v_air_m_s"]) + c_unexposed * (TS - ctr_means["thermal_sensation"]),
    p_marg = vapply(
      eta,
      function(e) mean(plogis(e + b0_draw_unexposed)),
      numeric(1)
    ),
    p_marg_clip = pmin(pmax(p_marg, 1e-8), 1 - 1e-8),
    logit_p = qlogis(p_marg_clip)
  )

# 5. Approximate closed-form equations
m_approx_exposed  <- lm(logit_p ~ V + TS, data = grid_exposed)
m_approx_unexposed <- lm(logit_p ~ V + TS, data = grid_unexposed)

# 6. approximation accuracy output

grid_exposed <- grid_exposed %>%
  mutate(p_hat = plogis(predict(m_approx_exposed, newdata = grid_exposed)))

rmse_exposed <- sqrt(mean((grid_exposed$p_hat - grid_exposed$p_marg)^2))
mae_exposed  <- mean(abs(grid_exposed$p_hat - grid_exposed$p_marg))
r2_exposed   <- cor(grid_exposed$p_hat, grid_exposed$p_marg)^2

c(RMSE_exposed = rmse_exposed, MAE_exposed = mae_exposed, R2_exposed = r2_exposed)

grid_unexposed <- grid_unexposed %>%
  mutate(p_hat = plogis(predict(m_approx_unexposed, newdata = grid_unexposed)))

rmse_unexposed <- sqrt(mean((grid_unexposed$p_hat - grid_unexposed$p_marg)^2))
mae_unexposed  <- mean(abs(grid_unexposed$p_hat - grid_unexposed$p_marg))
r2_unexposed   <- cor(grid_unexposed$p_hat, grid_unexposed$p_marg)^2

c(RMSE_unexposed = rmse_unexposed, MAE_unexposed = mae_unexposed, R2_unexposed = r2_unexposed)


# Visualization for two models--------------------------------------------------
# 1. grids for plotting closed-form equations

cf_exposed <- coef(m_approx_exposed)
cf_unexposed <- coef(m_approx_unexposed)

plot_grid_exposed <- expand_grid(
  TS = seq(-3, 3, length.out = 300),
  V  = seq(0, 1, length.out = 300)
) %>%
  mutate(
    eta = cf_exposed[1] + cf_exposed[2] * V + cf_exposed[3] * TS,
    PPD = 100 * plogis(eta)
  )

plot_grid_unexposed <- expand_grid(
  TS = seq(-3, 3, length.out = 300),
  V  = seq(0, 1, length.out = 300)
) %>%
  mutate(
    eta = cf_unexposed[1] + cf_unexposed[2] * V + cf_unexposed[3] * TS,
    PPD = 100 * plogis(eta)
  )

p_exposed  <- plot_draft_model(plot_grid_exposed,"a.","Ankle Uncovered")

p_unexposed <- plot_draft_model(plot_grid_unexposed,"b.","Ankle Covered")

model_final <- p_exposed + p_unexposed +
  plot_layout(ncol = 1)
ggsave(
  here::here("manuscript", "figs", "Model.png"),
  plot = model_final,
  dpi = 500,
  width = single_col_width,
  height = 160,
  units = "mm",
  bg = "transparent"
)


# 2. Save closed-form equations
model_formula <- tibble(
  Model = c("exposed","unexposed"),
  
  Intercept = c(cf_exposed[1], cf_unexposed[1]),
  V_coef    = c(cf_exposed[2], cf_unexposed[2]),
  TS_coef   = c(cf_exposed[3], cf_unexposed[3]),
  
  RMSE = c(rmse_exposed, rmse_unexposed),
  MAE  = c(mae_exposed,  mae_unexposed),
  R2   = c(r2_exposed,   r2_unexposed)
) %>%
  mutate(
    eta = sprintf("%.4f + %.4f*V + %.4f*TS",Intercept, V_coef, TS_coef),
    logit_formula = paste0("logit(P) = ", eta),
    probability_formula = paste0("P = exp(", eta, ")/(1 + exp(", eta, "))"
    )
  )


# Skin Temperature--------------------------------------------------


# 1) Ankle skin temperature change curve（mean +/- SD ribbon） -------------------------------------------
# Although the raw data were also recorded every 10 seconds, 
# re-binned them into 10-second intervals here to 
# avoid slight misalignment in sampling timestamps across subjects.
time_bin_s <- 10

tsk_timecourse <- tsk_ankle %>%
  dplyr::left_join(
    sessions %>%
      dplyr::select(session_id, session_type, session_sat) %>%
      dplyr::distinct(),
    by = "session_id"
  ) %>%
  dplyr::mutate(
    plot_time_s = as.numeric(difftime(timestamp, anchor_time, units = "secs"))
  ) %>%
  dplyr::rename(interval_phase = interval_type) %>%
  dplyr::select(
    subject_id, session_id, session_type, session_sat, round_id, workstation, tsk_site,
    interval_phase, anchor_time, timestamp, tsk_c, plot_time_s
  )

tsk_timecourse_subject <- tsk_timecourse %>%
  dplyr::mutate(
    plot_time_s_bin = time_bin_s * floor(plot_time_s / time_bin_s),
    plot_time_min = plot_time_s_bin / 60
  ) %>%
  dplyr::group_by(
    subject_id, session_id, session_type, workstation, tsk_site,
    plot_time_s_bin, plot_time_min
  ) %>%
  dplyr::summarise(
    tsk_c = mean(tsk_c, na.rm = TRUE),
    .groups = "drop"
  )

tsk_timecourse_summary <- tsk_timecourse_subject %>%
  dplyr::group_by(session_type, workstation, tsk_site, plot_time_s_bin, plot_time_min) %>%
  dplyr::summarise(
    mean_value = mean(tsk_c, na.rm = TRUE),
    sd_value = sd(tsk_c, na.rm = TRUE),
    n = dplyr::n_distinct(subject_id),
    .groups = "drop"
  ) %>%
  dplyr::arrange(session_type, workstation, tsk_site, plot_time_s_bin)

tsk_timecourse_p <- plot_timecourse_mean_sd(
  tsk_timecourse_summary,
  y_lab = "Skin temperature (°C)"
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_segment(
    data = tibble::tibble(x = c(0, 5, 10, 15, 20)),
    aes(
      x = x, xend = x,
      y = -Inf, yend = -Inf
    ),
    inherit.aes = FALSE,
    linewidth = 0.3,
    color = "grey30",
    lineend = "butt"
  ) +
  scale_x_continuous(
    breaks = c(-10, 0, 5, 10, 15, 20),
    labels = c("Adaptation", "0", "5", "10", "15", "20"),
    limits = c(-20, 20)
  )

# 2) Stable skin temperature (last 5 minutes) ----------------------------------

# the skin temperature during the last 5 minutes is treated as the “stable” value
tsk_stable_last5 <- tsk_timecourse %>%
  dplyr::filter(interval_phase == "Workstation", plot_time_s > 15 * 60, plot_time_s <= 20 * 60) %>%
  dplyr::group_by(subject_id, session_id, round_id, workstation, tsk_site) %>%
  dplyr::summarise(
    mean_last5_c = mean(tsk_c, na.rm = TRUE),
    sd_last5_c = sd(tsk_c, na.rm = TRUE),
    n_obs = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    sessions %>% dplyr::select(session_id, session_sat) %>% dplyr::distinct(),
    by = "session_id"
  )

tsk_last5_paired_ttest <- paired_t_test(
  data = tsk_stable_last5,
  group_by_var = "session_sat",
  within_var = "tsk_site",
  subject_var = "subject_id",
  question_var = "workstation",
  value_var = "mean_last5_c"
)

# 3) Time course relative to pre-workstation thermal comfort baseline ----------

# kin temperature was divided into the “formal experiment period” and the “adaptation period” 
# based on the 0-20 min and 20-40 min intervals preceding the final response.
# I also considered defining the “adaptation period” as the 20 min preceding the last response during adaptation. 
# However, because response speed varied across participants, this introduced a small fluctuation in skin temperature 
# at the transition between the two periods.

baseline_values <- tsk_timecourse %>%
  dplyr::filter(interval_phase == "Adaptation") %>%
  dplyr::group_by(subject_id, session_id, round_id, workstation, tsk_site, anchor_time) %>%
  dplyr::slice_min(
    order_by = abs(as.numeric(difftime(timestamp, anchor_time, units = "secs"))),
    n = 1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    subject_id,
    session_id,
    round_id,
    workstation,
    tsk_site,
    anchor_time,
    baseline_timestamp = timestamp,
    baseline_tsk_c = tsk_c,
    baseline_gap_s = abs(as.numeric(difftime(timestamp, anchor_time, units = "secs")))
  )

tsk_timecourse_delta <- tsk_timecourse %>%
  dplyr::filter(interval_phase == "Workstation", plot_time_s >= 0, plot_time_s <= 20 * 60) %>%
  dplyr::left_join(
    baseline_values,
    by = c("subject_id", "session_id", "round_id", "workstation", "tsk_site", "anchor_time")
  ) %>%
  dplyr::mutate(delta_tsk_c = tsk_c - baseline_tsk_c)

tsk_timecourse_delta_subject <- tsk_timecourse_delta %>%
  dplyr::mutate(
    plot_time_s_bin = time_bin_s * floor(plot_time_s / time_bin_s),
    plot_time_min = plot_time_s_bin / 60
  ) %>%
  dplyr::group_by(
    subject_id, session_id, session_type, workstation, tsk_site,
    plot_time_s_bin, plot_time_min
  ) %>%
  dplyr::summarise(
    delta_tsk_c = mean(delta_tsk_c, na.rm = TRUE),
    .groups = "drop"
  )

tsk_timecourse_delta_summary <- tsk_timecourse_delta_subject %>%
  dplyr::group_by(session_type, workstation, tsk_site, plot_time_s_bin, plot_time_min) %>%
  dplyr::summarise(
    mean_value = mean(delta_tsk_c, na.rm = TRUE),
    sd_value = sd(delta_tsk_c, na.rm = TRUE),
    n = dplyr::n_distinct(subject_id),
    .groups = "drop"
  ) %>%
  dplyr::arrange(session_type, workstation, tsk_site, plot_time_s_bin)

# 4) Delta skin temperature in the last minute (19-20 min) --------------------

# To compare delta skin temperature across different conditions, 
# the delta skin temperature during the last 1 minute is defined
# as the final value for paired-sample t-tests.
tsk_delta_last1 <- tsk_timecourse_delta %>%
  dplyr::filter(plot_time_s > 19 * 60, plot_time_s <= 20 * 60) %>%
  dplyr::group_by(subject_id, session_id, session_sat, workstation, tsk_site) %>%
  dplyr::summarise(
    mean_delta_last1_c = mean(delta_tsk_c, na.rm = TRUE),
    sd_delta_last1_c = sd(delta_tsk_c, na.rm = TRUE),
    n_obs = dplyr::n(),
    .groups = "drop"
  )

tsk_delta_last1_paired_ttest <- paired_t_test(
  data = tsk_delta_last1,
  group_by_var = "session_sat",
  within_var = "tsk_site",
  subject_var = "subject_id",
  question_var = "workstation",
  value_var = "mean_delta_last1_c"
)

tsk_timecourse_delta_p <- plot_timecourse_mean_sd(
  tsk_timecourse_delta_summary,
  y_lab = expression(Delta * "Skin temperature from baseline (" * degree * "C)")
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  scale_x_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20)) +
  theme(legend.position = "none")

tsk_p <- (tsk_timecourse_p / tsk_timecourse_delta_p) +
  plot_annotation(tag_levels = "a", tag_suffix = ".") &
  theme(
    plot.subtitle = element_text(hjust = 0.05, margin = margin(b = 3, unit = "mm")),
    plot.tag = element_text(size = 7, face = "bold"),
    plot.margin = margin(b = 5, unit = "mm"),
    axis.title = element_text(margin = margin(r = 2, unit = "mm")),
    legend.margin = margin(l = 3, unit = "mm")
  )

ggsave(
  here::here("manuscript", "figs", "tsk_p.png"),
  plot = tsk_p,
  dpi = 500,
  width = double_col_width,
  height = 120,
  units = "mm",
  bg = "transparent"
)

rm(
  grid_base, grid_exposed, grid_unexposed, Liumodel_calibrationcurve,
  m_approx_exposed, m_approx_unexposed,
  plot_grid_exposed, plot_grid_unexposed,
  tsk_ankle, tsk_timecourse,
  baseline_values, tsk_timecourse_delta
)
