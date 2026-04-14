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
    question %in% c("thermal_sensation_ankles", "air_movement_acceptability_ankles","dissatisfied_with_draft_ankles"),
    is_open_text == FALSE
  ) %>%
  dplyr::filter(workstation != "adaptation") %>%
  dplyr::mutate(response_value = as.numeric(response_value)) %>%
  dplyr::group_by(session_sat,t_air_c, t_supply_c, v_air_m_s, session_id, 
                  subject_id, workstation) %>%
  dplyr::summarise(
    thermal_sensation_ankles  = response_value[question == "thermal_sensation_ankles"],
    air_movement_acceptability_ankles = response_value[question == "air_movement_acceptability_ankles"],
    dissatisfied_with_draft_ankles = response_value[question == "dissatisfied_with_draft_ankles"],
    .groups = "drop"
  )

# - Apply Liu et al. model to observed conditions -> predicted PPD --------

dissatisfied_with_draft_ankles <- dissatisfied_with_draft_ankles %>%
  dplyr::mutate(
    ppd_liu = plogis(-2.58 + 3.05 * v_air_m_s - 1.06 * thermal_sensation_ankles)
  )


# - Observed dissatisfaction rates by condition & Compare predicted -------

Liumodel_performance <- dissatisfied_with_draft_ankles %>%
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
  ) %>%
  {dissatisfied_with_draft_ankles_rate <<- . ; .} %>%
  {
    label_df <- dplyr::distinct(., session_sat) %>%
      dplyr::mutate(
        x = -Inf,
        y = Inf,
        label = session_sat
      )
    
    ggplot(.) +
      geom_segment(
        aes(
          x = dissatisfied_rate+0.02,
          xend = ppd_liu_mean-0.02,
          y = workstation,
          yend = workstation,
          color = workstation
        ),
        arrow = arrow(length = unit(0.15, "cm")),
        colour = "grey40"
      ) +
      geom_text(
        aes(
          x = x_mid,
          y = workstation,
          label = diff_label
        ),
        colour = "grey30",
        size = 2.5,
        vjust = -0.7
      ) +
      geom_point(
        aes(x = ppd_liu_mean, y = workstation, color = workstation,
            shape = "Liu's model output"),
        size = 2.5,
      ) +
      geom_point(
        aes(x = dissatisfied_rate, y = workstation, color = workstation,
            shape = "Observed"),
        size = 2.5,
      ) +
      geom_text(
        data = label_df,
        aes(x = x, y = y, label = label),
        inherit.aes = FALSE,
        hjust = -0.3,
        vjust = 2,
        size = 3
      ) +
      ggh4x::facet_wrap2(
        ~ session_sat,
        ncol = 1,
        axes = "x",
        remove_labels = "x"
      )+
      scale_color_manual(values = rev(air_movement_preference_palette)) +
      scale_shape_manual(
        values = c(
          "Observed" = 17,
          "Liu's model output"   = 16
        ),
        breaks = c(
          "Observed",
          "Liu's model output"
        ),
        name = NULL
      ) +
      scale_x_continuous(
        limits = c(0, 0.6),
        breaks = seq(0, 0.6, by = 0.2),
        expand = c(0, 0)
      )+
      scale_y_discrete(
        name = "Air speed level",
      ) +
      labs(
        x = "Probability",
        color = NULL
      ) +
      guides(
        color = "none",
        shape = guide_legend(
          override.aes = list(color = "grey80")
        )
      )+
      theme_classic(base_size = 9) +
      theme(
        plot.margin = margin(r = 2, unit = "mm"),
        panel.spacing.y = unit(1, "lines"),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        strip.background = element_blank(),
        strip.text = element_blank()
      )
  }

ggsave(
  here::here("manuscript", "figs", "Liumodel_performance.png"),
  plot = Liumodel_performance,
  dpi = 500,
  width = single_col_width,
  height = 110,
  units = "mm",
  bg = "transparent"
)

# - Calibration curve of Liu's model on Toby's dataset ----------------------

Liumodel_calibrationcurve <- valProbggplot(
  p = dissatisfied_with_draft_ankles$ppd_liu,
  y = dissatisfied_with_draft_ankles$dissatisfied_with_draft_ankles,
  smooth = "none",
  logistic.cal = TRUE,
  col.log = "#6b8dd6",
  lwd.log = 1.2,
  col.ideal = "#eb6b58",
  lwd.ideal = 1,
  xlab = "Predicted probability",
  ylab = "Observed proportion",
  xlim = c(0, 1),
  ylim = c(-0.1, 1),
  statloc = c(0.02, 0.85),
  dostats = c("Intercept", "Slope", "C (ROC)", "Brier"),
  roundstats = 2,
  d0lab = "Satisfied",
  d1lab = "Disatisfied",
  size.d01 = 2.6,
  size = 2.8,
  dist.label = 0.000,
  line.bins = -0.02,
  dist.label2 = 0.03,
  allowPerfectPredictions = FALSE,
  legendloc = c(0.1, 0.95)
)
Liumodel_calibrationcurve <- Liumodel_calibrationcurve$ggPlot +
  theme_classic(base_size = 9)+
  coord_cartesian(xlim = c(-0.12, 1), ylim = c(-0.12, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.title.y = element_text(margin = margin(r = 6)),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.margin = margin(b = -6),
    legend.box.margin = margin(b = -3),
    legend.spacing.x = unit(0.2, "lines"),
    plot.margin = margin(r = 10),
  )


#ggsave() could not be used here, so the figure was saved using a graphics device instead. 
png(
  filename = here::here("manuscript", "figs", "Liumodel_calibrationcurve.png"),
  width = single_col_width,
  height = 100,
  units = "mm",
  res = 500,
  bg = "transparent"
)
print(Liumodel_calibrationcurve)
dev.off()


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

rm(grid_base,grid_exposed,grid_unexposed,Liumodel_calibrationcurve,m_approx_exposed,m_approx_unexposed,
   m_glmm_exposed,m_glmm_unexposed,plot_grid_exposed,plot_grid_unexposed)