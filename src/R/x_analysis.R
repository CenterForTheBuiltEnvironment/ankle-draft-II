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

thermal_sensation_p <- analysis %>%
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
  {
    thermal_sensation_stats <<- paired_t_test(
      data = .,
      group_by_var = "session_sat",
      within_var = "workstation",
      subject_var = "subject_id",
      question_var = "question",
      value_var = "response_value",
      order_var = "timestamp"
    )
    .
  } %>%
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

thermal_preference_overall_p <- analysis %>%
  dplyr::filter(
    question %in% c("thermal_preference"),
    is_open_text == FALSE
  ) %>%
  dplyr::filter(workstation != "adaptation") %>%
  dplyr::mutate(
    response_value_num = as.numeric(response_value),
    response_value = factor(
      dplyr::case_when(
        response_value_num < 0.0 ~ "Cooler",
        response_value_num > 0.0 ~ "Warmer",
        response_value_num == 0.0 ~ "No change"
      ),
      levels = c("Cooler", "No change", "Warmer")
    )
  ) %>%
  {
    thermal_preference_overall_stats <<- paired_wilcox_test(
      data = dplyr::mutate(., response_value = response_value_num),
      group_by_var = "session_sat",
      subject_var = "subject_id",
      question_var = "workstation",
      value_var = "response_value",
      order_var = "timestamp"
    )
    .
  } %>%
  plot_stacked_pct_ws(response_value, thermal_preference_palette) +
  labs(subtitle = "Thermal preference  |  Overall", x = NULL)


# Thermal preference (ankles) --------------------------------------------------

thermal_preference_ankles_p <- analysis %>%
  dplyr::filter(
    question %in% c("thermal_preference_ankles"),
    is_open_text == FALSE
  ) %>%
  dplyr::filter(workstation != "adaptation") %>%
  dplyr::mutate(
    response_value_num = as.numeric(response_value),
    response_value = factor(
      dplyr::case_when(
        response_value_num < 0.0 ~ "Cooler",
        response_value_num > 0.0 ~ "Warmer",
        response_value_num == 0.0 ~ "No change"
      ),
      levels = c("Cooler", "No change", "Warmer")
    )
  ) %>%
  {
    thermal_preference_ankle_stats <<- paired_wilcox_test(
      data = dplyr::mutate(., response_value = response_value_num),
      group_by_var = "session_sat",
      subject_var = "subject_id",
      question_var = "workstation",
      value_var = "response_value",
      order_var = "timestamp"
    )
    .
  } %>%
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

thermal_acceptability_p <- analysis %>%
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
  {
    thermal_acceptability_stats <<- paired_t_test(
      data = .,
      group_by_var = "session_sat",
      subject_var = "subject_id",
      question_var = "workstation",
      value_var = "response_value",
      order_var = "timestamp"
    )
    .
  } %>%
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
    plot.tag = element_text(size = 10, face = "bold"),
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

air_movement_acceptability_p <- analysis %>%
  dplyr::filter(
    question %in% c("air_movement_acceptability_ankles"),
    is_open_text == FALSE
  ) %>%
  dplyr::filter(workstation != "adaptation") %>%
  dplyr::mutate(
    response_value_num = as.numeric(response_value),
    acceptability = factor(
      dplyr::case_when(
        response_value_num > 0 ~ "Acceptable",
        response_value_num < 0 ~ "Unacceptable",
        response_value_num == 0 ~ NA
      ),
      levels = c("Unacceptable", "Acceptable"),
      ordered = TRUE
    )
  ) %>%
  {
    air_movement_acceptability_stats <<- paired_wilcox_test(
      data = dplyr::mutate(
        .,
        acceptability_num = dplyr::case_when(
          acceptability == "Unacceptable" ~ 0,
          acceptability == "Acceptable" ~ 1,
          TRUE ~ NA_real_
        )
      ),
      group_by_var = "session_sat",
      subject_var = "subject_id",
      question_var = "workstation",
      value_var = "acceptability_num",
      order_var = "timestamp"
    )
    .
  } %>%
  plot_stacked_pct_ws(
    acceptability,
    palette = c("Unacceptable" = "#d99fa8", "Acceptable" = "#c1e0b9")
  ) +
  labs(subtitle = "Ankle air movement acceptability", x = NULL)


# Air movement preference ------------------------------------------------------

air_movement_preference_p <- analysis %>%
  dplyr::filter(
    question %in% c("air_movement_preference_ankles"),
    is_open_text == FALSE
  ) %>%
  dplyr::filter(workstation != "adaptation") %>%
  dplyr::mutate(
    response_value_num = as.numeric(response_value),
    acceptability = factor(
      dplyr::case_when(
        response_value_num > 0 ~ "More",
        response_value_num < 0 ~ "Less",
        response_value_num == 0 ~ "No change"
      ),
      levels = c("More", "No change", "Less")
    )
  ) %>%
  {
    air_movement_preference_stats <<- paired_wilcox_test(
      data = dplyr::mutate(., response_value = response_value_num),
      group_by_var = "session_sat",
      subject_var = "subject_id",
      question_var = "workstation",
      value_var = "response_value",
      order_var = "timestamp"
    )
    .
  } %>%
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


# 5. Updated Model inc. Winter Conditions ======================================
# TODO: Integrate analysis from analysis_old.R
# - combine dataset -------------------------------------------------------

analysis_all <- bind_rows(
  analysis %>%
    dplyr::filter(
      is_open_text == FALSE
    ) %>%
    dplyr::filter(workstation != "adaptation") %>%
    dplyr::mutate(response_value = as.numeric(response_value)) %>%
    dplyr::select(timestamp,subject_id,workstation,t_supply_c, t_air_c, v_air_m_s,question,response_value) %>%
    dplyr::mutate(source = "toby"),
  
  analysis_liu %>%
    dplyr::filter(workstation != "adaptation") %>%
    dplyr::mutate(response_value = as.numeric(response_value)) %>%
    dplyr::select(timestamp,subject_id,workstation,t_supply_c, t_air_c, v_air_m_s,question,response_value) %>%
    mutate(source = "liu")
)

new_model <- analysis_all %>%
  dplyr::arrange(subject_id, workstation, t_supply_c, v_air_m_s, t_air_c, question, timestamp) %>%
  dplyr::group_by(t_supply_c, v_air_m_s, t_air_c, subject_id, workstation, question) %>%
  dplyr::mutate(rep_id = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    id_cols = c(t_supply_c, v_air_m_s, t_air_c, subject_id, workstation, rep_id, source),
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


# Fit separate mixed-effects models using Liu's dataset and Toby's --------

# 1. fit models
m_glmm_liu <- glmer(
  dissatisfied_with_draft_ankles ~ v_air_m_s + thermal_sensation + (1 | subject_id),
  data = new_model %>% filter(source == "liu"),
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

m_glmm_toby <- glmer(
  dissatisfied_with_draft_ankles ~ v_air_m_s + thermal_sensation + (1 | subject_id),
  data = new_model %>% filter(source == "toby"),
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

summary(m_glmm_liu)
summary(m_glmm_toby)

# 2. extract coefficients
fe_liu <- fixef(m_glmm_liu)
a_liu <- unname(fe_liu["(Intercept)"])
b_liu <- unname(fe_liu["v_air_m_s"])
c_liu <- unname(fe_liu["thermal_sensation"])
sd_b0_liu <- sqrt(as.numeric(VarCorr(m_glmm_liu)$subject_id[1, 1]))

fe_toby <- fixef(m_glmm_toby)
a_toby <- unname(fe_toby["(Intercept)"])
b_toby <- unname(fe_toby["v_air_m_s"])
c_toby <- unname(fe_toby["thermal_sensation"])
sd_b0_toby <- sqrt(as.numeric(VarCorr(m_glmm_toby)$subject_id[1, 1]))

# 3. common V-TS grid
grid_base <- expand_grid(
  V  = seq(0, 1, length.out = 100),
  TS = seq(-3, 3, length.out = 100)
)

# 4. Monte Carlo marginal probs
set.seed(1)
K <- 10000
b0_draw_liu  <- rnorm(K, mean = 0, sd = sd_b0_liu)
b0_draw_toby <- rnorm(K, mean = 0, sd = sd_b0_toby)

grid_liu <- grid_base %>%
  mutate(
    eta = a_liu + b_liu * V + c_liu * TS,
    p_marg = vapply(
      eta,
      function(e) mean(plogis(e + b0_draw_liu)),
      numeric(1)
    ),
    p_marg_clip = pmin(pmax(p_marg, 1e-8), 1 - 1e-8),
    logit_p = qlogis(p_marg_clip)
  )

grid_toby <- grid_base %>%
  mutate(
    eta = a_toby + b_toby * V + c_toby * TS,
    p_marg = vapply(
      eta,
      function(e) mean(plogis(e + b0_draw_toby)),
      numeric(1)
    ),
    p_marg_clip = pmin(pmax(p_marg, 1e-8), 1 - 1e-8),
    logit_p = qlogis(p_marg_clip)
  )

# Approximate closed-form equations ------------------------------------------

m_approx_liu  <- lm(logit_p ~ V + TS, data = grid_liu)
m_approx_toby <- lm(logit_p ~ V + TS, data = grid_toby)

summary(m_approx_liu)
summary(m_approx_toby)

coef(m_approx_liu)
coef(m_approx_toby)

# approximation accuracy

grid_liu <- grid_liu %>%
  mutate(p_hat = plogis(predict(m_approx_liu, newdata = grid_liu)))

rmse_liu <- sqrt(mean((grid_liu$p_hat - grid_liu$p_marg)^2))
mae_liu  <- mean(abs(grid_liu$p_hat - grid_liu$p_marg))
r2_liu   <- cor(grid_liu$p_hat, grid_liu$p_marg)^2

c(RMSE_liu = rmse_liu, MAE_liu = mae_liu, R2_liu = r2_liu)

grid_toby <- grid_toby %>%
  mutate(p_hat = plogis(predict(m_approx_toby, newdata = grid_toby)))

rmse_toby <- sqrt(mean((grid_toby$p_hat - grid_toby$p_marg)^2))
mae_toby  <- mean(abs(grid_toby$p_hat - grid_toby$p_marg))
r2_toby   <- cor(grid_toby$p_hat, grid_toby$p_marg)^2

c(RMSE_toby = rmse_toby, MAE_toby = mae_toby, R2_toby = r2_toby)


# Visualization for two models--------------------------------------------------
# 1. grids for plotting closed-form equations

cf_liu <- coef(m_approx_liu)
cf_toby <- coef(m_approx_toby)

plot_grid_liu <- expand_grid(
  TS = seq(-3, 3, length.out = 300),
  V  = seq(0, 1, length.out = 300)
) %>%
  mutate(
    eta = cf_liu[1] + cf_liu[2] * V + cf_liu[3] * TS,
    PPD = 100 * plogis(eta)
  )

plot_grid_toby <- expand_grid(
  TS = seq(-3, 3, length.out = 300),
  V  = seq(0, 1, length.out = 300)
) %>%
  mutate(
    eta = cf_toby[1] + cf_toby[2] * V + cf_toby[3] * TS,
    PPD = 100 * plogis(eta)
  )

p_liu  <- plot_draft_model(plot_grid_liu,"a.","Ankle Uncovered")

p_toby <- plot_draft_model(plot_grid_toby,"b.","Ankle Covered")

model_final <- p_liu + p_toby +
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


# Save closed-form equations -----------------------------------------------

model_formula <- tibble(
  Model = c("Liu","Toby"),
  
  Intercept = c(cf_liu[1], cf_toby[1]),
  V_coef    = c(cf_liu[2], cf_toby[2]),
  TS_coef   = c(cf_liu[3], cf_toby[3]),
  
  RMSE = c(rmse_liu, rmse_toby),
  MAE  = c(mae_liu,  mae_toby),
  R2   = c(r2_liu,   r2_toby)
) %>%
  mutate(
    eta = sprintf("%.4f + %.4f*V + %.4f*TS",Intercept, V_coef, TS_coef),
    logit_formula = paste0("logit(P) = ", eta),
    probability_formula = paste0("P = exp(", eta, ")/(1 + exp(", eta, "))"
    )
  )

rm(grid_base,grid_liu,grid_toby,Liumodel_calibrationcurve,m_approx_liu,m_approx_toby,
   m_glmm_liu,m_glmm_toby,plot_grid_liu,plot_grid_toby)