# Title: Statistics
# Description: Reusable functions for statistical analysis
# Author: Toby Kramer
# Date: 2026-01-05

# ---- Setup ----
source(here::here("src", "R", "x_setup.R"))
source(here::here("src", "R", "x_func.R"))
source(here::here("src", "R", "x_data.R"))

# Create clean analysis dataset (remove outlier subjects)
excluded_subjects <- c(
  "ac008", "ss018", "hd025", "ldg026", "dd027",
  "lh030", "ar032", "cw036", "rs052", "mch053", "lg054", "ip039"
)

survey_analysis <- survey_combine %>%
  filter(!subject_id %in% excluded_subjects)

# ============================================================================
# Analysis
# ============================================================================

# ============================================================================
# Part1：Linear relationship for Air Movement Acceptability and Predictors
# ============================================================================
model_v_air <- lmer(
  air_movement_acceptability_ankles ~ v_air_s + (1 | subject_id),
  data = survey_analysis,
  REML = TRUE
)

model_thermal_sens <- lmer(
  air_movement_acceptability_ankles ~ thermal_sensation + (1 | subject_id),
  data = survey_analysis,
  REML = TRUE
)

model_ankle_sens <- lmer(
  air_movement_acceptability_ankles ~ thermal_sensation_ankles + (1 | subject_id),
  data = survey_analysis,
  REML = TRUE
)

linear_df <- survey_analysis %>%
  select(
    subject_id,
    air_movement_acceptability_ankles,
    v_air_s,
    thermal_sensation,
    thermal_sensation_ankles
  ) %>%
  pivot_longer(
    cols = c(v_air_s, thermal_sensation, thermal_sensation_ankles),
    names_to = "predictor",
    values_to = "x"
  ) %>%
  filter(is.finite(x), is.finite(air_movement_acceptability_ankles)) %>%
  mutate(predictor = factor(predictor,
                            levels = c("v_air_s", "thermal_sensation", "thermal_sensation_ankles"),
                            labels = c("Ankle air speed (m/s)",
                                      "Whole-body thermal sensation",
                                      "Ankle thermal sensation")))


lmm_v_air <- lmer(
  air_movement_acceptability_ankles ~ v_air_s + (1 | subject_id),
  data = survey_analysis,
  REML = TRUE
)

lmm_ts <- lmer(
  air_movement_acceptability_ankles ~ thermal_sensation + (1 | subject_id),
  data = survey_analysis,
  REML = TRUE
)

lmm_ta <- lmer(
  air_movement_acceptability_ankles ~ thermal_sensation_ankles + (1 | subject_id),
  data = survey_analysis,
  REML = TRUE
)

pred_v_air <- expand_grid(
  v_air_s = seq(min(survey_analysis$v_air_s, na.rm = TRUE),
                max(survey_analysis$v_air_s, na.rm = TRUE),
                length.out = 100),
  subject_id = NA
) %>%
  mutate(
    pred = predict(lmm_v_air, newdata = ., re.form = NA),
    se = predict(lmm_v_air, newdata = ., re.form = NA, se.fit = TRUE)$se.fit,
    lower = pred - 1.96 * se,
    upper = pred + 1.96 * se
  )

pred_ts <- expand_grid(
  thermal_sensation = seq(min(survey_analysis$thermal_sensation, na.rm = TRUE),
                         max(survey_analysis$thermal_sensation, na.rm = TRUE),
                         length.out = 100),
  subject_id = NA
) %>%
  mutate(
    pred = predict(lmm_ts, newdata = ., re.form = NA),
    se = predict(lmm_ts, newdata = ., re.form = NA, se.fit = TRUE)$se.fit,
    lower = pred - 1.96 * se,
    upper = pred + 1.96 * se
  )

pred_ta <- expand_grid(
  thermal_sensation_ankles = seq(min(survey_analysis$thermal_sensation_ankles, na.rm = TRUE),
                                max(survey_analysis$thermal_sensation_ankles, na.rm = TRUE),
                                length.out = 100),
  subject_id = NA
) %>%
  mutate(
    pred = predict(lmm_ta, newdata = ., re.form = NA),
    se = predict(lmm_ta, newdata = ., re.form = NA, se.fit = TRUE)$se.fit,
    lower = pred - 1.96 * se,
    upper = pred + 1.96 * se
  )

p1 <- ggplot(linear_df %>% filter(predictor == "Ankle air speed (m/s)"),
             aes(x = x, y = air_movement_acceptability_ankles)) +
  geom_point(alpha = 0.25, size = 1.5) +
  geom_ribbon(data = pred_v_air, aes(x = v_air_s, ymin = lower, ymax = upper, y = NULL),
              alpha = 0.2, fill = "blue", color = NA, inherit.aes = FALSE) +
  geom_line(data = pred_v_air, aes(x = v_air_s, y = pred),
            color = "blue", linewidth = 0.9, inherit.aes = FALSE) +
  labs(
    x = "Ankle air speed (m/s)",
    y = "Air movement acceptability (ankles)"
  ) +
  theme_classic(base_size = 12)

p2 <- ggplot(linear_df %>% filter(predictor == "Whole-body thermal sensation"),
             aes(x = x, y = air_movement_acceptability_ankles)) +
  geom_point(alpha = 0.25, size = 1.5) +
  geom_ribbon(data = pred_ts, aes(x = thermal_sensation, ymin = lower, ymax = upper, y = NULL),
              alpha = 0.2, fill = "blue", color = NA, inherit.aes = FALSE) +
  geom_line(data = pred_ts, aes(x = thermal_sensation, y = pred),
            color = "blue", linewidth = 0.9, inherit.aes = FALSE) +
  labs(
    x = "Whole-body thermal sensation",
    y = "Air movement acceptability (ankles)"
  ) +
  theme_classic(base_size = 12)

p3 <- ggplot(linear_df %>% filter(predictor == "Ankle thermal sensation"),
             aes(x = x, y = air_movement_acceptability_ankles)) +
  geom_point(alpha = 0.25, size = 1.5) +
  geom_ribbon(data = pred_ta, aes(x = thermal_sensation_ankles, ymin = lower, ymax = upper, y = NULL),
              alpha = 0.2, fill = "blue", color = NA, inherit.aes = FALSE) +
  geom_line(data = pred_ta, aes(x = thermal_sensation_ankles, y = pred),
            color = "blue", linewidth = 0.9, inherit.aes = FALSE) +
  labs(
    x = "Ankle thermal sensation",
    y = "Air movement acceptability (ankles)"
  ) +
  theme_classic(base_size = 12)

patchwork::wrap_plots(p1, p2, p3, nrow = 1)

# ============================================================================
# Part2：Group comparisons for subjective responses (paired t-test and Friedman test)
# ============================================================================
# Paired t-test with effect size and p-value annotation--------------
# Overall Thermal Sensation Vote (TSV)
TSV_test <- paired_comparison(survey_analysis, "thermal_sensation", test_type = "t.test")
plot_paired_comparison(survey_analysis, "thermal_sensation", TSV_test,
                      y_label = "Thermal sensation")

# Ankle Local Thermal Sensation Vote (A_TSV)
A_TSV_test <- paired_comparison(survey_analysis, "thermal_sensation_ankles", 
                               test_type = "t.test", p.adjust.method = "holm")
plot_paired_comparison(survey_analysis, "thermal_sensation_ankles", A_TSV_test,
                      y_label = "Ankle Thermal sensation")

# Thermal Comfort Vote (TCV)
TCV_test <- paired_comparison(survey_analysis, "thermal_comfort", 
                             test_type = "t.test", p.adjust.method = "holm")
plot_paired_comparison(survey_analysis, "thermal_comfort", TCV_test,
                      y_label = "Thermal comfort")

# Friedman test and wilcoxon post-hoc test with effect size and p-value annotation------
# Overall Thermal Preference
TP_result <- friedman_posthoc(survey_analysis, "thermal_preference")
TP_friedman_res <- TP_result$friedman
TP_posthoc <- TP_result$posthoc

plot_distribution_with_posthoc(
  survey_analysis, 
  "thermal_preference", 
  TP_posthoc,
  fill_label = "Thermal preference"
)
# Ankle Thermal Preference
A_TP_result <- friedman_posthoc(survey_analysis, "thermal_preference_ankles")
A_TP_friedman_res <- A_TP_result$friedman
A_TP_posthoc <- A_TP_result$posthoc

plot_distribution_with_posthoc(
  survey_analysis, 
  "thermal_preference_ankles", 
  A_TP_posthoc,
  fill_label = "Thermal preference (ankles)"
)

#Clothing Changing Behavior
Clothing_result <- friedman_posthoc(survey_analysis, "clothing_change")
Clothing_friedman_res <- Clothing_result$friedman
Clothing_posthoc <- Clothing_result$posthoc

plot_distribution_with_posthoc(
  survey_analysis, 
  "clothing_change", 
  Clothing_posthoc,
  fill_label = "Changed clothing?"
)

#Disacceptability with Draft Ankles
A_Disaccept_result <- friedman_posthoc(survey_analysis, "disacceptability_with_draft_ankles")
A_Disaccept_friedman_res <- A_Disaccept_result$friedman
A_Disaccept_posthoc <- A_Disaccept_result$posthoc

plot_distribution_with_posthoc(
  survey_analysis,
  "disacceptability_with_draft_ankles",
  A_Disaccept_posthoc,
  fill_label = "Disacceptability (ankles)"
)

#Air Movement Preference Ankles
A_AMP_result <- friedman_posthoc(survey_analysis, "air_movement_preference_ankles")
A_AMP_friedman_res <- A_AMP_result$friedman
A_AMP_posthoc <- A_AMP_result$posthoc

plot_distribution_with_posthoc(
  survey_analysis,
  "air_movement_preference_ankles",
  A_AMP_posthoc,
  fill_label = "Air movement preference (ankles)"
)

# ============================================================================
# Part3：Logistic Mixed Model for Draft Ankle Discomfort Risk
# ============================================================================

# Fit: Random intercept GLMM
m_glmm <- glmer(
  #dissatisfied_with_draft_ankles ~ v_air_s +t_air_c+gender+ thermal_sensation + (1 | subject_id),
  dissatisfied_with_draft_ankles ~ v_air_s + thermal_sensation + (1 | subject_id),
  data = survey_analysis,
  family = binomial(link = "logit")
)
summary(m_glmm)

# Fixed Effects Coefficients (Population-level effects)
fe <- fixef(m_glmm)
a <- unname(fe["(Intercept)"])
b <- unname(fe["v_air_s"])
c <- unname(fe["thermal_sensation"])

# Create prediction grid
grid <- expand_grid(
  TS = seq(-3, 3, length.out = 300),
  V  = seq(min(survey_analysis$v_air_s, na.rm = TRUE),
           1,
           length.out = 300)
) %>%
  mutate(
    eta = a + b * V + c * TS
  )

# Marginal probability: Integrate over random intercept distribution (Monte Carlo)
sd_b0 <- sqrt(as.numeric(VarCorr(m_glmm)$subject_id[1, 1]))
set.seed(1)
K <- 3000
b0_draw <- rnorm(K, mean = 0, sd = sd_b0)
grid <- grid %>%
  mutate(
    PPD_marg = vapply(
      eta,
      function(e) mean(plogis(e + b0_draw)) * 100,
      numeric(1)
    )
  )

# Plot: Marginal probability and "comfort zone"
#    Condition: abs(TS) < 0.5 AND PPD < 20
ppd_levels <- c(10, 20, 30, 40, 50, 60, 70, 80)

p_marg <- ggplot() +
  geom_raster(
    data = filter(grid, abs(TS) < 0.5 & PPD_marg < 20),
    aes(TS, V),
    fill = "mediumpurple3",
    alpha = 0.75
  ) +
  geom_contour(
    data = grid,
    aes(TS, V, z = PPD_marg),
    breaks = ppd_levels,
    color = "darkgreen",
    linewidth = 1
  ) +
  scale_y_continuous(
    name = "Ankle air speed (m/s)",
    sec.axis = sec_axis(~ . * 196.85, name = "Ankle air speed (fpm)")
  ) +
  scale_x_continuous(
    name = "Whole-body thermal sensation (~PMV)",
    breaks = -3:3,
    labels = c("Cold","Cool","Slightly\ncool","Neutral","Slightly\nwarm","Warm","Hot")
  ) +
  coord_cartesian(xlim = c(-3, 3), expand = FALSE) +
  theme_classic(base_size = 13)

p_marg

# ============================================================================
# Part4：Skin temperature
# ============================================================================
# Individual
tsk %>%
  filter(
    subject_id == "rz021", # Select an individual
    tsk_sensing_location %in% c("A", "T")
  ) %>%
  mutate(
    session_type = factor(session_type),
    workstation = factor(workstation),
    workstation = factor(workstation),
    tsk_sensing_location = factor(tsk_sensing_location, levels = c("A", "T"))
  ) %>%
  ggplot(
    aes(x = running_time_s, y = tsk_c, color = tsk_sensing_location)
  ) +
  geom_line(linewidth = 0.6, na.rm = TRUE) +
  facet_grid(
    session_type ~ workstation,
    scales = "free_x",
    labeller = label_both
  ) +
  labs(
    x = "Running time (s)",
    y = "Skin temperature (°C)",
    color = "Location"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "grey90", color = NA)
  )

# Group Average
tsk %>%
  filter(
    !is.na(session_type),
    !is.na(workstation),
    tsk_sensing_location %in% c("A", "T"),
    !is.na(running_time_s),
    !is.na(tsk_c),
    running_time_s <= 1190
  ) %>%
  mutate(
    session_type = factor(session_type),
    workstation  = factor(workstation),
    tsk_sensing_location = factor(tsk_sensing_location, levels = c("A", "T"))
  ) %>%
  group_by(session_type, workstation, tsk_sensing_location, running_time_s) %>%
  summarise(
    n = n(),
    mean_tsk = mean(tsk_c, na.rm = TRUE),
    sd_tsk   = sd(tsk_c, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = running_time_s, y = mean_tsk, color = tsk_sensing_location)) +
  geom_ribbon(
    aes(ymin = mean_tsk - sd_tsk, ymax = mean_tsk + sd_tsk, fill = tsk_sensing_location),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 0.6, na.rm = TRUE) +
  facet_grid(
    session_type ~ workstation,
    scales = "free_x",
    labeller = label_both
  ) +
  labs(
    x = "Running time (s)",
    y = "Skin temperature (°C)",
    color = "Location",
    fill  = "Location"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "grey90", color = NA)
  )


# ============================================================================
# COMBINED FRIEDMAN & POST-HOC SUMMARY TABLE
# ============================================================================

# Pre-compute group means and sample sizes for each outcome variable
group_stats <- survey_analysis %>%
  pivot_longer(
    cols = c(thermal_sensation, thermal_sensation_ankles, thermal_comfort),
    names_to = "outcome_var",
    values_to = "value"
  ) %>%
  group_by(session_diffusor_sat, workstation, outcome_var) %>%
  summarise(
    group_mean = mean(value, na.rm = TRUE),
    group_n = n_distinct(subject_id),
    .groups = 'drop'
  )

# Apply to each test
TSV_with_stats <- add_group_stats(TSV_test, "thermal_sensation", group_stats)
A_TSV_with_stats <- add_group_stats(A_TSV_test, "thermal_sensation_ankles", group_stats)
TCV_with_stats <- add_group_stats(TCV_test, "thermal_comfort", group_stats)

stat_summary <- bind_rows(
  TSV_with_stats %>% mutate(Analysis = "Thermal Sensation Vote"),
  A_TSV_with_stats %>% mutate(Analysis = "Ankle Thermal Sensation Vote"),
  TCV_with_stats %>% mutate(Analysis = "Thermal Comfort Vote")
) %>%
  mutate(
    Test_Type = "Paired t-test",
    Comparison = paste(group1, "vs", group2),
    Temperature_C = as.character(session_diffusor_sat),
    # N is the minimum of the two groups (for paired comparison)
    N = pmin(group1_n, group2_n),
    Mean_Group1 = round(group1_mean, 3),
    Mean_Group2 = round(group2_mean, 3),
    P_Value = round(p.adj, 4),
    Effect_Size = round(effsize, 3),
    Significant = if_else(P_Value < 0.05, "Yes", "No")
  ) %>%
  select(Analysis, Test_Type, Temperature_C, Comparison, Mean_Group1, Mean_Group2, 
         N, P_Value, Effect_Size, Significant)

# Friedman test summary with post-hoc Wilcox comparisons
friedman_summary <- bind_rows(
  # Thermal Preference
  TP_friedman_res %>% 
    mutate(
      Analysis = "Thermal Preference",
      session_diffusor_sat = as.character(session_diffusor_sat)
    ) %>%
    select(Analysis, session_diffusor_sat, statistic, df, p),
  
  # Ankle Thermal Preference
  A_TP_friedman_res %>% 
    mutate(
      Analysis = "Thermal Preference (Ankles)",
      session_diffusor_sat = as.character(session_diffusor_sat)
    ) %>%
    select(Analysis, session_diffusor_sat, statistic, df, p),
  
  # Clothing Change
  Clothing_friedman_res %>% 
    mutate(
      Analysis = "Clothing Change",
      session_diffusor_sat = as.character(session_diffusor_sat)
    ) %>%
    select(Analysis, session_diffusor_sat, statistic, df, p),
  
  # Ankle Disacceptability
  A_Disaccept_friedman_res %>% 
    mutate(
      Analysis = "Ankle Disacceptability",
      session_diffusor_sat = as.character(session_diffusor_sat)
    ) %>%
    select(Analysis, session_diffusor_sat, statistic, df, p),
  
  # Air Movement Preference
  A_AMP_friedman_res %>% 
    mutate(
      Analysis = "Air Movement Preference (Ankles)",
      session_diffusor_sat = as.character(session_diffusor_sat)
    ) %>%
    select(Analysis, session_diffusor_sat, statistic, df, p)
) %>%
  rename(
    Temperature_or_Session = session_diffusor_sat,
    Friedman_Statistic = statistic,
    Friedman_p = p
  ) %>%
  mutate(
    Friedman_p = round(Friedman_p, 4),
    Friedman_Sig = if_else(Friedman_p < 0.05, "Yes", "No")
  ) %>%
  select(Analysis, Temperature_or_Session, df, Friedman_Statistic, Friedman_p, Friedman_Sig)

# Post-hoc Wilcox summary - use results from friedman_posthoc() function calls
# compute sample sizes for categorical outcomes
categorical_group_stats <- survey_analysis %>%
  pivot_longer(
    cols = c(thermal_preference, thermal_preference_ankles, clothing_change,
             disacceptability_with_draft_ankles, air_movement_preference_ankles),
    names_to = "outcome_var",
    values_to = "value"
  ) %>%
  group_by(session_diffusor_sat, workstation, outcome_var) %>%
  summarise(
    group_n = n_distinct(subject_id),
    .groups = 'drop'
  )

posthoc_summary <- bind_rows(
  TP_posthoc %>% 
    mutate(Analysis = "Thermal Preference", 
           outcome_var = "thermal_preference",
           .after = session_diffusor_sat),
  A_TP_posthoc %>% 
    mutate(Analysis = "Thermal Preference (Ankles)",
           outcome_var = "thermal_preference_ankles",
           .after = session_diffusor_sat),
  Clothing_posthoc %>% 
    mutate(Analysis = "Clothing Change",
           outcome_var = "clothing_change",
           .after = session_diffusor_sat),
  A_Disaccept_posthoc %>% 
    mutate(Analysis = "Ankle Disacceptability",
           outcome_var = "disacceptability_with_draft_ankles",
           .after = session_diffusor_sat),
  A_AMP_posthoc %>% 
    mutate(Analysis = "Air Movement Preference",
           outcome_var = "air_movement_preference_ankles",
           .after = session_diffusor_sat)
) %>%
  # Add sample sizes for group1 and group2
  left_join(
    categorical_group_stats %>%
      rename(temp = session_diffusor_sat, group1_n = group_n),
    by = c("session_diffusor_sat" = "temp", "group1" = "workstation", "outcome_var")
  ) %>%
  left_join(
    categorical_group_stats %>%
      rename(temp = session_diffusor_sat, group2_n = group_n),
    by = c("session_diffusor_sat" = "temp", "group2" = "workstation", "outcome_var")
  ) %>%
  # Add Friedman p-values by joining with friedman results
  left_join(
    bind_rows(
      TP_friedman_res %>% select(session_diffusor_sat, p) %>% mutate(Analysis = "Thermal Preference"),
      A_TP_friedman_res %>% select(session_diffusor_sat, p) %>% mutate(Analysis = "Thermal Preference (Ankles)"),
      Clothing_friedman_res %>% select(session_diffusor_sat, p) %>% mutate(Analysis = "Clothing Change"),
      A_Disaccept_friedman_res %>% select(session_diffusor_sat, p) %>% mutate(Analysis = "Ankle Disacceptability"),
      A_AMP_friedman_res %>% select(session_diffusor_sat, p) %>% mutate(Analysis = "Air Movement Preference")
    ) %>% rename(friedman_p = p),
    by = c("session_diffusor_sat", "Analysis")
  ) %>%
  mutate(
    Test_Type = "Wilcox post-hoc",
    Comparison = paste0(group1, " vs ", group2),
    N = pmin(group1_n, group2_n, na.rm = TRUE),
    Wilcox_p = round(p.adj, 4),
    Effect_Size = round(effsize, 3),
    Friedman_p = round(friedman_p, 4),
    Significant = if_else(p.adj < 0.05, "Yes", "No")
  ) %>%
  select(Analysis, Test_Type, session_diffusor_sat, Comparison, N, Friedman_p, Wilcox_p, Effect_Size, Significant) %>%
  rename(Temperature_or_Session = session_diffusor_sat) %>%
  arrange(Analysis, Temperature_or_Session)

# Save summary tables
write_csv(stat_summary, here::here("outputs", "paired_tests_summary.csv"))
write_csv(friedman_summary, here::here("outputs", "friedman_tests_summary.csv"))
write_csv(posthoc_summary, here::here("outputs", "friedman_posthoc_wilcox_summary.csv"))


#write.xlsx(survey_analysis, file = "Ankle_Draft2_analysis_data.xlsx")

# Clean up intermediate variables
rm(
  TP_result, TP_friedman_res, TP_posthoc,
  A_TP_result, A_TP_friedman_res, A_TP_posthoc,
  Clothing_result, Clothing_friedman_res, Clothing_posthoc,
  A_Disaccept_result, A_Disaccept_friedman_res, A_Disaccept_posthoc,
  A_AMP_result, A_AMP_friedman_res, A_AMP_posthoc,
  TSV_test, A_TSV_test, TCV_test, TSV_with_stats, A_TSV_with_stats, TCV_with_stats,
  m_glmm, fe, a, b, c, sd_b0, K, b0_draw, grid, p_marg,
  model_v_air, model_thermal_sens, model_ankle_sens,
  lmm_v_air, lmm_ts, lmm_ta,
  p1, p2, p3, p,
  linear_df, group_stats, categorical_group_stats, excluded_subjects,
  pred_v_air, pred_ts, pred_ta
)