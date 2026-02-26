# Title: Main Analysis
# Description: Statistical analysis of survey data and development of the PPD with ankle draft model
# Author: Toby Kramer
# Date: 2026-01-05


# ---- Setup ----

source(here::here("src", "R", "x_setup.R"))
source(here::here("src", "R", "x_func.R"))
source(here::here("src", "R", "x_data.R"))
source(here::here("src", "R", "x_stat.R"))


# Create clean analysis dataset

survey_analysis <- survey_processed 

# IMPORTANT: Ensure consistent pairing order for subsequent paired
# Wilcoxon signed-rank tests and paired t-tests
# REASON: pairwise_wilcox_test() does not automatically reorder observations
# based on an index column; pairing must be aligned manually.

survey_analysis <- survey_analysis %>%
  arrange(session_diffusor_sat, subject_id, workstation)

# ---- Part1：Linear relationship/Logistic relationship for Air Movement Acceptability --------
# This section is intended for comparison with Liu’s work.

# Because the dataset contains repeated measurements from the same subjects,
# Linear or logistic mixed-effects models are used,
# with subject-specific random intercepts to account for inter-individual variability.

# Notes:
# 1. Repeated measures prevent the use of simple linear regression due to
#    non-independence; mixed-effects models are adopted to properly handle
#    within-subject correlation.
# 2. This analysis is designed to enable comparison with Liu’s study.
# 3. Personally, I do not support using air speed as a continuous predictor
#    in linear regression, because in this study it is treated as a soft
#    control categorical variable rather than a true continuous exposure.

# Linear Mixed Model

m1 <- lmer(air_movement_acceptability_ankles ~ v_air_s + (1|subject_id), data=survey_analysis)
m2 <- lmer(air_movement_acceptability_ankles ~ thermal_sensation + (1|subject_id), data=survey_analysis)
m3 <- lmer(air_movement_acceptability_ankles ~ thermal_sensation_ankles + (1|subject_id), data=survey_analysis)
  # Visualization
p1 <- plot_lmm(survey_analysis, "v_air_s", "Ankle air speed (m/s)", m1)
p2 <- plot_lmm(survey_analysis, "thermal_sensation", "Whole-body thermal sensation", m2)
p3 <- plot_lmm(survey_analysis, "thermal_sensation_ankles", "Ankle thermal sensation", m3)
wrap_plots(p1, p2, p3, nrow=1)
summary(m2)

# Logistic Mixed Model
ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
logistic_m1 <- glmer(disacceptability_with_draft_ankles ~ v_air_s + (1 | subject_id),
  data = survey_analysis,family = binomial, control = ctrl)
logistic_m2 <- glmer(disacceptability_with_draft_ankles ~ thermal_sensation + (1 | subject_id),
  data = survey_analysis,family = binomial, control = ctrl)
logistic_m3 <- glmer(disacceptability_with_draft_ankles ~ thermal_sensation_ankles + (1 | subject_id),
  data = survey_analysis,family = binomial, control = ctrl)
  # Visualization
logistic_p1 <- plot_prob(logistic_m1, "v_air_s [all]", "Ankle air speed (m/s)")
logistic_p2 <- plot_prob(logistic_m2, "thermal_sensation [all]", "Whole-body thermal sensation")
logistic_p3 <- plot_prob(logistic_m3, "thermal_sensation_ankles [all]","Ankle thermal sensation")
wrap_plots(logistic_p1, logistic_p2, logistic_p3, nrow=1)


# ---- Part2：Group comparisons for subjective responses (paired t-test and wilcox test) --------

# Paired t-test for continuous variables (within-subject comparison) -----------------
# Notes:
# 1. Only for the three continuous voting variables: TSV, TCV, and ATSV.
# 2. p-values are adjusted using the Benjamini–Hochberg (BH) procedure.
# 3. The visualization function automatically annotates the adjusted p-values
#    (P.adj) and the corresponding effect size (Cohen’s d) in the figures.

# Overall Thermal Sensation Vote (TSV)
TSV_res <- pairedttest_with_anova(survey_analysis, "thermal_sensation",
                                  group_by_var = "session_diffusor_sat",
                                  within_var = "workstation",)
TSV_aov  <- TSV_res$anova
TSV_test <- TSV_res$pairwise
plot_pairedttest(survey_analysis, "thermal_sensation", TSV_test,
                       y_label = "Thermal sensation")

# Ankle Local Thermal Sensation Vote (A_TSV)
A_TSV_res <- pairedttest_with_anova(survey_analysis, "thermal_sensation_ankles")
A_TSV_aov  <- A_TSV_res$anova
A_TSV_test <- A_TSV_res$pairwise
plot_pairedttest(survey_analysis, "thermal_sensation_ankles", A_TSV_test,
                       y_label = "Ankle Thermal sensation")

# Thermal Comfort Vote (TCV)
TCV_res <- pairedttest_with_anova(survey_analysis, "thermal_comfort")
TCV_aov  <- TCV_res$anova
TCV_test <- TCV_res$pairwise
plot_pairedttest(survey_analysis, "thermal_comfort", TCV_test,
                       y_label = "Thermal comfort")


# Friedman test and Post-hoc pairwise Wilcoxon signed-rank tests ordinal variables-------
# Notes:
# 1. Only for the ordinal variables.
# 2. p-values are adjusted using the Benjamini–Hochberg (BH) procedure.
# 3. The visualization function automatically annotates the adjusted p-values
#    (P.adj) and the corresponding effect size (r) in the figures.

# Overall Thermal Preference
TP_result <- pairedwilcoxtest(survey_analysis, "thermal_preference")
TP_friedman_res <- TP_result$friedman
TP_posthoc <- TP_result$posthoc
plot_pairedwilcox(
  survey_analysis, 
  "thermal_preference", 
  TP_posthoc,
  fill_label = "Thermal preference"
)

# Ankle Thermal Preference
A_TP_result <- pairedwilcoxtest(survey_analysis, "thermal_preference_ankles")
A_TP_friedman_res <- A_TP_result$friedman
A_TP_posthoc <- A_TP_result$posthoc
plot_pairedwilcox(
  survey_analysis, 
  "thermal_preference_ankles", 
  A_TP_posthoc,
  fill_label = "Thermal preference (ankles)"
)

# Clothing Behavior
Clothing_result <- pairedwilcoxtest(survey_analysis, "clothing_change")
Clothing_friedman_res <- Clothing_result$friedman
Clothing_posthoc <- Clothing_result$posthoc
plot_pairedwilcox(
  survey_analysis, 
  "clothing_change", 
  Clothing_posthoc,
  fill_label = "Changed clothing?"
)

# Disacceptability with Draft Ankles
A_Disaccept_result <- pairedwilcoxtest(survey_analysis, "disacceptability_with_draft_ankles")
A_Disaccept_friedman_res <- A_Disaccept_result$friedman
A_Disaccept_posthoc <- A_Disaccept_result$posthoc
plot_pairedwilcox(
  survey_analysis,
  "disacceptability_with_draft_ankles",
  A_Disaccept_posthoc,
  fill_label = "Disacceptability (ankles)"
)

# Air Movement Preference Ankles
A_AMP_result <- pairedwilcoxtest(survey_analysis, "air_movement_preference_ankles")
A_AMP_friedman_res <- A_AMP_result$friedman
A_AMP_posthoc <- A_AMP_result$posthoc
plot_pairedwilcox(
  survey_analysis,
  "air_movement_preference_ankles",
  A_AMP_posthoc,
  fill_label = "Air movement preference (ankles)"
)


# ---- Part3：Logistic Mixed Model for PPD with ankle draft -----------------
# Notes:
# 1. The model specification follows the same input structure as Liu’s study.
# 2. A derived binary variable, `dissatisfied_with_draft_ankles`, is aimed.
#    Logistic regression is applied to model the probability of reporting dissatisfaction.
# 3. Due to the repeated-measurement, the assumption of independent observations
#    is violated. Therefore, a generalized linear mixed-effects model (GLMM) is used,
#    with a logit link function and subject-specific random intercepts to account
#    for inter-individual variability.

m_glmm <- glmer(
  dissatisfied_with_draft_ankles ~ v_air_s + t_air_c + (1 | subject_id), 
  # Optionally explore alternative predictors,as sat does not show statistical significance
  #dissatisfied_with_draft_ankles ~ v_air_s + thermal_sensation + (1 | subject_id),
  data = survey_analysis,
  family = binomial(link = "logit")
)
summary(m_glmm)

# IMPORTANT: 
# Compute the population-average probability of draft dissatisfaction
# using marginal (population-level) predicted percentage of dissatisfied with ankle draft.
# ----------------------------------------------------------
# Reason for computing marginal (population-averaged) probability
#
# In logistic mixed-effects models:
#   logit(p_ij) = β_0 + β_1*x_ij + u_0j
#   u_0j ~ N(0, σ^2)
# The inverse-logit of the fixed-effects part alone
#   inv_logit(β_0 + β_1*x_ij)
# corresponds to the probability conditional on u_0j = 0.
#
# This represents the probability for a "typical subject"
# whose random effect equals zero — NOT the population-average probability.
#
# Because the logit link is nonlinear:
#   E[inv_logit(β_0 + β_1*x_ij + u_0j)] ≠ inv_logit(β_0 + β_1*x_ij)
#
# Therefore, to obtain the population-level average probability,
# we must integrate over the distribution of the random effects:
#
#   p_marginal = E_u[inv_logit(β_0 + β_1*x_ij + u_0j)]
#
# This expectation has no closed-form solution, so we approximate
# it using Monte Carlo sampling from u_0j ~ N(0, σ^2).
#
# The resulting value represents the true average dissatisfied risk across
# the entire population.
# ----------------------------------------------------------

# Extract fixed-effects coefficients from the fitted mixed-effects model.
fe <- fixef(m_glmm)
a <- unname(fe["(Intercept)"])
b <- unname(fe["v_air_s"])
c <- unname(fe["thermal_sensation"])

# Create prediction grid
grid <- expand_grid(
  TS = seq(-3, 3, length.out = 300),
  V  = seq(0,1,length.out = 300)
) %>%
  mutate(
    eta = a + b * V + c * TS # log-odds based on the fixed-effects component only
  )

# Use a Monte Carlo approach to sample from the estimated random-intercept distribution, 
# generating 10,000 simulated individuals with different baseline levels.

  # Extract estimated variance of the random intercept
sd_b0 <- sqrt(as.numeric(VarCorr(m_glmm)$subject_id[1, 1]))
set.seed(1)
  # Number of Monte Carlo draws
K <- 10000
  # Draw K random intercepts from N(0, σ^2)
b0_draw <- rnorm(K, mean = 0, sd = sd_b0)

  # For each fixed-effects linear predictor η:
  # 1) Add sampled random intercepts
  # 2) Transform via inverse-logit
  # 3) Average across draws → marginal probability
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


# ---- Part 4: Export statistical test results to CSV file --------
# Repeated ANOVA test summary 
RepeatedANOVA_summary <- bind_rows(
  # Overall Thermal Sensation
  TSV_aov      %>% dplyr::mutate(outcome = "TSV"),
  # Ankle Thermal Sensation
  A_TSV_aov    %>% dplyr::mutate(outcome = "A_TSV"),
  # Thermal Comfort
  TCV_aov      %>% dplyr::mutate(outcome = "TCV")
)

# Paired t-test summary 
pairedttest_summary <- dplyr::bind_rows(
  # Overall Thermal Sensation
  TSV_test      %>% dplyr::mutate(outcome = "TSV"),
  # Ankle Thermal Sensation
  A_TSV_test    %>% dplyr::mutate(outcome = "A_TSV"),
  # Thermal Comfort
  TCV_test      %>% dplyr::mutate(outcome = "TCV")
)

# Friedman test summary
friedman_summary <- bind_rows(
  # Thermal Preference
  TP_friedman_res          %>% dplyr::mutate(outcome = "TP"),
  # Ankle Thermal Preference
  A_TP_friedman_res        %>% dplyr::mutate(outcome = "A_TP"),
  # Clothing Behavior
  Clothing_friedman_res    %>% dplyr::mutate(outcome = "Clothing"),
  # Disacceptability with Ankles Draft
  A_Disaccept_friedman_res %>% dplyr::mutate(outcome = "A_Disaccept"),
  # Ankle Air Movement Preference
  A_AMP_friedman_res       %>% dplyr::mutate(outcome = "A_AMP")
)

# Post-hoc Wilcox comparisons
wilcox_posthoc_summary <- bind_rows(
  # Thermal Preference
  TP_posthoc               %>% dplyr::mutate(outcome = "TP"),
  # Ankle Thermal Preference
  A_TP_posthoc             %>% dplyr::mutate(outcome = "A_TP"),
  # Clothing Behavior
  Clothing_posthoc         %>% dplyr::mutate(outcome = "Clothing"),
  # Disacceptability with Ankles Draft
  A_Disaccept_posthoc      %>% dplyr::mutate(outcome = "A_Disaccept"),
  # Ankle Air Movement Preference
  A_AMP_posthoc            %>% dplyr::mutate(outcome = "A_AMP")
) 

# Save summary tables
write_csv(RepeatedANOVA_summary, here::here("data", "02-export","RepeatedANOVA_tests_summary.csv"))
write_csv(pairedttest_summary, here::here("data", "02-export","paired_tests_summary.csv"))
write_csv(friedman_summary, here::here("data", "02-export", "friedman_tests_summary.csv"))
write_csv(wilcox_posthoc_summary, here::here("data", "02-export","friedman_posthoc_wilcox_summary.csv"))
write_csv(survey_analysis, here::here("data", "02-export","Ankle_Draft2_survey_analysis_data.csv"))

# Clean up intermediate variables
rm(
  TP_result, TP_friedman_res, TP_posthoc,
  A_TP_result, A_TP_friedman_res, A_TP_posthoc,
  Clothing_result, Clothing_friedman_res, Clothing_posthoc,
  A_Disaccept_result, A_Disaccept_friedman_res, A_Disaccept_posthoc,
  A_AMP_result, A_AMP_friedman_res, A_AMP_posthoc,
  TSV_test, A_TSV_test, TCV_test,
  m_glmm, fe, a, b, c, sd_b0, K, b0_draw, grid, p_marg,
  p1, p2, p3, m1, m2, m3, logistic_m1, logistic_m2, logistic_m3, TCV_aov, TCV_res,
  TSV_res, TSV_aov, A_TSV_res,A_TSV_aov, ctrl
)