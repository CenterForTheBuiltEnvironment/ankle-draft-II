# Title: Statistics
# Description: Reusable functions for statistical analysis
# Author: Toby Kramer
# Date: 2026-01-05

# ---- Setup ----
source(here::here("src", "R", "x_setup.R"))
source(here::here("src", "R", "x_data.R"))


# Statistics Functions ----------------------------------------------------

# Function 1: Extract Odds Ratio (OR) and 95% Confidence Interval  --------
# From a fitted logistic (mixed-effects) model
# Returns:
#     - term     : predictor name
#     - OR       : odds ratio (exp of coefficient)
#     - CI_low   : lower bound of 95% CI for OR
#     - CI_high  : upper bound of 95% CI for OR
logistic_or_ci <- function(mod, term){
  b  <- fixef(mod)[term]
  se <- sqrt(vcov(mod)[term, term])
  tibble(
    term = term,
    OR = exp(b),
    CI_low = exp(b - 1.96 * se),
    CI_high = exp(b + 1.96 * se)
  )
}

# Function 2: Repeated-measures ANOVA and Paired t-tests for continuous variables ----
# 1. Use paired-sample t-tests to compare air-speed levels pairwise
#    (within the same subjects).
# 2. Because multiple pairwise comparisons are conducted, 
#    adjust p-values using the Benjamini–Hochberg (BH)
#    procedure to control the false discovery rate.

pairedttest_with_anova <- function(data, outcome_var,
                                   p.adjust.method = "BH",
                                   group_by_var = "session_diffusor_sat",
                                   within_var = "workstation",
                                   subject_var = "subject_id",
                                   alpha = 0.05,
                                   pairwise_only_if_sig = FALSE) {
  
  group_sym   <- rlang::sym(group_by_var)
  within_sym  <- rlang::sym(within_var)
  subject_sym <- rlang::sym(subject_var)
  
  # Repeated-measures ANOVA (within each supply air temperature)
  aov_res <- data %>%
    dplyr::group_by(!!group_sym) %>%
    rstatix::anova_test(
      dv = !!rlang::sym(outcome_var),
      wid = !!subject_sym,
      within = !!within_sym
    ) %>%
    rstatix::get_anova_table() %>%
    dplyr::ungroup()
  
  # Identify groups with significant main effect
  p_col <- "p"
  
  sig_groups <- aov_res %>%
    dplyr::filter(.data[[p_col]] < alpha) %>%
    dplyr::pull(!!group_sym) %>%
    unique()
  
  # Decide which data go into pairwise step
  data_for_pairwise <- if (pairwise_only_if_sig) {
    data %>% dplyr::filter(!!group_sym %in% sig_groups)
  } else {
    data
  }
  
  # If nothing is significant and pairwise_only_if_sig=TRUE, return early
  if (nrow(data_for_pairwise) == 0) {
    return(list(
      anova = aov_res,
      pairwise = dplyr::tibble()
    ))
  }
  
  # Pairwise paired t-tests
  test_result <- data_for_pairwise %>%
    dplyr::group_by(!!group_sym) %>%
    rstatix::pairwise_t_test(
      as.formula(paste(outcome_var, "~", within_var)),
      paired = TRUE,
      p.adjust.method = p.adjust.method
    )
  
  # Paired Cohen’s d
  effect_result <- data_for_pairwise %>%
    dplyr::group_by(!!group_sym) %>%
    rstatix::cohens_d(
      as.formula(paste(outcome_var, "~", within_var)),
      paired = TRUE
    )
  
  # Group means
  mean_df <- data_for_pairwise %>%
    dplyr::group_by(!!group_sym, !!within_sym) %>%
    dplyr::summarise(
      mean_value = mean(.data[[outcome_var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Attach means to group1 / group2
  test_result <- test_result %>%
    dplyr::left_join(mean_df, by = c(group_by_var, "group1" = within_var)) %>%
    dplyr::rename(mean_group1 = mean_value) %>%
    dplyr::left_join(mean_df, by = c(group_by_var, "group2" = within_var)) %>%
    dplyr::rename(mean_group2 = mean_value)
  
  # Merge + add plotting positions
  by_cols <- c(group_by_var, "group1", "group2")
  pairwise_res <- test_result %>%
    dplyr::left_join(effect_result, by = by_cols) %>%
    rstatix::add_xy_position(x = within_var)
  
  list(
    anova = aov_res,
    pairwise = pairwise_res
  )
}

# Function 3: Friedman and Paired wilcox tests for ordinal variables ----
# 1. Use paired wilcox tests to compare air-speed levels pairwise
#    (within the same subjects).
# 2. Because multiple pairwise comparisons are conducted, 
#    adjust p-values using the Benjamini–Hochberg (BH)
#    procedure to control the false discovery rate.
pairedwilcoxtest <- function(data, outcome_var, 
                             group_by_var = "session_diffusor_sat") {
  outcome_sym <- sym(outcome_var)
  group_sym <- sym(group_by_var)
  
  # Friedman test
  friedman_result <- data %>%
    group_by(!!group_sym) %>%
    friedman_test(as.formula(paste(outcome_var, "~ workstation | subject_id")))
  
  # Compute post-hoc for all group levels
  posthoc_result <- data %>%
    group_by(!!group_sym) %>%
    pairwise_wilcox_test(
      as.formula(paste(outcome_var, "~ workstation")),
      paired = TRUE,
      p.adjust.method = "BH"
    ) %>%
    left_join(
      data %>%
        group_by(!!group_sym) %>%
        wilcox_effsize(
          as.formula(paste(outcome_var, "~ workstation")),
          paired = TRUE
        ),
      by = c(group_by_var, "group1", "group2")
    ) %>%
    mutate(
      label = paste0("p.adj=", format(round(p.adj, 4), nsmall = 4), 
                     ", r=", round(effsize, 2))
    )
  
  return(list(friedman = friedman_result, posthoc = posthoc_result))
}
