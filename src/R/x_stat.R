# ==============================================================================
# Title: Statistical Functions
# Description: Reusable functions for statistical analysis
# Author: Toby Kramer
# Date: 2026-01-05
# ==============================================================================

source(here::here("src", "R", "x_setup.R"))


# Odds Ratio Extraction ========================================================

#' Extract odds ratio and 95% CI from logistic mixed-effects model
#'
#' @param mod Fitted glmer model object (binomial family)
#' @param term Predictor term name (string)
#' @return Tibble with term, OR, CI_low, CI_high
logistic_or_ci <- function(mod, term) {
  b <- fixef(mod)[term]
  se <- sqrt(vcov(mod)[term, term])

  tibble(
    term = term,
    OR = exp(b),
    CI_low = exp(b - 1.96 * se),
    CI_high = exp(b + 1.96 * se)
  )
}


# Paired Comparisons ===========================================================

#' Repeated-measures ANOVA with paired t-tests for continuous variables
#'
#' Performs repeated-measures ANOVA within each group level, followed by
#' pairwise paired t-tests with BH adjustment. Computes Cohen's d effect sizes.
#'
#' @param data Wide-format data frame with one row per subject-workstation
#' @param outcome_var Outcome variable name (string)
#' @param p.adjust.method P-value adjustment method (default: "BH")
#' @param group_by_var Grouping variable (default: "session_sat")
#' @param within_var Within-subject factor (default: "workstation")
#' @param subject_var Subject identifier (default: "subject_id")
#' @param alpha Significance threshold for ANOVA (default: 0.05)
#' @param pairwise_only_if_sig If TRUE, only run pairwise tests for significant
#'   ANOVA groups (default: FALSE)
#' @return List with 'anova' (ANOVA results) and 'pairwise' (pairwise tests)
pairedttest_with_anova <- function(data, outcome_var,
                                   p.adjust.method = "BH",
                                   group_by_var = "session_sat",
                                   within_var = "workstation",
                                   subject_var = "subject_id",
                                   alpha = 0.05,
                                   pairwise_only_if_sig = FALSE) {
  group_sym <- rlang::sym(group_by_var)
  within_sym <- rlang::sym(within_var)
  subject_sym <- rlang::sym(subject_var)

  # Repeated-measures ANOVA within each group level
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

  # Filter data for pairwise tests if requested
  data_for_pairwise <- if (pairwise_only_if_sig) {
    data %>% dplyr::filter(!!group_sym %in% sig_groups)
  } else {
    data
  }

  # Return early if no data for pairwise tests
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

  # Compute paired Cohen's d
  effect_result <- data_for_pairwise %>%
    dplyr::group_by(!!group_sym) %>%
    rstatix::cohens_d(
      as.formula(paste(outcome_var, "~", within_var)),
      paired = TRUE
    )

  # Compute group means for annotation
  mean_df <- data_for_pairwise %>%
    dplyr::group_by(!!group_sym, !!within_sym) %>%
    dplyr::summarise(
      mean_value = mean(.data[[outcome_var]], na.rm = TRUE),
      .groups = "drop"
    )

  # Attach means to group1/group2
  test_result <- test_result %>%
    dplyr::left_join(mean_df, by = c(group_by_var, "group1" = within_var)) %>%
    dplyr::rename(mean_group1 = mean_value) %>%
    dplyr::left_join(mean_df, by = c(group_by_var, "group2" = within_var)) %>%
    dplyr::rename(mean_group2 = mean_value)

  # Merge effect sizes and add plotting positions
  by_cols <- c(group_by_var, "group1", "group2")
  pairwise_res <- test_result %>%
    dplyr::left_join(effect_result, by = by_cols) %>%
    rstatix::add_xy_position(x = within_var)

  list(
    anova = aov_res,
    pairwise = pairwise_res
  )
}


#' Friedman test with paired Wilcoxon post-hoc for ordinal variables
#'
#' Performs Friedman test within each group level, followed by pairwise
#' Wilcoxon signed-rank tests with BH adjustment. Computes effect size r.
#'
#' @param data Wide-format data frame with one row per subject-workstation
#' @param outcome_var Outcome variable name (string)
#' @param group_by_var Grouping variable (default: "session_sat")
#' @return List with 'friedman' (Friedman test results) and 'posthoc' (pairwise)
pairedwilcoxtest <- function(data, outcome_var, group_by_var = "session_sat") {
  outcome_sym <- sym(outcome_var)
  group_sym <- sym(group_by_var)

  # Friedman test within each group
  friedman_result <- data %>%
    group_by(!!group_sym) %>%
    friedman_test(as.formula(paste(outcome_var, "~ workstation | subject_id")))

  # Post-hoc pairwise Wilcoxon tests with effect sizes
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
      label = paste0(
        "p.adj=", format(round(p.adj, 4), nsmall = 4),
        ", r=", round(effsize, 2)
      )
    )

  list(
    friedman = friedman_result,
    posthoc = posthoc_result
  )
}
