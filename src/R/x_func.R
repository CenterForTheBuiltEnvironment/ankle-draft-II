# Title: Plotting functions
# Description: Define functions for plotting different types experimental results
# Author: Toby Kramer
# Date: 2025-10-29

source(here::here("src", "R", "x_setup.R"))


# Tables ---------------------------------------------------------------

ft_std <- function(data) {
  
  thin_border  <- fp_border(color = "black", width = 0.4)
  thick_border <- fp_border(color = "black", width = 1.0)

  ft <- flextable(data)
  
  # ---- Global standard settings ----
  ft <- font(ft, fontname = "Arial", part = "all")
  ft <- fontsize(ft, size = 9, part = "body")
  ft <- fontsize(ft, size = 9, part = "header")
  ft <- bold(ft, part = "header")
  
  # Borders
  ft <- border_remove(ft)
  ft <- hline_top(ft, part = "header", border = thin_border)
  ft <- hline(ft, part = "header", border = thin_border)
  ft <- hline_bottom(ft, part = "body", border = thin_border)
  
  # Spacing
  ft <- padding(ft, padding = 3)
  
  
  return(ft)
}


# Subject Feedback --------------------------------------------------------

stacked_pct_data <- function(data, var) {
  
  var <- rlang::enquo(var)
  
  data %>%
    drop_na(!!var) %>%
    count(session_type, !!var, name = "n") %>%
    group_by(session_type) %>%
    mutate(
      pct = n / sum(n),
      pct_label = scales::percent(pct, accuracy = 1)
    ) %>%
    ungroup()
}

plot_stacked_pct <- function(data, var, palette) {
  
  df <- data %>%
    drop_na({{ var }})
  
  ggplot(df, aes(x = session_type, fill = {{ var }})) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.01))) +
    scale_fill_manual(values = palette, labels = label_wrap_gen(width = 9)) +
    labs(x = "Session type", y = "Percentage") +
    theme_minimal(base_size = 7) +
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.ticks.y = element_line(color = "grey", linewidth = 0.25),
      axis.ticks.x = element_blank(),
      axis.ticks.length = unit(1, "mm")
    ) +
    guides(
      fill = guide_legend(
        ncol = 1,
        byrow = TRUE,
        label.position = "right",
        keywidth  = unit(7.5, "mm"),
        keyheight = unit(5, "mm"),
        label.hjust = 0,
        label.vjust = 0.5
      )
    )
}


# Statistical Comparison Functions ----------------------------------------
# 1. Paired comparison test with effect size (t-test or Wilcox test)
paired_comparison <- function(data, outcome_var, test_type = "t.test",
                             p.adjust.method = "BH", group_by_var = "session_diffusor_sat") {
  outcome_sym <- sym(outcome_var)
  group_sym <- sym(group_by_var)
  
  if (test_type == "t.test") {
    test_result <- data %>%
      group_by(!!group_sym) %>%
      pairwise_t_test(
        as.formula(paste(outcome_var, "~ workstation")),
        paired = TRUE,
        p.adjust.method = p.adjust.method
      )
    
    effect_result <- data %>%
      group_by(!!group_sym) %>%
      cohens_d(
        as.formula(paste(outcome_var, "~ workstation")),
        paired = TRUE
      )
  } else if (test_type == "wilcox") {
    test_result <- data %>%
      group_by(!!group_sym) %>%
      pairwise_wilcox_test(
        as.formula(paste(outcome_var, "~ workstation")),
        paired = TRUE,
        p.adjust.method = p.adjust.method
      )
    
    effect_result <- data %>%
      group_by(!!group_sym) %>%
      wilcox_effsize(
        as.formula(paste(outcome_var, "~ workstation")),
        paired = TRUE
      )
  }
  
  # Build proper by vector for join
  by_cols <- c(group_by_var, "group1", "group2")
  
  result <- test_result %>%
    left_join(effect_result, by = by_cols) %>%
    add_xy_position(x = "workstation")
  
  return(result)
}

# 2. Plot paired comparison with boxplot and p-values
plot_paired_comparison <- function(data, outcome_var, test_result, 
                                   y_label = NULL, group_by_var = "session_diffusor_sat",
                                   p.signif = TRUE) {
  outcome_sym <- sym(outcome_var)
  
  if (is.null(y_label)) y_label <- outcome_var
  
  # Create formatted label with p-value and effect size if available
  test_result_labeled <- test_result %>%
    mutate(
      label = if_else(
        p.adj < 0.05,
        paste0(
          "p.adj=", format(round(p.adj, 4), nsmall = 4),
          ", d=", round(effsize, 2)
        ),
        NA_character_
      )
    )
  
  ggplot(data, aes(x = workstation, y = !!outcome_sym, fill = workstation)) +
    geom_boxplot(outlier.alpha = 0.4) +
    facet_wrap(
      as.formula(paste("~", group_by_var)),
      nrow = 1,
      labeller = labeller(session_diffusor_sat = function(x) paste0(x, " °C"))
    ) +
    stat_pvalue_manual(
      test_result_labeled,
      label = "label",
      hide.ns = TRUE
    ) +
    scale_fill_manual(values = workstation_palette) +
    labs(x = "Air movement", y = y_label) +
    theme_bw() +
    theme(legend.position = "none")
}

# 3. Friedman test with posthoc analysis
friedman_posthoc <- function(data, outcome_var, group_by_var = "session_diffusor_sat") {
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

# 4. Plot distribution with post-hoc significance annotations
plot_distribution_with_posthoc <- function(data, outcome_var, posthoc_result, 
                                          group_by_var = "session_diffusor_sat",
                                          fill_label = NULL, fill_palette = thermal_preference_palette) {
  outcome_sym <- sym(outcome_var)
  group_sym <- sym(group_by_var)
  
  if (is.null(fill_label)) fill_label <- outcome_var
  
  factor_config <- list(
    thermal_preference = list(levels = c(-1, 0, 1), 
                             labels = c("Cooler", "Neutral", "Warmer")),
    thermal_preference_ankles = list(levels = c(-1, 0, 1), 
                                    labels = c("Cooler", "Neutral", "Warmer")),
    clothing_change = list(levels = c(0, 1), 
                          labels = c("No change", "Changed")),
    disacceptability_with_draft_ankles = list(levels = c(0, 1), 
                                             labels = c("Accept", "Not accept")),
    air_movement_preference_ankles = list(levels = c(-1, 0, 1), 
                                         labels = c("Lower", "No change", "Higher"))
  )
  
  config <- factor_config[[outcome_var]]
  
  if (!is.null(config)) {
    data_plot <- data %>%
      mutate(!!outcome_sym := factor(!!outcome_sym, levels = config$levels, labels = config$labels))
  } else {
    data_plot <- data
  }
  
  # Filter significant comparisons and prepare for plotting
  sig_data <- posthoc_result %>%
    filter(p.adj < 0.05) %>%
    mutate(y.position = 1.05)
  
  p <- ggplot(data_plot, aes(x = workstation, fill = !!outcome_sym, group = !!outcome_sym)) +
    geom_bar(position = "fill", color = "grey30", linewidth = 0.2) +
    facet_wrap(
      as.formula(paste("~", group_by_var)),
      nrow = 1,
      labeller = labeller(session_diffusor_sat = function(x) paste0(x, " °C"))
    ) +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = fill_palette, drop = FALSE) +
    labs(x = "Air movement", y = "Proportion", fill = fill_label) +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      strip.background = element_rect(fill = "grey90", color = NA)
    )
  
  # Add significance annotations if any exist
  if (nrow(sig_data) > 0) {
    p <- p +
      stat_pvalue_manual(
        sig_data,
        label = "label",
        xmin = "group1",
        xmax = "group2",
        y.position = -0.1 + 0.04 * ave(sig_data$y.position, sig_data[[group_by_var]], FUN = seq_along),
        tip.length = 0.01,
        bracket.size = 0.3,
        size = 3,
        vjust = 2
      )
  }
  
  return(p)
}

# 5. Add group statistics (means and sample sizes) to test results
add_group_stats <- function(test_df, outcome_name, stats_data) {
  test_df %>%
    mutate(outcome_var = outcome_name) %>%
    left_join(
      stats_data %>%
        filter(outcome_var == outcome_name) %>%
        select(-outcome_var) %>%
        rename(temp = session_diffusor_sat, group1_mean = group_mean, group1_n = group_n),
      by = c("session_diffusor_sat" = "temp", "group1" = "workstation")
    ) %>%
    left_join(
      stats_data %>%
        filter(outcome_var == outcome_name) %>%
        select(-outcome_var) %>%
        rename(temp = session_diffusor_sat, group2_mean = group_mean, group2_n = group_n),
      by = c("session_diffusor_sat" = "temp", "group2" = "workstation")
    ) %>%
    select(-outcome_var)
}
