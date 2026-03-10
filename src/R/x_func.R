# Title: Plotting functions
# Description: Define functions for plotting different types experimental results
# Author: Toby Kramer, Junmeng Lyu
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

stacked_pct_data_ws <- function(data, var) {
  
  var <- rlang::enquo(var)
  
  data %>%
    drop_na(!!var) %>%
    count(session_sat, workstation, !!var, name = "n") %>%
    group_by(session_sat, workstation) %>%
    mutate(
      pct = n / sum(n),
      pct_label = scales::percent(pct, accuracy = 1)
    ) %>%
    ungroup()
}

plot_stacked_pct_ws <- function(data, var, palette) {
  
  df <- data %>%
    drop_na({{ var }})
  
  ggplot(df, aes(x = workstation, fill = {{ var }})) +
    geom_bar(position = "fill") +
    facet_wrap(~ session_sat) +
    scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.01))) +
    scale_fill_manual(values = palette, labels = label_wrap_gen(width = 9)) +
    labs(x = "Workstation", y = "No. of Subjects (%)") +
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
    )
}


# Plot Functions ----------------------------------------

# Plot for Part 1: Logistic relationship between predictors and AMA --------
# (mixed-effects logistic model).
plot_prob <- function(model, xterm, xlab){
  pred <- as.data.frame(ggpredict(model, terms = xterm))
  ggplot(pred, aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    geom_line(linewidth = 1.2) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = xlab, y = "Predicted probability of Air movement disacceptability") +
    theme_classic()
}

# Plot for Part 1: Linear relationship between predictors and AMA --------
# (mixed-effects linear model).
plot_lmm <- function(dat, xterm, xlab, model){
  pred <- as.data.frame(ggpredict(model, terms = xterm))
  ggplot(dat, aes(x = .data[[xterm]], y = air_movement_acceptability_ankles)) +
    geom_point(alpha=0.25, size=1.5) +
    geom_ribbon(data = pred, aes(x = x, ymin = conf.low, ymax = conf.high),
                inherit.aes = FALSE, alpha = 0.2) +
    geom_line(data = pred, aes(x = x, y = predicted),
              inherit.aes = FALSE, linewidth = 0.9) +
    labs(x = xlab, y = "Air movement acceptability (ankles)") +
    theme_classic()
}


# Plot for Part 2 paired t-test comparisons ----------------------------
# Annotated with BH-adjusted p-values and effect sizes (Cohen’s d).

plot_pairedttest <- function(data, outcome_var, test_result,
                                   y_label = NULL, group_by_var = "session_sat",
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
      labeller = labeller(session_sat = function(x) paste0(x, " °C"))
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


# Plot for Part 2 paired wilcox comparisons -------------------------------
# Annotated with BH-adjusted p-values and effect sizes (r).
plot_pairedwilcox <- function(data, outcome_var, posthoc_result,
                                          group_by_var = "session_sat",
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
      labeller = labeller(session_sat = function(x) paste0(x, " °C"))
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