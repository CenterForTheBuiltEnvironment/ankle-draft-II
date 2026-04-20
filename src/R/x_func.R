# ==============================================================================
# Title: Plotting Functions
# Description: Reusable functions for visualizing experimental results
# Author: Toby Kramer, Junmeng Lyu
# Date: 2025-10-29
# ==============================================================================

source(here::here("src", "R", "x_setup.R"))


# Stacked Bar Chart Functions ==================================================

#' Prepare data for stacked percentage bar chart by workstation
#'
#' @param data Survey data frame
#' @param var Response variable to summarize
#' @return Data frame with counts and percentages by session_sat and workstation
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


#' Create stacked percentage bar chart by workstation
#'
#' @param data Survey data frame
#' @param var Response variable to plot (unquoted)
#' @param palette Named vector of colors for fill scale
#' @return ggplot object
plot_stacked_pct_ws <- function(data, var, palette) {
  df <- data %>%
    drop_na({{ var }})

  ggplot(df, aes(x = workstation, fill = {{ var }})) +
    geom_bar(position = position_fill(reverse = TRUE), show.legend = TRUE) +
    facet_wrap(~ session_sat) +
    scale_y_continuous(
      labels = percent,
      expand = expansion(mult = c(0, 0.01))
    ) +
    scale_fill_manual(values = palette, labels = label_wrap_gen(width = 9), drop = FALSE) +
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
    ) +
    guides(
      fill = guide_legend(
        reverse = TRUE,
        ncol = 1,
        byrow = TRUE,
        label.position = "right",
        keywidth = unit(7.5, "mm"),
        keyheight = unit(5, "mm"),
        label.hjust = 0,
        label.vjust = 0.5
      )
    )
}


# Mixed Model Plot Functions ===================================================

#' Plot predicted probability from logistic mixed-effects model
#'
#' @param model Fitted glmer model object
#' @param xterm Predictor term for ggpredict (e.g., "v_air_m_s [all]")
#' @param xlab X-axis label
#' @return ggplot object showing predicted probability with confidence band
plot_prob <- function(model, xterm, xlab) {
  pred <- as.data.frame(ggpredict(model, terms = xterm))
  
  ggplot(pred, aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    geom_line(linewidth = 1.2) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = xlab, y = "Predicted probability of Air movement disacceptability") +
    theme_classic()
}


#' Plot linear mixed-effects model predictions with raw data
#'
#' @param dat Data frame with observations
#' @param xterm Predictor variable name (string)
#' @param xlab X-axis label
#' @param model Fitted lmer model object
#' @return ggplot object showing data points with model prediction line
plot_lmm <- function(dat, xterm, xlab, model) {
  pred <- as.data.frame(ggpredict(model, terms = xterm))
  
  ggplot(dat, aes(x = .data[[xterm]], y = air_movement_acceptability_ankles)) +
    geom_point(alpha = 0.25, size = 1.5) +
    geom_ribbon(
      data = pred,
      aes(x = x, ymin = conf.low, ymax = conf.high),
      inherit.aes = FALSE,
      alpha = 0.2
    ) +
    geom_line(
      data = pred,
      aes(x = x, y = predicted),
      inherit.aes = FALSE,
      linewidth = 0.9
    ) +
    labs(x = xlab, y = "Air movement acceptability (ankles)") +
    theme_classic()
}


# Time Course Plot Functions ===================================================

#' Plot mean +/- SD time course with workstation colors and session facets
#'
#' @param data Summary data frame with `plot_time_min`, `mean_value`, and `sd_value`
#' @param y_lab Y-axis label
#' @return ggplot object
plot_timecourse_mean_sd <- function(data, y_lab) {
  ggplot(data, aes(x = plot_time_min, y = mean_value, color = workstation, fill = workstation)) +
    geom_ribbon(
      aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
      alpha = 0.15,
      linewidth = 0
    ) +
    geom_line(linewidth = 0.8) +
    facet_grid(
      tsk_site ~ session_type,
      labeller = ggplot2::labeller(
        session_type = c(
          yosemite = "15C",
          yellowstone = "17C",
          sequoia = "19C"
        )
      )
    ) +
    scale_color_manual(values = workstation_palette, drop = FALSE) +
    scale_fill_manual(values = workstation_palette, drop = FALSE) +
    labs(
      x = "Time (min)",
      y = y_lab,
      color = "Air speed",
      fill = "Air speed"
    ) +
    theme_minimal(base_size = 7) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(margin = margin(t = 4)),
      legend.position = "top",
      strip.text = element_text()
    )
}


# Statistical Comparison Plot Functions ========================================

#' Plot paired t-test comparisons with boxplots
#'
#' Annotated with BH-adjusted p-values and effect sizes (Cohen's d).
#'
#' @param data Wide-format analysis data frame
#' @param outcome_var Outcome variable name (string)
#' @param test_result Result from pairedttest_with_anova()$pairwise
#' @param y_label Y-axis label (defaults to outcome_var)
#' @param group_by_var Grouping variable (default: "session_sat")
#' @param p.signif Whether to use significance stars (default: TRUE)
#' @return ggplot object with boxplots and significance annotations
plot_pairedttest <- function(data, outcome_var, test_result,
                             y_label = NULL, group_by_var = "session_sat",
                             p.signif = TRUE) {
  outcome_sym <- sym(outcome_var)
  
  if (is.null(y_label)) y_label <- outcome_var
  
  # Format label with p-value and effect size for significant comparisons
  test_result_labeled <- test_result %>%
    mutate(
      label = if_else(
        p.adj < 0.05,
        paste0("p.adj=", format(round(p.adj, 4), nsmall = 4),
               ", d=", round(effsize, 2)),
        NA_character_
      )
    )
  
  ggplot(data, aes(x = workstation, y = !!outcome_sym, fill = workstation)) +
    geom_boxplot(outlier.alpha = 0.4) +
    facet_wrap(
      as.formula(paste("~", group_by_var)),
      nrow = 1,
      labeller = labeller(session_sat = function(x) paste0(x, " C"))
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


#' Plot paired Wilcoxon test comparisons with stacked bar chart
#'
#' Annotated with BH-adjusted p-values and effect sizes (r).
#'
#' @param data Wide-format analysis data frame
#' @param outcome_var Outcome variable name (string)
#' @param posthoc_result Result from pairedwilcoxtest()$posthoc
#' @param group_by_var Grouping variable (default: "session_sat")
#' @param fill_label Legend title (defaults to outcome_var)
#' @param fill_palette Color palette for fill scale
#' @return ggplot object with stacked bars and significance annotations
plot_pairedwilcox <- function(data, outcome_var, posthoc_result,
                              group_by_var = "session_sat",
                              fill_label = NULL,
                              fill_palette = thermal_preference_palette) {
  outcome_sym <- sym(outcome_var)
  group_sym <- sym(group_by_var)
  
  if (is.null(fill_label)) fill_label <- outcome_var
  
  # Factor level configurations for common outcome variables
  factor_config <- list(
    thermal_preference = list(
      levels = c(-1, 0, 1),
      labels = c("Cooler", "Neutral", "Warmer")
    ),
    thermal_preference_ankles = list(
      levels = c(-1, 0, 1),
      labels = c("Cooler", "Neutral", "Warmer")
    ),
    clothing_change = list(
      levels = c(0, 1),
      labels = c("No change", "Changed")
    ),
    disacceptability_with_draft_ankles = list(
      levels = c(0, 1),
      labels = c("Accept", "Not accept")
    ),
    air_movement_preference_ankles = list(
      levels = c(-1, 0, 1),
      labels = c("Lower", "No change", "Higher")
    )
  )
  
  config <- factor_config[[outcome_var]]
  
  if (!is.null(config)) {
    data_plot <- data %>%
      mutate(!!outcome_sym := factor(
        !!outcome_sym,
        levels = config$levels,
        labels = config$labels
      ))
  } else {
    data_plot <- data
  }
  
  # Filter significant comparisons and set y position for annotations
  sig_data <- posthoc_result %>%
    filter(p.adj < 0.05) %>%
    mutate(y.position = 1.05)
  
  p <- ggplot(data_plot, aes(x = workstation, fill = !!outcome_sym)) +
    geom_bar(position = "fill", color = "grey30", linewidth = 0.2) +
    facet_wrap(
      as.formula(paste("~", group_by_var)),
      nrow = 1,
      labeller = labeller(session_sat = function(x) paste0(x, " C"))
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
        y.position = -0.1 + 0.04 * ave(
          sig_data$y.position,
          sig_data[[group_by_var]],
          FUN = seq_along
        ),
        tip.length = 0.01,
        bracket.size = 0.3,
        size = 3,
        vjust = 2
      )
  }
  
  return(p)
}

plot_draft_model <- function(data, label, subtitle_text){
  
  ppd_levels <- c(10,20,40,60,80)
  
  z_mat <- xtabs(PPD ~ TS + V, data = data)
  contour_lines <- grDevices::contourLines(
    x = as.numeric(rownames(z_mat)),
    y = as.numeric(colnames(z_mat)),
    z = as.matrix(z_mat),
    levels = ppd_levels
  )
  
  contour_length <- function(line) {
    sum(sqrt(diff(line$x)^2 + diff(line$y)^2))
  }
  
  label_y <- 1.055
  tick_y_top <- 1.018
  tick_y_bottom <- 1.002
  
  contour_labels <- purrr::map_dfr(seq_along(ppd_levels), function(i) {
    level <- ppd_levels[i]
    level_lines <- contour_lines[
      vapply(contour_lines, function(line) line$level == level, logical(1))
    ]
    
    if (length(level_lines) == 0) {
      return(tibble::tibble())
    }
    
    line_scores <- purrr::map_dbl(level_lines, function(line) {
      max(line$y) + contour_length(line) * 1e-6
    })
    target_line <- level_lines[[which.max(line_scores)]]
    idx_top <- which.max(target_line$y)
    tick_x <- target_line$x[idx_top]
    
    tibble::tibble(
      tick_x = tick_x,
      text_x = tick_x,
      V = label_y,
      tick_y_top = tick_y_top,
      tick_y_bottom = tick_y_bottom,
      prefix_label = if (level == max(ppd_levels)) {
        "PPD[AD]~\"=\""
      } else {
        NA_character_
      },
      value_label = paste0(level, "*\"%\"")
    )
  })
  
  ggplot(data, aes(TS, V)) +
    
    geom_raster(
      data = dplyr::filter(data, abs(TS) < 0.5 & PPD < 20),
      fill = "#0077b6",
      alpha = 0.75
    ) +
    
    geom_contour(
      aes(z = PPD),
      breaks = ppd_levels,
      color = "#99d98c",
      linewidth = 1
    ) +
    
    geom_text(
      data = dplyr::filter(contour_labels, !is.na(prefix_label)),
      aes(x = text_x - 0.02, y = V, label = prefix_label),
      inherit.aes = FALSE,
      parse = TRUE,
      hjust = 1,
      size = 2,
      color = "#2d6a4f"
    ) +
    
    geom_text(
      data = contour_labels,
      aes(x = text_x, y = V, label = value_label),
      inherit.aes = FALSE,
      parse = TRUE,
      hjust = 0,
      size = 2,
      color = "#2d6a4f"
    ) +
    
    scale_y_continuous(
      name = "Ankle air speed (m/s)",
      sec.axis = sec_axis(~ . * 196.85, name = "Ankle air speed (fpm)")
    ) +
    
    scale_x_continuous(
      name = "Whole-body thermal sensation (~PMV)",
      breaks = -3:3,
      labels = c("Cold","Cool","Slightly\ncool","Neutral",
                 "Slightly\nwarm","Warm","Hot")
    ) +
    
    coord_cartesian(xlim = c(-3,3), ylim = c(0,1), expand = FALSE, clip = "off") +
    
    
    theme_classic(base_size = 9) +
    
    theme(
      plot.tag = element_text(size = 9, face = "bold"),
      plot.subtitle = element_text(
        hjust = 0.02,
        margin = margin(b = 5, unit = "mm"),
        size=9
      ),
      axis.title.x = element_text(margin = margin(t= 3, unit = "mm")),
      plot.margin = margin(t = 8, r = 3, b = 2, l = 6, unit = "mm")
    ) +
    
    
    labs(
      tag = label,
      subtitle = subtitle_text
    )
}
