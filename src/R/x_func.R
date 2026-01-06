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

