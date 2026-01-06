

# Size presets (mm)
fig_sizes <- list(
  journal_1col = c(83, 60),   # width, height in mm
  journal_2col = c(182, 120),
  ppt_169      = c(280, 157)  # ~11 x 6.2 in
)

theme_base <- function(base_size = 10, base_family = "Helvetica") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.position = "right",
      strip.text = element_text(face = "bold")
    )
}

theme_journal_1col <- function() theme_base(8)
theme_journal_2col <- function() theme_base(9)
theme_ppt <- function() theme_base(14)

# Helper to pick size by key
size_mm <- function(key) {
  stopifnot(key %in% names(fig_sizes))
  fig_sizes[[key]]
}

# Wrapper around ggsave with millimeters
save_fig <- function(p, path, size_key = "journal_1col", dpi = 300) {
  wh <- size_mm(size_key)
  ggplot2::ggsave(
    filename = path, plot = p,
    width = wh[1], height = wh[2], units = "mm", dpi = dpi
  )
}