#' The GLA ggplot2 theme
#'
#' Non-data ink from the `chart-*` tokens: the plot sits on `chart-surface`,
#' gridlines are `chart-grid`, the axis line `chart-axis`, tick labels
#' `chart-label-muted` and titles `text`. These are the same tokens the
#' dashboards draw their plotly charts with, so a static chart and a live
#' one on the same page read as one system.
#'
#' Titles, subtitle and caption are left-aligned to the plot edge. Only one
#' axis carries gridlines by default: the value axis, which on most charts is
#' y. There is no axis line on the gridline axis, because the lowest
#' gridline sits where it would go and does the same job.
#'
#' @param base_size Base font size in points.
#' @param base_family Font family. The GLA type is Inter; the default is the
#'   device's sans so a chart renders wherever it is drawn. Pass `"Inter"`
#'   when the font is installed.
#' @param theme `"light"` or `"dark"`. Dark reads the `-dark` tokens and is
#'   for a chart on an inverse surface, not a dark-mode device setting.
#' @param gridlines Which axis carries gridlines: `"y"`, `"x"`, `"both"` or
#'   `"none"`. Use `"x"` for horizontal bars.
#' @param legend Legend position: `"top"`, `"right"`, `"bottom"` or `"none"`.
#' @return A ggplot2 theme.
#' @examples
#' library(ggplot2)
#' ggplot(economics, aes(date, unemploy)) +
#'   geom_line(colour = gla_token("data-primary")) +
#'   labs(title = "Unemployment", caption = "Source: FRED") +
#'   theme_gla()
#' @export
theme_gla <- function(base_size = 12, base_family = "", theme = c("light", "dark"),
                      gridlines = c("y", "x", "both", "none"),
                      legend = c("top", "right", "bottom", "none")) {
  theme <- match.arg(theme)
  gridlines <- match.arg(gridlines)
  legend <- match.arg(legend)
  ct <- chart_tokens(theme)

  grid <- ggplot2::element_line(colour = ct$grid, linewidth = 0.4)
  none <- ggplot2::element_blank()
  axis_line <- ggplot2::element_line(colour = ct$axis, linewidth = 0.4)

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = ct$surface, colour = NA),
      panel.background = ggplot2::element_rect(fill = ct$surface, colour = NA),
      text = ggplot2::element_text(colour = ct$text),
      plot.title = ggplot2::element_text(
        colour = ct$text, face = "bold", size = base_size * 17 / 12,
        margin = ggplot2::margin(b = base_size * 0.3)
      ),
      plot.subtitle = ggplot2::element_text(
        colour = ct$text_muted, size = base_size,
        margin = ggplot2::margin(b = base_size)
      ),
      plot.caption = ggplot2::element_text(
        colour = ct$text_muted, size = base_size * 10 / 12, hjust = 0,
        margin = ggplot2::margin(t = base_size)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.text = ggplot2::element_text(colour = ct$label_muted, size = base_size * 11 / 12),
      axis.title = ggplot2::element_text(colour = ct$text_muted, size = base_size * 11 / 12),
      axis.ticks = none,
      panel.grid.minor = none,
      panel.grid.major.y = if (gridlines %in% c("y", "both")) grid else none,
      panel.grid.major.x = if (gridlines %in% c("x", "both")) grid else none,
      # the axis line goes on the axis that has no gridlines, where nothing
      # else draws the baseline
      axis.line.x = if (gridlines %in% c("y", "both")) none else axis_line,
      axis.line.y = if (gridlines %in% c("x", "both")) none else axis_line,
      legend.position = legend,
      legend.title = none,
      legend.text = ggplot2::element_text(colour = ct$label, size = base_size * 11 / 12),
      legend.justification = if (legend %in% c("top", "bottom")) "left" else "center",
      legend.key.size = grid::unit(base_size, "pt"),
      strip.text = ggplot2::element_text(colour = ct$text, face = "bold", hjust = 0),
      plot.margin = ggplot2::margin(base_size, base_size, base_size, base_size)
    )
}
