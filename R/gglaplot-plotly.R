# gglaplot's plotly helpers, on the tokens and the chart type roles, and
# the conversions that let plotly::ggplotly() draw the house marks. plotly
# is a suggested package: these need it installed, nothing else does.

#' Style a plotly chart in the house style
#'
#' gglaplot's helpers for a plotly chart, kept for the scripts that use
#' them. `ggla_plotly_settings()` applies all three: `ggla_plotly_layout()`
#' sets the surface, the type and the axes from the tokens and the chart
#' roles, `ggla_plotly_style()` the hover label, and `ggla_plotly_config()`
#' hides the mode bar. The arguments are gglaplot's. Interactive charts on
#' a GLA site belong to the visualisation team's web components; this is
#' for a notebook or a one-off.
#'
#' `plotly::ggplotly()` also understands the `geom_gla_*()` marks, so a
#' ggplot made here converts.
#'
#' @param plot A plotly object, from `plotly::plot_ly()` or
#'   `plotly::ggplotly()`.
#' @param gla_theme `"default"` or `"inverse"`.
#' @param x_axis_title,y_axis_title Show the axis titles.
#' @param xgridlines Draw gridlines on the x axis as well.
#' @param legend Show the legend, at the top left.
#' @param base_size The tick size in pixels.
#' @param annotations A list passed to `plotly::layout(annotations = )`,
#'   with the house defaults filled in.
#' @param ... Passed to `plotly::layout()`, `plotly::style()` or
#'   `plotly::config()`.
#' @return The plotly object.
#' @examples
#' if (requireNamespace("plotly", quietly = TRUE)) {
#'   library(ggplot2)
#'   p <- ggplot(gla_yearly, aes(year, average, colour = series)) +
#'     geom_gla_line() +
#'     scale_colour_gla() +
#'     theme_gla()
#'   ggla_plotly_settings(plotly::ggplotly(p))
#' }
#' @name ggla_plotly
NULL

#' @rdname ggla_plotly
#' @export
ggla_plotly_settings <- function(plot, gla_theme = "default", x_axis_title = FALSE,
                                 y_axis_title = FALSE, xgridlines = FALSE, legend = TRUE,
                                 base_size = 14, ...) {
  plot <- ggla_plotly_layout(plot, gla_theme = gla_theme, x_axis_title = x_axis_title,
                             y_axis_title = y_axis_title, xgridlines = xgridlines,
                             legend = legend, base_size = base_size, ...)
  plot <- ggla_plotly_style(plot, gla_theme = gla_theme, base_size = base_size)
  ggla_plotly_config(plot)
}

#' @rdname ggla_plotly
#' @export
ggla_plotly_layout <- function(plot, gla_theme = "default", x_axis_title = FALSE,
                               y_axis_title = FALSE, xgridlines = FALSE, legend = TRUE,
                               base_size = 14, annotations = NULL, ...) {
  rlang::check_installed("plotly", "for the plotly helpers")
  theme <- compat_theme(gla_theme)
  ct <- chart_tokens(theme)
  roles <- gla_type()
  size <- function(role) base_size * roles[[role]]$size / roles$tick$size
  family <- "Inter, Arial, sans-serif"
  font <- function(colour, role, weight = 400) {
    list(color = colour, family = family, size = size(role), weight = weight)
  }
  axis <- function(gridlines) {
    list(
      automargin = TRUE, ticks = "", showticklabels = TRUE, tickfont = font(ct$label_muted, "tick"),
      tickangle = 0, showline = !gridlines, linecolor = ct$axis, linewidth = 1,
      showgrid = gridlines, gridcolor = ct$grid, gridwidth = 1, zeroline = FALSE,
      title = list(font = font(ct$text_muted, "axis_title", 500))
    )
  }
  has_title <- !is.null(plot$x$layout$title$text)
  plot <- plotly::layout(
    plot,
    margin = list(t = if (has_title) 4 * size("title") else 16, r = 16, b = 16, l = 16, pad = 0),
    plot_bgcolor = ct$surface, paper_bgcolor = ct$surface,
    font = font(ct$text, "tick"),
    title = list(font = font(ct$text, "title", 600), x = 0, xref = "container", xanchor = "left"),
    xaxis = axis(xgridlines), yaxis = axis(TRUE),
    legend = list(bgcolor = ct$surface, bordercolor = "transparent", borderwidth = 0,
                  x = 0, y = 1, orientation = "h", xanchor = "left", yanchor = "bottom",
                  font = font(ct$label, "label")),
    showlegend = legend
  )
  if (!x_axis_title) plot <- plotly::layout(plot, xaxis = list(title = list(text = NULL)))
  if (!y_axis_title) plot <- plotly::layout(plot, yaxis = list(title = list(text = NULL)))
  if (!is.null(annotations)) {
    defaults <- list(arrowhead = 0, showarrow = TRUE, arrowcolor = ct$label, arrowwidth = 1,
                     bgcolor = ct$surface, bordercolor = ct$surface, borderpad = 0, borderwidth = 0)
    annotations <- utils::modifyList(defaults, annotations)
    annotations$font <- utils::modifyList(font(ct$label, "label"), annotations$font %||% list())
    plot <- plotly::layout(plot, annotations = annotations)
  }
  plotly::layout(plot, ...)
}

#' @rdname ggla_plotly
#' @export
ggla_plotly_style <- function(plot, gla_theme = "default", base_size = 14, ...) {
  rlang::check_installed("plotly", "for the plotly helpers")
  ct <- chart_tokens(compat_theme(gla_theme))
  plotly::style(
    plot,
    hoverlabel = list(bgcolor = ct$text, bordercolor = ct$text,
                      font = list(family = "Inter, Arial, sans-serif", color = ct$surface,
                                  size = base_size)),
    ...
  )
}

#' @rdname ggla_plotly
#' @export
ggla_plotly_config <- function(plot, ...) {
  rlang::check_installed("plotly", "for the plotly helpers")
  plotly::config(plot, displayModeBar = FALSE, ...)
}

# plotly::ggplotly() converts a layer by the class of its geom. The house
# marks are the base geoms with different defaults, so each converts as its
# parent: the class is swapped and plotly's own method for the parent runs.
as_basic <- function(parent) {
  function(data, prestats_data, layout, params, p, ...) {
    class(data) <- c(parent, class(data)[-1])
    plotly::to_basic(data, prestats_data, layout, params, p, ...)
  }
}

#' @exportS3Method plotly::to_basic
to_basic.GeomGlaLine <- as_basic("GeomLine")
#' @exportS3Method plotly::to_basic
to_basic.GeomGlaDot <- as_basic("GeomPoint")
#' @exportS3Method plotly::to_basic
to_basic.GeomGlaMarker <- as_basic("GeomPoint")
#' @exportS3Method plotly::to_basic
to_basic.GeomGlaBar <- as_basic("GeomBar")
#' @exportS3Method plotly::to_basic
to_basic.GeomGlaArea <- as_basic("GeomArea")
#' @exportS3Method plotly::to_basic
to_basic.GeomGlaConnector <- as_basic("GeomSegment")
#' @exportS3Method plotly::to_basic
to_basic.GeomGlaTreemap <- as_basic("GeomRect")
#' @exportS3Method plotly::to_basic
to_basic.GeomGlaTreemapLabel <- as_basic("GeomText")
#' @exportS3Method plotly::to_basic
to_basic.GeomGlaText <- as_basic("GeomText")
#' @exportS3Method plotly::to_basic
to_basic.GeomGlaEndlabel <- as_basic("GeomText")
#' @exportS3Method plotly::to_basic
to_basic.GeomGLAAnnotate <- as_basic("GeomText")
#' @exportS3Method plotly::to_basic
to_basic.GeomGlaHline <- as_basic("GeomHline")
#' @exportS3Method plotly::to_basic
to_basic.GeomGlaVline <- as_basic("GeomVline")
# the labels on a reference line sit at the panel edge, which a plotly
# trace has no position for; the line converts and the label is left off
#' @exportS3Method plotly::to_basic
to_basic.GeomGlaEdgelabel <- as_basic("GeomBlank")
#' @exportS3Method plotly::to_basic
to_basic.GeomGlaToplabel <- as_basic("GeomBlank")
