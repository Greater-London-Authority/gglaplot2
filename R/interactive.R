#' An interactive chart from a ggplot, with the chrome plotly drops
#'
#' [plotly::ggplotly()] converts a chart drawn here, but leaves three things
#' behind: the eyebrow above the title, the `Source:` and `Note:` lines of
#' the footer, and the legend of a stepped scale, which it draws as
#' polygons with no key at all. `gla_plotly()` converts the chart and puts
#' them back: the eyebrow and title as the plot title, the footer as an
#' annotation under the plot with its labels in bold, and the stepped
#' scale's bins as a legend, highest bin first, labelled with the ranges
#' [guide_gla_bins()] shows. The surface, type and hover label come from
#' the tokens as [ggla_plotly_settings()] sets them.
#'
#' Interactive charts on a GLA site belong to the visualisation team's web
#' components; this is for a notebook, a Shiny app or a one-off.
#'
#' @param plot A ggplot.
#' @param theme `"light"` or `"dark"`. Defaults to the mode set by
#'   [gla_mode()].
#' @param tooltip Passed to [plotly::ggplotly()].
#' @param ... Passed to [plotly::ggplotly()].
#' @return A plotly object.
#' @examples
#' if (requireNamespace("plotly", quietly = TRUE) && requireNamespace("sf", quietly = TRUE)) {
#'   library(ggplot2)
#'   b <- gla_boroughs
#'   b$change <- seq(-6, 4, length.out = nrow(b))
#'   p <- ggplot(b) +
#'     geom_gla_sf(aes(fill = change)) +
#'     scale_fill_gla_b("diverging", steps = 3, labels = label_gla_percent(scale = 1),
#'                      name = NULL) +
#'     gla_labs(eyebrow = "Synthetic data", title = "Change by borough",
#'              subtitle = "Percentage points", source = "Demo") +
#'     theme_gla_map()
#'   gla_plotly(p)
#' }
#' @export
gla_plotly <- function(plot, theme = gla_mode(), tooltip = "all", ...) {
  rlang::check_installed("plotly", "for interactive charts")
  theme <- match_mode(theme)
  ct <- chart_tokens(theme)
  built <- ggplot2::ggplot_build(plot)
  labels <- plot$labels
  out <- plotly::ggplotly(plot, tooltip = tooltip, ...)
  out <- ggla_plotly_settings(out, gla_theme = if (theme == "dark") "inverse" else "default")
  out <- plotly_chrome(out, labels, ct)
  out <- plotly_bin_legend(out, built, ct)
  out
}

# the eyebrow and title as one title block, the subtitle under it, and the
# footer as an annotation below the plot with its labels in bold
plotly_chrome <- function(out, labels, ct) {
  roles <- gla_type()
  px <- function(role) roles[[role]]$size
  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  span <- function(text, size, colour, weight = 400) {
    sprintf("<span style='font-size:%dpx;color:%s;font-weight:%d'>%s</span>",
            size, colour, weight, text)
  }
  title <- labels$title
  eyebrow <- attr(title, "eyebrow")
  title <- if (is.null(title) || inherits(title, "waiver")) "" else as.character(title)
  lines <- c(
    if (!is.null(eyebrow) && nzchar(eyebrow)) span(esc(eyebrow), px("eyebrow"), ct$text_muted, 500),
    if (nzchar(title)) span(esc(title), px("title"), ct$text, 600),
    if (!is.null(labels$subtitle) && !inherits(labels$subtitle, "waiver")) {
      span(esc(as.character(labels$subtitle)), px("subtitle"), ct$text_muted)
    }
  )
  caption <- labels$caption
  footer <- if (!is.null(caption) && !inherits(caption, "waiver")) {
    parts <- strsplit(as.character(caption), "\n", fixed = TRUE)[[1]]
    parts <- vapply(parts, function(line) {
      head <- regmatches(line, regexpr("^(Source|Note):", line))
      if (length(head)) {
        rest <- trimws(substring(line, nchar(head) + 1))
        paste0("<b>", head, "</b> ", esc(rest))
      } else {
        esc(line)
      }
    }, character(1))
    paste(parts, collapse = "<br>")
  }
  n_lines <- length(lines)
  top <- if (n_lines) 16 + sum(vapply(lines, function(x) 0, numeric(1))) +
    (if (!is.null(eyebrow)) px("eyebrow") * 1.4 else 0) + px("title") * 1.3 +
    (if (!is.null(labels$subtitle)) px("subtitle") * 1.4 else 0) + 12 else 16
  out <- plotly::layout(
    out,
    title = list(text = paste(lines, collapse = "<br>"), x = 0, xref = "container",
                 xanchor = "left", y = 1, yref = "container", yanchor = "top",
                 pad = list(t = 16, l = 16)),
    margin = list(t = top, b = if (!is.null(footer)) 48 + 16 * length(parts) else 16)
  )
  if (!is.null(footer)) {
    out <- plotly::layout(out, annotations = list(list(
      text = footer, x = 0, y = 0, xref = "paper", yref = "paper", xanchor = "left",
      yanchor = "top", yshift = -36, showarrow = FALSE, align = "left",
      font = list(size = px("caption"), color = ct$text_muted,
                  family = "Inter, Arial, sans-serif")
    )))
  }
  out
}

# a stepped colour scale's bins as legend entries: one hidden marker per
# bin in the bin's colour, labelled with its range, highest first
plotly_bin_legend <- function(out, built, ct) {
  scale <- built$plot$scales$get_scales("fill") %||% built$plot$scales$get_scales("colour")
  if (is.null(scale) || !inherits(scale, "ScaleBinned")) return(out)
  limits <- scale$get_limits()
  breaks <- scale$get_breaks()
  edges <- sort(unique(c(limits[1], breaks, limits[2])))
  if (length(edges) < 2) return(out)
  mids <- edges[-length(edges)] + diff(edges) / 2
  colours <- scale$map(mids)
  labels <- scale$get_labels(edges)
  ranges <- paste0(labels[-length(labels)], " to ", labels[-1])
  order <- rev(seq_along(ranges))
  for (i in order) {
    out <- plotly::add_trace(
      out, x = list(NULL), y = list(NULL), type = "scatter", mode = "markers",
      marker = list(color = colours[i], symbol = "square", size = 14),
      name = ranges[i], showlegend = TRUE, hoverinfo = "none", inherit = FALSE
    )
  }
  plotly::layout(
    out, showlegend = TRUE,
    legend = list(orientation = "v", x = 1, y = 0.5, xanchor = "left", yanchor = "middle",
                  traceorder = "normal", itemclick = FALSE, itemdoubleclick = FALSE,
                  bgcolor = ct$surface)
  )
}
