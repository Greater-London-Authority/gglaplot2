#' A paired dot plot
#'
#' Two values for each category, one marker each, joined by an arrow from
#' the first to the second: the web charts' paired dot plot, for a measure
#' at two dates across boroughs or groups. Categories go on `y`, values on
#' `x` and the two dates on `colour`, with the data long, one row per
#' category and date. The arrow runs from the first level of the colour
#' scale to the second, in `data-context`, and stops short of the markers.
#' A category with only one of the two values gets its marker and no arrow.
#'
#' To order the categories by their second value, as the web chart does,
#' reorder the factor: `y = reorder(area, value, FUN = last)`, or compute
#' the order from the data first. `gla_zero_line("x")` adds the zero rule
#' when the values cross it.
#'
#' @inheritParams ggplot2::layer
#' @param ... Other arguments passed on to [ggplot2::layer()], such as a
#'   fixed `size` for the markers.
#' @param theme `"light"` or `"dark"`, for the arrow's colour. Defaults to
#'   the mode set by [gla_mode()].
#' @param arrow Whether the connector ends in an arrowhead. `FALSE` draws a
#'   plain line.
#' @param inset How far the connector stops short of each marker, in CSS
#'   pixels.
#' @param na.rm Whether to drop missing values silently.
#' @return A list of two layers, the connectors and the markers.
#' @examples
#' library(ggplot2)
#' d <- gla_yearly[gla_yearly$year %in% c(2015, 2024), ]
#' d$year <- factor(d$year)
#' ggplot(d, aes(average, reorder(series, average), colour = year)) +
#'   geom_gla_paired() +
#'   scale_colour_gla() +
#'   gla_labs(title = "Every variable fell", subtitle = "Yearly average, 2015 and 2024",
#'            x = NULL, y = NULL) +
#'   theme_gla(gridlines = "x")
#' @export
geom_gla_paired <- function(mapping = NULL, data = NULL, ..., theme = gla_mode(),
                            arrow = TRUE, inset = 7, na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE) {
  theme <- match_mode(theme)
  head <- if (isTRUE(arrow)) {
    grid::arrow(angle = 28, length = grid::unit(px_pt(15), "pt"), type = "open")
  }
  list(
    ggplot2::layer(
      data = data, mapping = mapping, stat = StatGlaPaired, geom = GeomGlaConnector,
      position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, arrow = head, inset = px_pt(inset),
                    context = unname(gla_token("data-context", theme)))
    ),
    ggplot2::layer(
      data = data, mapping = mapping, stat = "identity", geom = GeomGlaMarker,
      position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  )
}

#' @rdname geom_gla_paired
#' @format NULL
#' @usage NULL
#' @export
StatGlaPaired <- ggplot2::ggproto(
  "StatGlaPaired", ggplot2::Stat,
  required_aes = c("x", "y", "colour"),
  # one segment per category, from the value at the first colour level to
  # the value at the second; a category without both is left out
  compute_panel = function(data, scales) {
    if (!is.null(scales$colour) && scales$colour$is_discrete()) {
      levels <- scales$colour$get_limits()
      rank <- match(as.character(data$colour), levels)
    } else {
      rank <- data$colour
    }
    data <- data[order(data$y, rank), , drop = FALSE]
    pieces <- split(data, data$y)
    rows <- lapply(pieces, function(d) {
      d <- d[!is.na(d$x), , drop = FALSE]
      if (nrow(d) < 2) return(NULL)
      data.frame(x = d$x[1], xend = d$x[nrow(d)], y = d$y[1], yend = d$y[1],
                 PANEL = d$PANEL[1], group = -1L)
    })
    out <- do.call(rbind, rows)
    if (is.null(out)) return(data.frame())
    out
  }
)

#' @rdname geom_gla_paired
#' @format NULL
#' @usage NULL
#' @export
GeomGlaConnector <- ggplot2::ggproto(
  "GeomGlaConnector", ggplot2::GeomSegment,
  default_aes = ggplot2::aes(
    colour = ggplot2::from_theme(ink), linewidth = ggplot2::from_theme(borderwidth),
    linetype = 1, alpha = NA
  ),
  # ggplot2's segment, shortened at both ends by the inset so the arrow
  # does not run into the markers, in the context colour the layer was
  # given (a constant colour on the layer would hide the mapped colour
  # the stat pairs by)
  draw_panel = function(self, data, panel_params, coord, arrow = NULL, inset = 0,
                        context = NULL, lineend = "butt", linejoin = "round",
                        na.rm = FALSE) {
    data <- ggplot2::remove_missing(data, na.rm = na.rm,
                                    c("x", "y", "xend", "yend", "linetype", "linewidth"),
                                    name = "geom_gla_paired")
    if (nrow(data) == 0) return(ggplot2::zeroGrob())
    coords <- coord$transform(data, panel_params)
    if (!is.null(context)) coords$colour <- context
    step <- sign(coords$xend - coords$x)
    x0 <- grid::unit(coords$x, "npc") + grid::unit(step * inset, "pt")
    x1 <- grid::unit(coords$xend, "npc") - grid::unit(step * inset, "pt")
    grid::segmentsGrob(
      x0, grid::unit(coords$y, "npc"), x1, grid::unit(coords$yend, "npc"),
      default.units = "npc",
      gp = grid::gpar(
        col = scales::alpha(coords$colour, coords$alpha),
        fill = scales::alpha(coords$colour, coords$alpha),
        lwd = coords$linewidth * ggplot2::.pt, lty = coords$linetype,
        lineend = lineend, linejoin = linejoin
      ),
      arrow = arrow
    )
  }
)
