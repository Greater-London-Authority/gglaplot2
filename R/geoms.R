# The marks the visualisation team's charts draw, as ggplot2 layers. Each
# reads its default colour, width and font from the theme (ggplot2 4's
# element_geom), so theme_gla(theme = "dark") recolours them without a
# change to the layer: accent is data-primary, paper is chart-surface and
# ink is chart-label.

#' A line in the house style
#'
#' A 2px line in `data-primary` with round ends, the default line mark of
#' the web charts. Colour it by series with `aes(colour = )` and
#' [scale_colour_gla()].
#'
#' @inheritParams ggplot2::geom_line
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @return A ggplot2 layer.
#' @examples
#' library(ggplot2)
#' ggplot(gla_yearly, aes(year, average, colour = series)) +
#'   geom_gla_line() +
#'   scale_colour_gla() +
#'   theme_gla()
#' @export
geom_gla_line <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", ..., na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomGlaLine,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, lineend = "round", ...)
  )
}

#' @rdname geom_gla_line
#' @format NULL
#' @usage NULL
#' @export
GeomGlaLine <- ggplot2::ggproto(
  "GeomGlaLine", ggplot2::GeomLine,
  default_aes = ggplot2::aes(
    colour = ggplot2::from_theme(accent), linewidth = ggplot2::from_theme(linewidth),
    linetype = ggplot2::from_theme(linetype), alpha = NA
  )
)

#' Dots and markers in the house style
#'
#' `geom_gla_dot()` is the small, slightly transparent dot the web charts
#' use for a dense scatter: `data-primary`, no edge. `geom_gla_marker()` is
#' the larger hollow marker they use to pick out a point on a line, with a
#' 2px edge in the series colour and a `chart-surface` fill.
#'
#' @inheritParams ggplot2::geom_point
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @return A ggplot2 layer.
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_gla_dot() + theme_gla()
#' ggplot(gla_yearly, aes(year, average, colour = series)) +
#'   geom_gla_line() +
#'   geom_gla_marker() +
#'   scale_colour_gla() +
#'   theme_gla()
#' @name geom_gla_dot
NULL

#' @rdname geom_gla_dot
#' @export
geom_gla_dot <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", ..., na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomGlaDot,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname geom_gla_dot
#' @export
geom_gla_marker <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", ..., na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomGlaMarker,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname geom_gla_dot
#' @format NULL
#' @usage NULL
#' @export
GeomGlaDot <- ggplot2::ggproto(
  "GeomGlaDot", ggplot2::GeomPoint,
  default_aes = ggplot2::aes(
    shape = 19, colour = ggplot2::from_theme(accent), size = 1.5, fill = NA,
    alpha = 0.7, stroke = 0
  )
)

#' @rdname geom_gla_dot
#' @format NULL
#' @usage NULL
#' @export
GeomGlaMarker <- ggplot2::ggproto(
  "GeomGlaMarker", ggplot2::GeomPoint,
  default_aes = ggplot2::aes(
    shape = 21, colour = ggplot2::from_theme(accent), fill = ggplot2::from_theme(paper),
    size = 2.5, alpha = NA, stroke = 1
  )
)

#' Bars and columns in the house style
#'
#' Bars filled with `data-primary` and edged in `chart-surface`, so stacked
#' segments and neighbouring bars keep a hairline of surface between them.
#' The default width leaves half the slot empty, as on the web charts.
#' `geom_gla_col()` draws the value as given; `geom_gla_bar()` counts.
#' For horizontal bars map the category to `y` and use
#' `theme_gla(gridlines = "x")`.
#'
#' @inheritParams ggplot2::geom_col
#' @param width Bar width as a share of the slot. The default leaves a
#'   little under half the slot empty.
#' @param stat For `geom_gla_bar()`, the statistical transformation:
#'   `"count"` by default, `"bin"` for a histogram.
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @return A ggplot2 layer.
#' @examples
#' library(ggplot2)
#' ggplot(gla_yearly[gla_yearly$series == "Variable A", ], aes(factor(year), average)) +
#'   geom_gla_col() +
#'   theme_gla()
#' @name geom_gla_col
NULL

#' @rdname geom_gla_col
#' @export
geom_gla_col <- function(mapping = NULL, data = NULL, position = "stack", ...,
                         width = 0.6, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = "identity", geom = GeomGlaBar,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(width = width, na.rm = na.rm, ...)
  )
}

#' @rdname geom_gla_col
#' @export
geom_gla_bar <- function(mapping = NULL, data = NULL, stat = "count", position = "stack", ...,
                         width = 0.6, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomGlaBar,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(width = width, na.rm = na.rm, ...)
  )
}

#' @rdname geom_gla_col
#' @format NULL
#' @usage NULL
#' @export
GeomGlaBar <- ggplot2::ggproto(
  "GeomGlaBar", ggplot2::GeomBar,
  default_aes = ggplot2::aes(
    colour = ggplot2::from_theme(paper), fill = ggplot2::from_theme(accent),
    linewidth = ggplot2::from_theme(borderwidth), linetype = ggplot2::from_theme(bordertype),
    alpha = NA, width = 0.6
  )
)

#' An area in the house style
#'
#' A `data-primary` fill at a fifth opacity with no edge, the web charts'
#' area mark. Draw the line over it with [geom_gla_line()].
#'
#' @inheritParams ggplot2::geom_area
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @return A ggplot2 layer.
#' @examples
#' library(ggplot2)
#' a <- gla_monthly[gla_monthly$series == "Variable A", ]
#' ggplot(a, aes(month, value)) + geom_gla_area() + geom_gla_line() + theme_gla()
#' @export
geom_gla_area <- function(mapping = NULL, data = NULL, stat = "align",
                          position = "identity", ..., na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomGlaArea,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, outline.type = "upper", ...)
  )
}

#' @rdname geom_gla_area
#' @format NULL
#' @usage NULL
#' @export
GeomGlaArea <- ggplot2::ggproto(
  "GeomGlaArea", ggplot2::GeomArea,
  default_aes = ggplot2::aes(
    colour = NA, fill = ggplot2::from_theme(accent), linewidth = 0, linetype = 1, alpha = 0.2
  )
)

#' Annotation text in the house style
#'
#' Text drawn in `chart-label` at the label size, in the theme's family, as
#' the web charts annotate a point or a range. [ggplot2::geom_text()] with
#' the design system's defaults; use it for the words on a chart that are
#' not an axis or a legend.
#'
#' @inheritParams ggplot2::geom_text
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @return A ggplot2 layer.
#' @examples
#' library(ggplot2)
#' ggplot(gla_yearly, aes(year, average, colour = series)) +
#'   geom_gla_line() +
#'   geom_gla_text(aes(label = series), where = "max", stat = "gla_end", vjust = -0.6) +
#'   scale_colour_gla() +
#'   theme_gla(legend = "none")
#' @export
geom_gla_text <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", ..., na.rm = FALSE,
                          show.legend = FALSE, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomGlaText,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname geom_gla_text
#' @format NULL
#' @usage NULL
#' @export
GeomGlaText <- ggplot2::ggproto(
  "GeomGlaText", ggplot2::GeomText,
  default_aes = ggplot2::aes(
    colour = ggplot2::from_theme(ink), family = ggplot2::from_theme(family),
    size = ggplot2::from_theme(fontsize), angle = 0, hjust = 0.5, vjust = 0.5,
    alpha = NA, fontface = 1, lineheight = 1.2
  )
)

# ── end of line ─────────────────────────────────────────────────────────────

#' One point per series: the end, the start, the high or the low
#'
#' Keeps one row per group. Used by [geom_gla_endpoint()] and
#' [geom_gla_endlabel()], and available for any geom with
#' `stat = "gla_end"`.
#'
#' @param where `"end"` (the largest x), `"start"`, `"max"` (the largest y)
#'   or `"min"`.
#' @inheritParams ggplot2::stat_identity
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @return A ggplot2 layer.
#' @examples
#' library(ggplot2)
#' ggplot(gla_yearly, aes(year, average, colour = series)) +
#'   geom_gla_line() +
#'   stat_gla_end(where = "max", geom = "point") +
#'   theme_gla()
#' @export
stat_gla_end <- function(mapping = NULL, data = NULL, geom = "point",
                         position = "identity", ..., where = c("end", "start", "max", "min"),
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = StatGlaEnd, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(where = match.arg(where), na.rm = na.rm, ...)
  )
}

#' @rdname stat_gla_end
#' @format NULL
#' @usage NULL
#' @export
StatGlaEnd <- ggplot2::ggproto(
  "StatGlaEnd", ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales, where = "end", x_filt = NULL, y_filt = NULL) {
    data <- data[!is.na(data$x) & !is.na(data$y), , drop = FALSE]
    if (nrow(data) == 0) return(data)
    if (where == "xy") {
      # gglaplot's filter_type = "xy": the rows at a given x and/or y
      keep <- rep(TRUE, nrow(data))
      if (!is.null(x_filt)) keep <- keep & data$x %in% as.numeric(x_filt)
      if (!is.null(y_filt)) keep <- keep & data$y %in% as.numeric(y_filt)
      return(data[keep, , drop = FALSE])
    }
    i <- switch(where,
      end = which.max(data$x), start = which.min(data$x),
      max = which.max(data$y), min = which.min(data$y),
      stop("where must be end, start, max, min or xy", call. = FALSE)
    )
    data[i, , drop = FALSE]
  }
)

#' Mark and label the end of each line
#'
#' The web line charts finish each line with a hollow marker and the series
#' name beside it, instead of a legend. `geom_gla_endpoint()` draws the
#' marker and `geom_gla_endlabel()` the name, 8px to the right of the last
#' point. Labels that would overlap are pushed apart vertically, each
#' staying as close to its line as the others allow. Map the name to
#' `label` and the series to `colour`, and leave room with
#' `scale_x_*(expand = expansion(mult = c(0, 0.2)))` or a plot margin.
#'
#' @inheritParams ggplot2::geom_text
#' @param where Which point: `"end"`, `"start"`, `"max"` or `"min"`.
#' @param dx Gap between the point and the label, in pixels.
#' @param dodge Push overlapping labels apart.
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @return A ggplot2 layer.
#' @examples
#' library(ggplot2)
#' ggplot(gla_yearly, aes(year, average, colour = series)) +
#'   geom_gla_line() +
#'   geom_gla_endpoint() +
#'   geom_gla_endlabel(aes(label = series)) +
#'   scale_x_continuous(expand = expansion(mult = c(0.02, 0.25))) +
#'   scale_colour_gla() +
#'   theme_gla(legend = "none")
#' @name geom_gla_endlabel
NULL

#' @rdname geom_gla_endlabel
#' @export
geom_gla_endpoint <- function(mapping = NULL, data = NULL, position = "identity", ...,
                              where = c("end", "start", "max", "min"), na.rm = FALSE,
                              show.legend = FALSE, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = StatGlaEnd, geom = GeomGlaMarker,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(where = match.arg(where), na.rm = na.rm, ...)
  )
}

#' @rdname geom_gla_endlabel
#' @export
geom_gla_endlabel <- function(mapping = NULL, data = NULL, position = "identity", ...,
                              where = c("end", "start", "max", "min"), dx = 8, dodge = TRUE,
                              na.rm = FALSE, show.legend = FALSE, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = StatGlaEnd, geom = GeomGlaEndlabel,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(where = match.arg(where), dx = dx, dodge = dodge, na.rm = na.rm, ...)
  )
}

#' @rdname geom_gla_endlabel
#' @format NULL
#' @usage NULL
#' @export
GeomGlaEndlabel <- ggplot2::ggproto(
  "GeomGlaEndlabel", ggplot2::GeomText,
  required_aes = c("x", "y", "label"),
  default_aes = ggplot2::aes(
    colour = ggplot2::from_theme(ink), family = ggplot2::from_theme(family),
    size = ggplot2::from_theme(fontsize), angle = 0, hjust = 0, vjust = 0.5,
    alpha = NA, fontface = 1, lineheight = 1.1
  ),
  draw_panel = function(data, panel_params, coord, dx = 8, dodge = TRUE, na.rm = FALSE) {
    data <- coord$transform(data, panel_params)
    grob <- grid::textGrob(
      data$label,
      x = grid::unit(data$x, "native") + grid::unit(px_pt(dx), "pt"),
      y = grid::unit(data$y, "native"),
      hjust = data$hjust, vjust = data$vjust, rot = data$angle,
      gp = grid::gpar(
        col = ggplot2::alpha(data$colour, data$alpha),
        fontsize = data$size * ggplot2::.pt, fontfamily = data$family,
        fontface = data$fontface, lineheight = data$lineheight
      )
    )
    if (!dodge || nrow(data) < 2) return(grob)
    # the spacing is a label's height; the push happens at draw time, when
    # the panel's height in points is known
    grid::gTree(children = grid::gList(grob),
                spacing = max(data$size) * ggplot2::.pt * 1.2, cl = "gla_dodge")
  },
  draw_key = ggplot2::draw_key_text
)

#' @exportS3Method grid::makeContent
makeContent.gla_dodge <- function(x) {
  g <- x$children[[1]]
  y <- grid::convertY(g$y, "pt", valueOnly = TRUE)
  height <- grid::convertHeight(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
  g$y <- grid::unit(separate_labels(y, x$spacing, x$spacing / 2, height - x$spacing / 2), "pt")
  grid::setChildren(x, grid::gList(g))
}

#' Push overlapping labels apart
#'
#' Moves positions so neighbours are at least `spacing` apart while each
#' stays as close as possible to where it was, and all stay within
#' `[lower, upper]`. Overlapping labels are merged into clusters laid out as
#' an evenly spaced stack centred on the mean of the members' original
#' positions. The web charts' end-of-line label dodge, in one dimension.
#'
#' @param y Positions, any unit.
#' @param spacing The minimum gap between neighbours.
#' @param lower,upper The bounds positions must stay within.
#' @return Positions in the same order as `y`.
#' @examples
#' separate_labels(c(10, 12, 50), spacing = 8, lower = 0, upper = 100)
#' @export
separate_labels <- function(y, spacing, lower = -Inf, upper = Inf) {
  ok <- which(is.finite(y))
  if (length(ok) < 2) return(y)
  ord <- ok[order(y[ok])]
  ys <- y[ord]
  # each cluster: a run of consecutive labels, with the sum of
  # (original - k * spacing) over its members, so the best top is sum / n
  clusters <- list()
  top <- function(c) c$sum / c$n
  for (k in seq_along(ys)) {
    cl <- list(start = k, n = 1L, sum = ys[k])
    while (length(clusters) > 0) {
      prev <- clusters[[length(clusters)]]
      if (top(prev) + prev$n * spacing <= top(cl)) break
      cl <- list(start = prev$start, n = prev$n + cl$n,
                 sum = prev$sum + cl$sum - cl$n * prev$n * spacing)
      clusters[[length(clusters)]] <- NULL
    }
    clusters[[length(clusters) + 1]] <- cl
  }
  out <- ys
  for (cl in clusters) {
    for (k in seq_len(cl$n)) out[cl$start + k - 1] <- top(cl) + (k - 1) * spacing
  }
  # keep inside the bounds without losing the spacing
  out[1] <- max(out[1], lower)
  for (k in seq_along(out)[-1]) out[k] <- max(out[k], out[k - 1] + spacing)
  n <- length(out)
  out[n] <- min(out[n], upper)
  for (k in rev(seq_len(n - 1))) out[k] <- min(out[k], out[k + 1] - spacing)
  y[ord] <- out
  y
}

# ── rules and annotations ───────────────────────────────────────────────────

#' A baseline at zero
#'
#' The web charts draw a rule at zero whenever the value axis crosses or
#' starts at it, in `chart-axis`, so the baseline reads as the axis. Use it
#' on a chart whose value axis starts at zero or runs through it.
#'
#' @param axis The axis the baseline belongs to: `"y"` draws a horizontal
#'   rule at y = 0, `"x"` a vertical one at x = 0.
#' @return A ggplot2 layer.
#' @examples
#' library(ggplot2)
#' ggplot(gla_yearly[gla_yearly$series == "Variable A", ], aes(factor(year), average)) +
#'   geom_gla_col() +
#'   gla_zero_line() +
#'   theme_gla()
#' @export
gla_zero_line <- function(axis = c("y", "x")) {
  axis <- match.arg(axis)
  if (axis == "y") {
    ggplot2::layer(
      data = data.frame(yintercept = 0), mapping = ggplot2::aes(yintercept = .data$yintercept),
      stat = "identity", geom = GeomGlaHline, position = "identity",
      show.legend = FALSE, inherit.aes = FALSE, params = list(na.rm = TRUE)
    )
  } else {
    ggplot2::layer(
      data = data.frame(xintercept = 0), mapping = ggplot2::aes(xintercept = .data$xintercept),
      stat = "identity", geom = GeomGlaVline, position = "identity",
      show.legend = FALSE, inherit.aes = FALSE, params = list(na.rm = TRUE)
    )
  }
}

GeomGlaHline <- ggplot2::ggproto(
  "GeomGlaHline", ggplot2::GeomHline,
  default_aes = ggplot2::aes(
    colour = ggplot2::from_theme(ink), linewidth = ggplot2::from_theme(borderwidth),
    linetype = 1, alpha = NA
  )
)

GeomGlaVline <- ggplot2::ggproto(
  "GeomGlaVline", ggplot2::GeomVline,
  default_aes = ggplot2::aes(
    colour = ggplot2::from_theme(ink), linewidth = ggplot2::from_theme(borderwidth),
    linetype = 1, alpha = NA
  )
)

#' A labelled reference line
#'
#' A dashed 2px rule in `data-context` grey across the plot at a value,
#' with its label sitting on the line at the right edge, as the web line
#' charts draw a target or an average. Give `x` instead of `y` for a
#' vertical rule at a date or a value, labelled at the top. The line reads
#' its colour from the theme's surface, so it holds in dark mode.
#'
#' @param y The value a horizontal line marks.
#' @param label Text for the line. `NULL` for none.
#' @param theme `"light"` or `"dark"`, to colour the rule.
#' @param x The value a vertical line marks, instead of `y`.
#' @return A list of ggplot2 layers.
#' @examples
#' library(ggplot2)
#' ggplot(gla_yearly, aes(year, average, colour = series)) +
#'   geom_gla_line() +
#'   gla_reference_line(30000, "Target") +
#'   gla_reference_line(x = 2020, label = "Pandemic") +
#'   scale_colour_gla() +
#'   theme_gla()
#' @export
gla_reference_line <- function(y = NULL, label = NULL, theme = c("light", "dark"), x = NULL) {
  theme <- match.arg(theme)
  if (is.null(y) == is.null(x)) stop("Give one of y or x", call. = FALSE)
  rule <- list(na.rm = TRUE, colour = unname(gla_token("data-context", theme)),
               linewidth = px(2), linetype = "42")
  if (!is.null(y)) {
    layers <- list(ggplot2::layer(
      data = data.frame(yintercept = y), mapping = ggplot2::aes(yintercept = .data$yintercept),
      stat = "identity", geom = GeomGlaHline, position = "identity",
      show.legend = FALSE, inherit.aes = FALSE, params = rule
    ))
    if (!is.null(label)) {
      layers <- c(layers, list(ggplot2::layer(
        data = data.frame(y = y, label = label),
        mapping = ggplot2::aes(y = .data$y, label = .data$label),
        stat = "identity", geom = GeomGlaEdgelabel, position = "identity",
        show.legend = FALSE, inherit.aes = FALSE, params = list(na.rm = TRUE)
      )))
    }
    return(layers)
  }
  layers <- list(ggplot2::layer(
    data = data.frame(xintercept = x), mapping = ggplot2::aes(xintercept = .data$xintercept),
    stat = "identity", geom = GeomGlaVline, position = "identity",
    show.legend = FALSE, inherit.aes = FALSE, params = rule
  ))
  if (!is.null(label)) {
    layers <- c(layers, list(ggplot2::layer(
      data = data.frame(x = x, label = label),
      mapping = ggplot2::aes(x = .data$x, label = .data$label),
      stat = "identity", geom = GeomGlaToplabel, position = "identity",
      show.legend = FALSE, inherit.aes = FALSE, params = list(na.rm = TRUE)
    )))
  }
  layers
}

# text at the right edge of the panel, just above its y
GeomGlaEdgelabel <- ggplot2::ggproto(
  "GeomGlaEdgelabel", ggplot2::GeomText,
  required_aes = c("y", "label"),
  default_aes = ggplot2::aes(
    colour = ggplot2::from_theme(ink), family = ggplot2::from_theme(family),
    size = ggplot2::from_theme(fontsize), alpha = NA, fontface = 1
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data$x <- panel_params$x.range[2]
    data <- coord$transform(data, panel_params)
    grid::textGrob(
      data$label, x = grid::unit(1, "npc") - grid::unit(px_pt(4), "pt"),
      y = grid::unit(data$y, "native") + grid::unit(px_pt(4), "pt"),
      hjust = 1, vjust = 0,
      gp = grid::gpar(col = ggplot2::alpha(data$colour, data$alpha),
                      fontsize = data$size * ggplot2::.pt, fontfamily = data$family,
                      fontface = data$fontface)
    )
  }
)

# text at the top of the panel, just right of its x
GeomGlaToplabel <- ggplot2::ggproto(
  "GeomGlaToplabel", ggplot2::GeomText,
  required_aes = c("x", "label"),
  default_aes = ggplot2::aes(
    colour = ggplot2::from_theme(ink), family = ggplot2::from_theme(family),
    size = ggplot2::from_theme(fontsize), alpha = NA, fontface = 1
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data$y <- panel_params$y.range[2]
    data <- coord$transform(data, panel_params)
    grid::textGrob(
      data$label, x = grid::unit(data$x, "native") + grid::unit(px_pt(4), "pt"),
      y = grid::unit(1, "npc") - grid::unit(px_pt(4), "pt"),
      hjust = 0, vjust = 1,
      gp = grid::gpar(col = ggplot2::alpha(data$colour, data$alpha),
                      fontsize = data$size * ggplot2::.pt, fontfamily = data$family,
                      fontface = data$fontface)
    )
  }
)

#' A shaded range behind the data
#'
#' A `chart-label` wash at a tenth opacity between two values, the web
#' charts' annotation range: a recession, a forecast period, a target band.
#' Leave an end open with `-Inf` or `Inf`.
#'
#' @param xmin,xmax,ymin,ymax The extent of the range, in data units.
#' @param theme `"light"` or `"dark"`.
#' @return A ggplot2 layer.
#' @examples
#' library(ggplot2)
#' ggplot(gla_yearly, aes(year, average, colour = series)) +
#'   annotate_gla_range(xmin = 2020, xmax = 2021.5) +
#'   geom_gla_line() +
#'   scale_colour_gla() +
#'   theme_gla()
#' @export
annotate_gla_range <- function(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
                               theme = c("light", "dark")) {
  theme <- match.arg(theme)
  ggplot2::annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                    fill = unname(gla_token("chart-label", theme)), alpha = 0.1, colour = NA)
}

# ── maps ────────────────────────────────────────────────────────────────────

#' Boundaries in the house style
#'
#' [ggplot2::geom_sf()] with the design system's defaults: a `data-empty`
#' fill, a 1px `chart-surface` edge between areas, and no graticule. Map
#' `fill` and add [scale_fill_gla_c()] for a choropleth. Needs the sf
#' package.
#'
#' @inheritParams ggplot2::geom_sf
#' @param theme `"light"` or `"dark"`.
#' @param ... Other arguments passed to [ggplot2::geom_sf()].
#' @return A list of a ggplot2 layer and a coord.
#' @examples
#' library(ggplot2)
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   ggplot(gla_boroughs) + geom_gla_sf() + theme_gla_map()
#' }
#' @export
geom_gla_sf <- function(mapping = ggplot2::aes(), data = NULL, ..., theme = c("light", "dark"),
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  theme <- match.arg(theme)
  dots <- list(...)
  if (is.null(dots$fill) && !"fill" %in% names(mapping)) {
    dots$fill <- unname(gla_token("data-empty", theme))
  }
  if (is.null(dots$colour)) dots$colour <- unname(gla_token("chart-surface", theme))
  if (is.null(dots$linewidth)) dots$linewidth <- px(1)
  list(
    do.call(ggplot2::geom_sf, c(list(mapping = mapping, data = data, na.rm = na.rm,
                                     show.legend = show.legend, inherit.aes = inherit.aes), dots)),
    ggplot2::coord_sf(datum = NA)
  )
}
