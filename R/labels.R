#' Title, subtitle and the footer, as the web charts carry them
#'
#' The chart container on the web shows a title, a subtitle, and a footer
#' with a byline, a source and a note. `gla_labs()` is [ggplot2::labs()]
#' with those names, writing the footer into the caption through
#' [gla_caption()]. `alt` is the short text alternative; see [gla_describe()]
#' for one written from the plot.
#'
#' @param title,subtitle,alt As in [ggplot2::labs()].
#' @param source Where the data came from.
#' @param note A footnote.
#' @param byline Who made the chart, e.g. `"GLA City Intelligence"`.
#' @param ... Other labels passed to [ggplot2::labs()].
#' @return A labels object to add to a plot.
#' @examples
#' library(ggplot2)
#' ggplot(gla_yearly, aes(year, average, colour = series)) +
#'   geom_gla_line() +
#'   scale_colour_gla() +
#'   gla_labs(
#'     title = "Variable A has fallen since 2017",
#'     subtitle = "Yearly average value, GBP",
#'     source = "LDN Viz Tools demo data", note = "Data for demonstration only",
#'     byline = "GLA City Intelligence"
#'   ) +
#'   theme_gla()
#' @export
gla_labs <- function(title = ggplot2::waiver(), subtitle = ggplot2::waiver(), source = NULL,
                     note = NULL, byline = NULL, alt = ggplot2::waiver(), ...) {
  caption <- gla_caption(source = source, note = note, byline = byline)
  ggplot2::labs(title = title, subtitle = subtitle, alt = alt,
                caption = if (nzchar(caption)) caption else ggplot2::waiver(), ...)
}

#' The chart footer as caption text
#'
#' One line each, in the web charts' order: byline, then `Source:`, then
#' `Note:`. Empty parts are left out.
#'
#' @inheritParams gla_labs
#' @return A single string, empty if nothing was given.
#' @examples
#' gla_caption(source = "ONS", note = "Provisional", byline = "GLA City Intelligence")
#' @export
gla_caption <- function(source = NULL, note = NULL, byline = NULL) {
  parts <- c(
    byline,
    if (!is.null(source)) paste0("Source: ", source),
    if (!is.null(note)) paste0("Note: ", note)
  )
  paste(parts, collapse = "\n")
}

#' Describe a chart in a sentence
#'
#' A text alternative written from the plot, in the form the visualisation
#' team's charts generate theirs: the chart type, the series it has lines or
#' bars for, and the range the x axis covers. "Line chart with lines for
#' London and England, showing values between 2015 and 2024." Set it as the
#' plot's `alt` with [gla_alt()], or start from it and say what the chart
#' shows. A description is a starting point, not a substitute for a title
#' that states the finding.
#'
#' @param plot A ggplot.
#' @return A single string.
#' @examples
#' library(ggplot2)
#' p <- ggplot(gla_yearly, aes(year, average, colour = series)) + geom_gla_line()
#' gla_describe(p)
#' @export
gla_describe <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  geoms <- vapply(plot$layers, function(l) class(l$geom)[1], character(1))
  stats <- vapply(plot$layers, function(l) class(l$stat)[1], character(1))
  kind <- if (any(grepl("Sf", geoms))) {
    "Map"
  } else if (any(stats == "StatBin")) {
    "Histogram"
  } else if (any(grepl("Line|Path|Step", geoms))) {
    "Line chart"
  } else if (any(grepl("Bar|Col", geoms))) {
    positions <- vapply(plot$layers, function(l) class(l$position)[1], character(1))
    if (any(positions == "PositionStack") && !is.null(series_scale(built, "fill"))) {
      "Stacked bar chart"
    } else if (any(positions == "PositionDodge")) {
      "Grouped bar chart"
    } else {
      "Bar chart"
    }
  } else if (any(grepl("Area", geoms))) {
    "Area chart"
  } else if (any(grepl("Point|Dot|Marker", geoms))) {
    "Scatter plot"
  } else {
    "Chart"
  }

  series <- series_scale(built, "colour") %||% series_scale(built, "fill")
  series_part <- if (!is.null(series) && length(series) > 0) {
    noun <- if (kind == "Line chart") "lines" else if (grepl("bar", kind)) "bars" else "series"
    if (length(series) == 1) {
      sprintf("with a single %s for %s", sub("s$", "", noun), series)
    } else {
      sprintf("with %s for %s", noun, join_names(series))
    }
  }

  x_part <- if (kind != "Map") x_range_statement(built)
  paste0(paste(c(kind, series_part, x_part), collapse = ", "), ".")
}

#' @rdname gla_describe
#' @return `gla_alt()` returns the plot with its `alt` label set.
#' @examples
#' p <- gla_alt(p)
#' ggplot2::get_alt_text(p)
#' @export
gla_alt <- function(plot) {
  plot + ggplot2::labs(alt = gla_describe(plot))
}

# the levels of a discrete colour or fill scale, if the plot has one
series_scale <- function(built, aesthetic) {
  sc <- built$plot$scales$get_scales(aesthetic)
  if (is.null(sc) || !sc$is_discrete()) return(NULL)
  lim <- sc$get_limits()
  as.character(lim[!is.na(lim)])
}

# "showing values between 2015 and 2024", or the categories on a discrete x
x_range_statement <- function(built) {
  sc <- built$layout$panel_scales_x[[1]]
  if (is.null(sc)) return(NULL)
  lim <- sc$get_limits()
  lim <- lim[!is.na(lim)]
  if (!length(lim)) return(NULL)
  if (sc$is_discrete()) {
    return(sprintf("showing values for %s", join_names(lim)))
  }
  rng <- range(lim)
  if (inherits(sc, "ScaleContinuousDate")) {
    rng <- format(as.Date(rng, origin = "1970-01-01"), "%Y")
  } else if (inherits(sc, "ScaleContinuousDatetime")) {
    rng <- format(as.POSIXct(rng, origin = "1970-01-01", tz = "UTC"), "%Y")
  } else if (all(rng == round(rng)) && all(abs(rng) < 10000)) {
    # whole numbers under ten thousand read as years or counts, not as 2,015
    rng <- format(rng, trim = TRUE)
  } else {
    rng <- format(signif(rng, 4), big.mark = ",", trim = TRUE)
  }
  sprintf("showing values between %s and %s", rng[1], rng[2])
}

# "A", "A and B", "A, B and C"
join_names <- function(x) {
  x <- as.character(x)
  n <- length(x)
  if (n == 0) return("")
  if (n == 1) return(x)
  paste0(paste(x[-n], collapse = ", "), " and ", x[n])
}
