#' GLA colour and fill scales for ggplot2
#'
#' Discrete scales colour the series the way the visualisation team's charts
#' do. `type = "auto"`, the default, reads the level names: London against
#' one comparator is primary against context grey, Domestic against
#' International and Inner against Outer are primary against secondary, and
#' anything else takes the categorical palette in order (see
#' [gla_series_colours()]). `"categorical"` is the palette in order with no
#' reading of names. `"sex"` maps women and men by level name so the colours
#' never swap. `"change"` maps `up` and `down`.
#'
#' Continuous scales use the sequential or diverging ramp. Missing values
#' take `data-empty`.
#'
#' `scale_color_*` spellings are provided as aliases.
#'
#' @param type A palette as above. Discrete scales take `"auto"`,
#'   `"categorical"`, `"sex"` or `"change"`; continuous ones `"sequential"`
#'   or `"diverging"`.
#' @param theme `"light"` or `"dark"`. Defaults to the mode set by [gla_mode()].
#' @param ... Passed to the underlying ggplot2 scale.
#' @return A ggplot2 scale.
#' @examples
#' library(ggplot2)
#' ggplot(gla_yearly, aes(year, average, colour = series)) +
#'   geom_gla_line() +
#'   scale_colour_gla() +
#'   theme_gla()
#' @name scale_gla
NULL

# a discrete scale that colours by level name rather than by position, so
# the series rules can see what the levels are
ScaleDiscreteGla <- ggplot2::ggproto(
  "ScaleDiscreteGla", ggplot2::ScaleDiscrete,
  theme_mode = "light",
  map = function(self, x, limits = self$get_limits()) {
    limits <- limits[!is.na(limits)]
    pal <- gla_series_colours(limits, self$theme_mode)
    cols <- unname(pal[match(as.character(x), limits)])
    ifelse(is.na(cols), self$na.value, cols)
  }
)

.gla_discrete <- function(aesthetic, type, theme, ...) {
  empty <- unname(gla_token("data-empty", theme))
  if (type == "auto") {
    sc <- ggplot2::discrete_scale(
      aesthetic, palette = function(n) gla_pal("categorical", n, theme),
      na.value = empty, super = ScaleDiscreteGla, ...
    )
    sc$theme_mode <- theme
    return(sc)
  }
  if (type == "categorical") {
    pal <- gla_pal(type, theme = theme)
    return(ggplot2::discrete_scale(aesthetic, palette = function(n) {
      if (n > length(pal)) {
        stop(sprintf("The categorical palette has %d colours; the data has %d levels. %s",
                     length(pal), n, "Fold the categories"), call. = FALSE)
      }
      unname(pal[seq_len(n)])
    }, na.value = empty, ...))
  }
  # ggplot2 matches a named vector of values to levels by name, which is
  # what the sex and change palettes want
  ggplot2::scale_discrete_manual(aesthetic, values = gla_pal(type, theme = theme),
                                 na.value = empty, ...)
}

#' @rdname scale_gla
#' @export
scale_colour_gla <- function(type = c("auto", "categorical", "sex", "change"),
                             theme = gla_mode(), ...) {
  .gla_discrete("colour", match.arg(type), match_mode(theme), ...)
}

#' @rdname scale_gla
#' @export
scale_fill_gla <- function(type = c("auto", "categorical", "sex", "change"),
                           theme = gla_mode(), ...) {
  .gla_discrete("fill", match.arg(type), match_mode(theme), ...)
}

#' @rdname scale_gla
#' @export
scale_colour_gla_c <- function(type = c("sequential", "diverging"),
                               theme = gla_mode(), ...) {
  type <- match.arg(type)
  theme <- match_mode(theme)
  ggplot2::scale_colour_gradientn(colours = gla_pal(type, theme = theme),
                                  na.value = unname(gla_token("data-empty", theme)), ...)
}

#' @rdname scale_gla
#' @export
scale_fill_gla_c <- function(type = c("sequential", "diverging"),
                             theme = gla_mode(), ...) {
  type <- match.arg(type)
  theme <- match_mode(theme)
  ggplot2::scale_fill_gradientn(colours = gla_pal(type, theme = theme),
                                na.value = unname(gla_token("data-empty", theme)), ...)
}

#' @rdname scale_gla
#' @export
scale_color_gla <- scale_colour_gla

#' @rdname scale_gla
#' @export
scale_color_gla_c <- scale_colour_gla_c

#' Line styles for when colour cannot carry the series
#'
#' The web charts tell two sets of lines apart with a dash: solid for the
#' first, dashed for the second, as in a chart of a measure and its
#' average, or an actual and a target. `scale_linetype_gla()` maps a
#' variable to that set, solid first. `gla_linetypes()` is the set itself,
#' as ggplot2 linetype strings in units of the line's width, so a 2px line
#' dashes 4px on, 4px off as the web's does. The `projected` style is the
#' forecast dash [geom_gla_line()] draws.
#'
#' The web only has the two; the third and fourth styles are here so a
#' scale with more levels still draws, but a chart that needs them is
#' usually better faceted.
#'
#' @param n How many styles, or `NULL` for all of them.
#' @param ... Passed to [ggplot2::discrete_scale()].
#' @return `gla_linetypes()` a named character vector of linetypes;
#'   `scale_linetype_gla()` a ggplot2 scale.
#' @examples
#' library(ggplot2)
#' d <- rbind(
#'   cbind(gla_yearly, kind = "Yearly average"),
#'   cbind(transform(gla_yearly, average = mean(average)), kind = "Mean")
#' )
#' ggplot(d, aes(year, average, colour = series, linetype = kind)) +
#'   geom_gla_line() +
#'   scale_colour_gla() +
#'   scale_linetype_gla() +
#'   theme_gla()
#' @name scale_linetype_gla
NULL

#' @rdname scale_linetype_gla
#' @export
gla_linetypes <- function(n = NULL) {
  styles <- c(solid = "solid", dashed = "22", dotted = "12", dashdot = "4212", projected = "32")
  if (is.null(n)) return(styles)
  if (n > 4) stop("Four line styles are the most a chart can carry; facet instead", call. = FALSE)
  styles[seq_len(n)]
}

#' @rdname scale_linetype_gla
#' @export
scale_linetype_gla <- function(...) {
  ggplot2::discrete_scale("linetype", palette = function(n) unname(gla_linetypes(n)), ...)
}
