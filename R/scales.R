#' GLA colour and fill scales for ggplot2
#'
#' Discrete scales draw from [gla_pal()] in palette order, so the first
#' series is `data-primary`, the second `data-secondary` and so on. Pass
#' `type = "sex"` for a women and men split, which maps by level name rather
#' than position so the colours never swap when the levels reorder.
#' Continuous scales use the sequential or diverging ramp.
#'
#' `scale_color_*` spellings are provided as aliases.
#'
#' @param type A palette type as in [gla_pal()]. Discrete scales take
#'   `"categorical"`, `"sex"` or `"change"`; continuous ones `"sequential"`
#'   or `"diverging"`.
#' @param theme `"light"` or `"dark"`.
#' @param ... Passed to the underlying ggplot2 scale.
#' @return A ggplot2 scale.
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   scale_colour_gla() +
#'   theme_gla()
#' @name scale_gla
NULL

# a palette function for discrete_scale: by level name for the named
# palettes, by position otherwise
.discrete_pal <- function(type, theme) {
  pal <- gla_pal(type, theme = theme)
  function(n) {
    if (n > length(pal)) {
      stop(sprintf("The %s palette has %d colours; the data has %d levels", type, length(pal), n),
           call. = FALSE)
    }
    unname(pal[seq_len(n)])
  }
}

# ggplot2 matches a named vector of values to levels by name, which is what
# the sex and change palettes want
.named_scale <- function(aesthetic, type, theme, ...) {
  ggplot2::scale_discrete_manual(aesthetic, values = gla_pal(type, theme = theme), ...)
}

#' @rdname scale_gla
#' @export
scale_colour_gla <- function(type = c("categorical", "sex", "change"),
                             theme = c("light", "dark"), ...) {
  type <- match.arg(type)
  theme <- match.arg(theme)
  if (type != "categorical") return(.named_scale("colour", type, theme, ...))
  ggplot2::discrete_scale("colour", palette = .discrete_pal(type, theme), ...)
}

#' @rdname scale_gla
#' @export
scale_fill_gla <- function(type = c("categorical", "sex", "change"),
                           theme = c("light", "dark"), ...) {
  type <- match.arg(type)
  theme <- match.arg(theme)
  if (type != "categorical") return(.named_scale("fill", type, theme, ...))
  ggplot2::discrete_scale("fill", palette = .discrete_pal(type, theme), ...)
}

#' @rdname scale_gla
#' @export
scale_colour_gla_c <- function(type = c("sequential", "diverging"),
                               theme = c("light", "dark"), ...) {
  type <- match.arg(type)
  theme <- match.arg(theme)
  ggplot2::scale_colour_gradientn(colours = gla_pal(type, theme = theme), ...)
}

#' @rdname scale_gla
#' @export
scale_fill_gla_c <- function(type = c("sequential", "diverging"),
                             theme = c("light", "dark"), ...) {
  type <- match.arg(type)
  theme <- match.arg(theme)
  ggplot2::scale_fill_gradientn(colours = gla_pal(type, theme = theme), ...)
}

#' @rdname scale_gla
#' @export
scale_color_gla <- scale_colour_gla

#' @rdname scale_gla
#' @export
scale_color_gla_c <- scale_colour_gla_c
