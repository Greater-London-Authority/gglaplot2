#' GLA colour palettes
#'
#' Palettes built from the `data-*` and `palette-*` tokens, so a chart made
#' here matches a dashboard built on the same tokens.
#'
#' `"categorical"` leads with `data-primary`, `data-secondary` and
#' `data-tertiary`, then the differentiable extras, ordered by measured
#' OKLab separation. It validates clean to three series: at four the teal
#' reads grey and drops below 3:1 contrast on a white card, at five the
#' dark purple leaves the lightness band. Design to three and fold
#' categories rather than reach for a fourth colour. Red and yellow are held
#' back for good and bad.
#'
#' `"sequential"` is the blue ramp, light to dark, interpolated to `n`.
#' `"diverging"` runs red through a neutral grey to blue, the same colours
#' the `change` palette uses for down and up. `"sex"` is purple for women
#' and green for men, named by both spellings a source might use.
#' `"change"` is `up` (blue) and `down` (red): a value rising is coloured by
#' the token, not by arithmetic sign.
#'
#' @param type One of `"categorical"`, `"sequential"`, `"diverging"`,
#'   `"sex"`, `"change"`.
#' @param n Number of colours. Defaults to the full palette for the discrete
#'   types and five for the ramps. A categorical request past the palette is
#'   an error, not a recycled colour.
#' @param theme `"light"` or `"dark"`.
#' @return A character vector of hex colours, named for the discrete types.
#' @examples
#' gla_pal()
#' gla_pal("sequential", 7)
#' gla_pal("sex")
#' @export
gla_pal <- function(type = c("categorical", "sequential", "diverging", "sex", "change"),
                    n = NULL, theme = c("light", "dark")) {
  type <- match.arg(type)
  theme <- match.arg(theme)
  t <- function(x) unname(gla_token(x, theme))
  blues <- function(levels) t(paste0("palette-blue-", levels))

  pal <- switch(type,
    categorical = stats::setNames(
      t(c("data-primary", "data-secondary", "data-tertiary", "data-female",
          "data-male", "data-categorical-turquoise", "data-categorical-pink")),
      c("primary", "secondary", "tertiary", "quaternary", "quinary", "senary", "septenary")
    ),
    sequential = blues(c(100, 300, 500, 700, 900)),
    diverging = c(t(paste0("palette-red-", c(700, 400))), t("data-neutral-0"), blues(c(400, 700))),
    sex = stats::setNames(t(c("data-female", "data-male", "data-female", "data-male")),
                          c("Women", "Men", "Female", "Male")),
    change = stats::setNames(t(c("data-positive", "data-negative")), c("up", "down"))
  )

  if (is.null(n)) return(pal)
  if (type %in% c("sequential", "diverging")) {
    return(grDevices::colorRampPalette(pal)(n))
  }
  if (n > length(pal)) {
    stop(sprintf("The %s palette has %d colours; %d were asked for. Fold the categories",
                 type, length(pal), n), call. = FALSE)
  }
  pal[seq_len(n)]
}

#' The number of categorical series colour can carry on one chart
#'
#' Three is the recommended ceiling for a clean chart; six is the measured
#' one, past which the palette has no separable hues left. See [gla_pal()].
#'
#' @return An integer.
#' @export
gla_max_series <- function() 6L

#' Series colours by what the series are
#'
#' The rules the visualisation team's charts colour a series by, applied to
#' a set of level names. Two series where one is London and the other is not
#' get `data-primary` for London and `data-context` grey for the comparator,
#' so London is the subject and the other a reference. Domestic against
#' International, and Inner against Outer London, get primary and secondary.
#' Otherwise the categorical palette in order, with London moved to the
#' front if it is there. More levels than the palette holds is an error.
#'
#' [scale_colour_gla()] with `type = "auto"` applies this to a plot.
#'
#' @param levels The series names, in the order the scale shows them.
#' @param theme `"light"` or `"dark"`.
#' @return A character vector of hex colours named by level.
#' @examples
#' gla_series_colours(c("England", "London"))
#' gla_series_colours(c("Inner London", "Outer London"))
#' gla_series_colours(c("Variable A", "Variable B", "Variable C"))
#' @export
gla_series_colours <- function(levels, theme = c("light", "dark")) {
  theme <- match.arg(theme)
  lv <- as.character(levels)
  lv <- lv[!is.na(lv)]
  t <- function(x) unname(gla_token(x, theme))
  has <- function(pattern) grepl(pattern, lv, ignore.case = TRUE)
  pair <- function(first, second, colour_first, colour_second) {
    a <- has(first)
    b <- has(second)
    if (sum(a) == 1 && sum(b) == 1 && !any(a & b)) {
      return(stats::setNames(ifelse(a, colour_first, colour_second), lv))
    }
    NULL
  }

  if (length(lv) == 2) {
    london <- has("London")
    if (sum(london) == 1) {
      return(stats::setNames(ifelse(london, t("data-primary"), t("data-context")), lv))
    }
    out <- pair("Domestic", "International", t("data-primary"), t("data-secondary")) %||%
      pair("Inner", "Outer", t("data-primary"), t("data-secondary"))
    if (!is.null(out)) return(out)
  }

  pal <- gla_pal("categorical", theme = theme)
  if (length(lv) > length(pal)) {
    stop(sprintf("The categorical palette has %d colours; the data has %d levels. %s",
                 length(pal), length(lv), "Fold the categories"), call. = FALSE)
  }
  order <- c(which(lv == "London"), which(lv != "London"))
  stats::setNames(unname(pal[seq_along(lv)])[order(order)], lv)
}
