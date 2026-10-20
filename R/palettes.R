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
