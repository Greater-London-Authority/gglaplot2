#' GLA colour palettes
#'
#' Palettes built from the `data-*` and `palette-*` tokens, so a chart made
#' here matches a dashboard built on the same tokens.
#'
#' `"categorical"` is the order the visualisation team's charts colour
#' series in: `data-primary`, `data-secondary`, `data-tertiary`, then
#' turquoise, purple, red, green and yellow from the `data-categorical-*`
#' tokens. It is their `getColorScale` order, so a chart here and a chart on
#' one of their sites colour the fourth series alike. It validates clean to
#' three series under the palette checks (lightness band, chroma, colour
#' vision deficiency separation, contrast on the chart surface); past three,
#' turquoise reads grey on a light surface, purple leaves the lightness
#' band and red against green is a known pair. Design to three and fold
#' categories rather than reach for a fourth colour; where a chart must
#' carry more, label the series directly.
#'
#' `"sequential"` is the blue ramp, light to dark, interpolated to `n`.
#' `"diverging"` is red for down and blue for up, the hues the `change`
#' palette uses, as steps: `n` colours with a neutral grey in the middle
#' when `n` is odd and none when it is even, or `n = c(down, up)` for a
#' different number of steps each side and no neutral. Each side is cut
#' from a ramp of the longer side's length, from the light end, so a step
#' the same distance from the centre is the same depth of colour on either
#' side and the shorter side stops at a lighter colour rather than reaching
#' the darkest. The steps run from the 400 to the 800 level of the token
#' ramps, the lightest that holds 2:1 against the surface and reads apart
#' from the neutral. `"sex"` is purple for women
#' and green for men, named by both spellings a source might use.
#' `"change"` is `up` (blue) and `down` (red): a value rising is coloured by
#' the token, not by arithmetic sign.
#'
#' @param type One of `"categorical"`, `"sequential"`, `"diverging"`,
#'   `"sex"`, `"change"`.
#' @param n Number of colours. Defaults to the full palette for the discrete
#'   types and five for the ramps. For `"diverging"`, `c(down, up)` gives
#'   each side its own count. A categorical request past the palette is an
#'   error, not a recycled colour.
#' @param theme `"light"` or `"dark"`. Defaults to the mode set by [gla_mode()].
#' @param ... gglaplot's arguments, for a script written against it:
#'   `gla_theme`, `palette_type` (`"categorical"`, `"quantitative"`,
#'   `"highlight"` or `"diverging"`), `palette_name`, `main_colours`
#'   (gglaplot's colour names, translated to the tokens), `inc0` and
#'   `remove_margin`. With any of them given the palette is built the way
#'   gglaplot built it, from the current tokens, and `n` means what it
#'   meant there.
#' @return A character vector of hex colours, named for the discrete types.
#' @examples
#' gla_pal()
#' gla_pal("sequential", 7)
#' gla_pal("diverging", 7)
#' gla_pal("diverging", c(4, 1))
#' gla_pal("sex")
#' @export
gla_pal <- function(type = c("categorical", "sequential", "diverging", "sex", "change"),
                    n = NULL, theme = gla_mode(), ...) {
  dots <- list(...)
  if (length(type) == 1 && tolower(type) %in% c("default", "inverse", "light", "dark")) {
    # gglaplot's first argument was the theme name
    dots$gla_theme <- type
    type <- "categorical"
  }
  if (length(dots)) {
    return(compat_gla_pal(n, if (is.character(theme)) theme[1] else "light", dots))
  }
  type <- match.arg(type)
  theme <- match_mode(theme)
  if (type == "diverging") return(diverging_steps(n %||% 5, theme))
  t <- function(x) unname(gla_token(x, theme))
  blues <- function(levels) t(paste0("palette-blue-", levels))

  pal <- switch(type,
    categorical = stats::setNames(
      t(c("data-primary", "data-secondary", "data-tertiary", "data-categorical-turquoise",
          "data-categorical-purple", "data-categorical-red", "data-categorical-green",
          "data-categorical-yellow")),
      c("primary", "secondary", "tertiary", "turquoise", "purple", "red", "green", "yellow")
    ),
    sequential = blues(c(100, 300, 500, 700, 900)),
    sex = stats::setNames(t(c("data-female", "data-male", "data-female", "data-male")),
                          c("Women", "Men", "Female", "Male")),
    change = stats::setNames(t(c("data-positive", "data-negative")), c("up", "down"))
  )

  if (is.null(n)) return(pal)
  if (type == "sequential") {
    return(grDevices::colorRampPalette(pal)(n))
  }
  if (n > length(pal)) {
    stop(sprintf("The %s palette has %d colours; %d were asked for. Fold the categories",
                 type, length(pal), n), call. = FALSE)
  }
  pal[seq_len(n)]
}

# k steps of one hue for a stepped ramp, cut from the token ramp between two
# levels (nearest the neutral first), through the levels between, so five
# steps from 400 to 800 are the tokens themselves and another count sits on
# the ramp the design system drew. The tokens are a hundred apart and
# already even in lightness, so a straight blend between neighbours holds
# the ramp. The dark tokens run the other way, so the same levels give the
# step nearest the neutral in either mode.
ramp_steps <- function(hue, k, levels = c(400, 800), theme = gla_mode()) {
  if (k < 1) return(character())
  stops <- unname(gla_token(paste0("palette-", hue, "-", seq(levels[1], levels[2], by = 100)),
                            theme))
  if (k == length(stops)) return(stops)
  tolower(grDevices::colorRampPalette(stops)(k))
}

# the diverging palette as steps, red for down and blue for up. n is one
# count (odd for a neutral centre, even for none) or c(down, up) with no
# neutral. Both sides are cut from one ramp of the longer side's length,
# from the light end, so a step the same distance from the centre is the
# same depth whichever side it is on.
diverging_steps <- function(n, theme = gla_mode()) {
  if (!is.numeric(n) || !length(n) %in% 1:2 || any(n < 0) || any(n != round(n))) {
    stop("For the diverging palette n is one count or c(down, up)", call. = FALSE)
  }
  if (length(n) == 1) {
    neutral <- n %% 2 == 1
    sides <- rep((n - neutral) / 2, 2)
  } else {
    neutral <- FALSE
    sides <- n
  }
  m <- max(sides)
  down <- rev(ramp_steps("red", m, theme = theme)[seq_len(sides[1])])
  up <- ramp_steps("blue", m, theme = theme)[seq_len(sides[2])]
  c(down, if (neutral) unname(gla_token("data-neutral-0", theme)), up)
}

#' The number of categorical series colour can carry on one chart
#'
#' Three is the design ceiling for a clean chart. Past it the palette still
#' has colours, eight in all, but they no longer all pass the palette
#' checks, so a chart that goes over should label its series directly. See
#' [gla_pal()].
#'
#' @return An integer.
#' @examples
#' gla_max_series()
#' @export
gla_max_series <- function() 3L

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
#' @param theme `"light"` or `"dark"`. Defaults to the mode set by [gla_mode()].
#' @return A character vector of hex colours named by level.
#' @examples
#' gla_series_colours(c("England", "London"))
#' gla_series_colours(c("Inner London", "Outer London"))
#' gla_series_colours(c("Variable A", "Variable B", "Variable C"))
#' @export
gla_series_colours <- function(levels, theme = gla_mode()) {
  theme <- match_mode(theme)
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
