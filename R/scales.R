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
#' Continuous scales, `scale_colour_gla_c()` and `scale_fill_gla_c()`, use
#' the sequential or diverging ramp. The diverging one puts its neutral grey
#' at `midpoint`, not at the middle of the data.
#'
#' Binned scales, `scale_colour_gla_b()` and `scale_fill_gla_b()`, are the
#' stepped ramps the web maps use and the choice for a choropleth, where a
#' reader matches an area to a step in the legend. The sequential one cuts
#' the blue ramp into as many steps as there are bins. The diverging one
#' counts its bins out from `midpoint` in equal steps, so zero is always a
#' break, and colours each side from a ramp of the longer side's length:
#' one step below the midpoint is as deep a red as one step above is a
#' blue, and a side with fewer bins stops at a lighter colour rather than
#' stretching to the darkest. A bin that holds the midpoint takes the
#' neutral. Breaks, labels and limits are ggplot2's; where one area is far
#' from the rest, set `limits` and the values beyond take the end step,
#' which a note on the chart should say. `steps` fixes how many bins each
#' side has: `steps = 3` is three below and three above, `steps = c(2, 4)`
#' two below and four above. Each side then takes its own step width,
#' rounded to 1, 2, 2.5 or 5 times a power of ten, so a small rise beside a
#' large fall gets narrow blue steps rather than being stretched; with
#' `limits` set, the steps divide them exactly. The count holds as the data
#' changes, where `breaks` would need resetting.
#'
#' Both ramps draw their guide vertical on the right, whatever legend
#' position the theme sets, because a ramp reads top to bottom and the
#' theme's top position is for series keys. `theme_gla(legend = "none")`
#' still hides it, and a `guide` passed to the scale wins.
#'
#' Missing values take `data-empty`. `scale_color_*` spellings are
#' provided as aliases.
#'
#' @param type A palette as above. Discrete scales take `"auto"`,
#'   `"categorical"`, `"sex"` or `"change"`; continuous and binned ones
#'   `"sequential"` or `"diverging"`.
#' @param theme `"light"` or `"dark"`. Defaults to the mode set by [gla_mode()].
#' @param midpoint For the diverging scales, the value the neutral sits at
#'   and the bins are counted from. Default 0.
#' @param steps For the diverging binned scales, how many bins each side of
#'   the midpoint has: one number for both sides or `c(below, above)`.
#'   `NULL`, the default, counts ggplot2's step out from the midpoint.
#' @param ... Passed to the underlying ggplot2 scale.
#' @return A ggplot2 scale.
#' @examples
#' library(ggplot2)
#' ggplot(gla_yearly, aes(year, average, colour = series)) +
#'   geom_gla_line() +
#'   scale_colour_gla() +
#'   theme_gla()
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   b <- gla_boroughs
#'   b$change <- seq(-12, 4, length.out = nrow(b))
#'   ggplot(b) +
#'     geom_gla_sf(aes(fill = change)) +
#'     scale_fill_gla_b("diverging", name = NULL) +
#'     theme_gla_map()
#' }
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

# a ramp reads top to bottom, so its guide stands on the right whatever the
# theme's legend position; the theme's "none" still hides it
ramp_guide <- function(constructor) {
  constructor(position = "right", direction = "vertical")
}

# a smooth ramp; the diverging one rescaled so the midpoint, not the middle
# of the data, is the neutral
.gla_continuous <- function(aesthetic, type, theme, midpoint, ...) {
  args <- list(...)
  args$guide <- args$guide %||% ramp_guide(ggplot2::guide_colourbar)
  if (type == "diverging") {
    args$rescaler <- args$rescaler %||% function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
      scales::rescale_mid(x, to, from, mid = midpoint)
    }
  }
  rlang::exec(ggplot2::continuous_scale, aesthetic,
              palette = scales::pal_gradient_n(gla_pal(type, theme = theme)),
              na.value = unname(gla_token("data-empty", theme)), !!!args)
}

#' @rdname scale_gla
#' @export
scale_colour_gla_c <- function(type = c("sequential", "diverging"),
                               theme = gla_mode(), midpoint = 0, ...) {
  .gla_continuous("colour", match.arg(type), match_mode(theme), midpoint, ...)
}

#' @rdname scale_gla
#' @export
scale_fill_gla_c <- function(type = c("sequential", "diverging"),
                             theme = gla_mode(), midpoint = 0, ...) {
  .gla_continuous("fill", match.arg(type), match_mode(theme), midpoint, ...)
}

# a binned scale whose bins count out from the midpoint and whose colours
# come from where each bin sits against it
ScaleBinnedGlaDiverging <- ggplot2::ggproto(
  "ScaleBinnedGlaDiverging", ggplot2::ScaleBinned,
  midpoint = 0,
  theme_mode = "light",
  steps = NULL,
  # ggplot2's default breaks, then the same step counted out from the
  # midpoint so it is always a break; without limits from the caller the
  # limits widen to whole bins, as ggplot2's do. With steps, each side is
  # cut into that many bins of its own rounded width, or of the width that
  # divides limits the caller set.
  get_breaks = function(self, limits = self$get_limits()) {
    if (!inherits(self$breaks, "waiver") || !self$nice.breaks) {
      return(ggplot2::ggproto_parent(ggplot2::ScaleBinned, self)$get_breaks(limits))
    }
    if (self$is_empty()) return(numeric())
    transformation <- self$get_transformation()
    lim <- sort(transformation$inverse(limits))
    if (scales::zero_range(lim)) lim <- lim + c(-0.05, 0.05)
    mid <- self$midpoint
    if (!is.null(self$steps)) {
      n <- rep_len(self$steps, 2)
      fixed <- !is.null(self$limits)
      width <- function(extent, k) if (fixed) extent / k else nice_step(extent / k)
      down <- width(max(mid - lim[1], 0), n[1])
      up <- width(max(lim[2] - mid, 0), n[2])
      if (!fixed) {
        lim <- c(mid - n[1] * down, mid + n[2] * up)
        self$limits <- transformation$transform(lim)
      }
      breaks <- c(mid - down * seq_len(n[1] - 1), mid, mid + up * seq_len(n[2] - 1))
      breaks <- sort(breaks[breaks > lim[1] & breaks < lim[2]])
      self$breaks <- breaks
      return(transformation$transform(breaks))
    }
    step <- break_step(lim, self$n.breaks %||% 5)
    steps_out <- function(x, f) f(round((x - mid) / step, 8))
    if (is.null(self$limits)) {
      lim <- mid + step * c(steps_out(lim[1], floor), steps_out(lim[2], ceiling))
      self$limits <- transformation$transform(lim)
    }
    breaks <- mid + step * seq(steps_out(lim[1], ceiling), steps_out(lim[2], floor))
    breaks <- breaks[breaks > lim[1] & breaks < lim[2]]
    self$breaks <- breaks
    transformation$transform(breaks)
  },
  map = function(self, x, limits = self$get_limits()) {
    if (self$after.stat) return(x)
    breaks <- self$get_breaks(limits)
    if (missing(limits)) limits <- self$get_limits()
    edges <- sort(unique(c(limits[1], breaks, limits[2])))
    x <- self$rescale(self$oob(x, range = limits), limits)
    cuts <- self$rescale(edges, limits)
    x_binned <- if (length(cuts) > 1) {
      cut(x, cuts, labels = FALSE, include.lowest = TRUE, right = self$right)
    } else {
      rep(1L, length(x))
    }
    pal <- self$palette.cache
    if (is.null(pal)) {
      edges <- self$get_transformation()$inverse(edges)
      pal <- diverging_bin_colours(edges[-length(edges)], edges[-1], self$midpoint,
                                   self$theme_mode)
      self$palette.cache <- pal
    }
    scaled <- pal[x_binned]
    ifelse(!is.na(scaled), scaled, self$na.value)
  }
)

# a step of 1, 2, 2.5, 3, 4 or 5 times a power of ten, the first at or
# above x, so a side of fixed bins wastes as little of its end bin as it can
nice_step <- function(x) {
  if (x <= 0) return(0)
  p <- 10^floor(log10(x))
  steps <- c(1, 2, 2.5, 3, 4, 5, 10)
  p * steps[which(steps >= x / p - 1e-9)[1]]
}

# the step ggplot2's extended breaks would use over these limits
break_step <- function(limits, n) {
  ext <- scales::breaks_extended(n)(limits)
  if (length(ext) >= 2) min(diff(ext)) else diff(limits)
}

# colours for bins by where they sit against the midpoint: those below it
# red, those above blue, a bin that holds it the neutral
diverging_bin_colours <- function(lo, hi, midpoint, theme) {
  tol <- 1e-8 * max(abs(c(lo, hi)), 1)
  down <- hi <= midpoint + tol
  up <- lo >= midpoint - tol
  steps <- diverging_steps(c(sum(down), sum(up)), theme)
  out <- rep(unname(gla_token("data-neutral-0", theme)), length(lo))
  out[down] <- steps[seq_len(sum(down))]
  out[up] <- steps[sum(down) + seq_len(sum(up))]
  out
}

.gla_binned <- function(aesthetic, type, theme, midpoint, steps, ...) {
  args <- list(...)
  args$guide <- args$guide %||% ramp_guide(ggplot2::guide_coloursteps)
  empty <- unname(gla_token("data-empty", theme))
  if (!is.null(steps)) {
    if (type != "diverging") {
      stop("`steps` is for the diverging scale; the sequential one takes ggplot2's `n.breaks`",
           call. = FALSE)
    }
    whole <- is.numeric(steps) && length(steps) %in% 1:2 && all(steps >= 1 & steps == round(steps))
    if (!whole) {
      stop("`steps` must be a whole number of bins, one for both sides or c(below, above)",
           call. = FALSE)
    }
  }
  if (type == "sequential") {
    return(rlang::exec(ggplot2::binned_scale, aesthetic,
                       palette = function(x) ramp_steps("blue", length(x), c(200, 900), theme),
                       na.value = empty, !!!args))
  }
  sc <- rlang::exec(ggplot2::binned_scale, aesthetic,
                    palette = function(x) rep(NA_character_, length(x)), na.value = empty,
                    super = ScaleBinnedGlaDiverging, !!!args)
  sc$midpoint <- midpoint
  sc$theme_mode <- theme
  sc$steps <- steps
  sc
}

#' @rdname scale_gla
#' @export
scale_colour_gla_b <- function(type = c("sequential", "diverging"),
                               theme = gla_mode(), midpoint = 0, steps = NULL, ...) {
  .gla_binned("colour", match.arg(type), match_mode(theme), midpoint, steps, ...)
}

#' @rdname scale_gla
#' @export
scale_fill_gla_b <- function(type = c("sequential", "diverging"),
                             theme = gla_mode(), midpoint = 0, steps = NULL, ...) {
  .gla_binned("fill", match.arg(type), match_mode(theme), midpoint, steps, ...)
}

#' @rdname scale_gla
#' @export
scale_color_gla <- scale_colour_gla

#' @rdname scale_gla
#' @export
scale_color_gla_c <- scale_colour_gla_c

#' @rdname scale_gla
#' @export
scale_color_gla_b <- scale_colour_gla_b

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
