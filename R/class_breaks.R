#' Breaks that follow the distribution of the data
#'
#' The stepped scales cut equal steps by default, which read the same from
#' one publication to the next. A one-off map of a skewed distribution, or
#' one with an area far from the rest, is often better classed by the data:
#' this is what GIS tools call a classification method, and
#' `scale_fill_gla_b(method = )` draws it. `gla_class_breaks()` is the
#' calculation on its own, for a table or a map drawn elsewhere.
#'
#' Every method decides which areas share a bin, and nothing else. The
#' lines between the bins are then drawn by one rule, so the legend reads
#' cleanly and no area moves: each break is put in the gap between two
#' neighbouring classes, rounded to the coarsest precision at which every
#' break still sits inside its gap (whole numbers if they do, else one
#' decimal place, else two). The limits widen to that precision, and the
#' legend prints at it. A class of one value, an outlier in a bin of its
#' own, keeps its bin that way, where a break put on the value itself
#' would give it no width.
#'
#' With a `midpoint`, for a diverging scale, each side is classed on its
#' own with its own count, and the midpoint is always a break. The methods:
#'
#' - `"jenks"`: natural breaks, the classes that keep the variation within
#'   each bin smallest (Fisher's exact algorithm). Usually gives an outlier
#'   a bin of its own. The general choice.
#' - `"quantile"`: the same number of areas in each bin. Even colour across
#'   the map, at the cost of hiding an outlier among its neighbours and
#'   sometimes splitting near-identical values.
#' - `"boxplot"`: the quartiles, with a bin beyond each fence at 1.5 times
#'   the interquartile range. An outlier is set apart by a fixed rule,
#'   so the bin only appears when there is one. The count is set by the
#'   data, not `n`, and fewer than five values are one bin.
#' - `"sd"`: bins one standard deviation wide, centred on the mean, and
#'   only the ones that hold an area. For a measure that is roughly
#'   normal. The count is set by the data, and fewer than five values are
#'   one bin.
#' - `"headtails"`: for a heavy tail, a few large values and many small:
#'   the values split at their mean, then the upper part at its mean,
#'   until the upper part is no longer the minority or `n` bins are made.
#'   On the side below a midpoint it runs on the size of the fall.
#' - `"geometric"`: bin widths that grow by a constant ratio from the
#'   smallest value to the largest, for counts and densities that spread
#'   over orders of magnitude. Needs values above zero; on a diverging
#'   scale it runs on the distance from the midpoint.
#'
#' Classed breaks move with the data. A map published every month on
#' them is not comparable from one month to the next, because the same
#' value can fall in a different bin, so for a series set `steps` with
#' `limits`, or `breaks` by hand, and keep classing for the one-off read.
#'
#' @param x The values.
#' @param method One of the methods above.
#' @param n The number of bins to make: one number, or with a `midpoint`
#'   one for each side as `c(below, above)`. A side with fewer distinct
#'   values than bins gets one bin per value. Ignored by `"boxplot"` and
#'   `"sd"`.
#' @param midpoint For a diverging scale, the value the classes are counted
#'   from, or `NULL` for a sequential one.
#' @param limits The ends of the scale, if the caller has set them: values
#'   beyond take the end bins, and the limits are not widened.
#' @return The breaks, in order, with attributes: `limits`, the ends of
#'   the scale; `digits`, the precision the breaks were rounded to (two is
#'   hundredths, minus one is tens); `counts`, how many values fall in each
#'   bin.
#' @examples
#' falls <- c(-5.3, -2, -1.9, -1.7, -1.5, -1.3, -1.1, -0.9, -0.8, -0.5, 0.2, 0.4)
#' gla_class_breaks(falls, "jenks", n = c(4, 1), midpoint = 0)
#' gla_class_breaks(c(1, 2, 2, 3, 8, 9, 30, 55, 120), "geometric", n = 4)
#' @export
gla_class_breaks <- function(x, method = c("jenks", "quantile", "boxplot", "sd", "headtails",
                                           "geometric"),
                             n = 5, midpoint = NULL, limits = NULL) {
  method <- match.arg(method)
  if (!is.numeric(n) || !length(n) %in% 1:2 || any(n < 1 | n != round(n))) {
    stop("`n` must be a whole number of bins, or c(below, above) with a midpoint", call. = FALSE)
  }
  x <- x[is.finite(x)]
  if (!is.null(limits)) {
    limits <- sort(limits)
    x <- pmin(pmax(x, limits[1]), limits[2])
  }
  if (!length(x)) return(structure(numeric(), limits = limits, digits = 0L, counts = integer()))
  v <- sort(x)
  if (is.null(midpoint)) {
    if (identical(method, "geometric") && v[1] <= 0) {
      stop("`geometric` breaks need values above zero; give a `midpoint` for a change",
           call. = FALSE)
    }
    gaps <- class_gaps(v, method, n[1])
    fit <- place_breaks(gaps)
    breaks <- fit$breaks
    span <- range(v)
  } else {
    n <- rep_len(n, 2)
    below <- v[v < midpoint]
    above <- v[v > midpoint]
    # the lower side is classed on its distance from the midpoint, so a
    # method built for a long upper tail reads the large falls as the tail
    gaps_below <- class_gaps(midpoint - rev(below), method, n[1])
    gaps_below <- lapply(rev(gaps_below), function(g) rev(midpoint - g))
    gaps_above <- class_gaps(above - midpoint, method, n[2])
    gaps_above <- lapply(gaps_above, function(g) midpoint + g)
    fit <- place_breaks(c(gaps_below, gaps_above))
    breaks <- sort(c(fit$breaks, midpoint))
    span <- range(c(v, midpoint))
  }
  digits <- fit$digits
  if (is.null(limits)) {
    limits <- c(floor(span[1] * 10^digits - 1e-9) / 10^digits,
                ceiling(span[2] * 10^digits + 1e-9) / 10^digits)
    if (!is.null(midpoint)) {
      limits <- c(min(limits[1], midpoint), max(limits[2], midpoint))
    }
  }
  breaks <- breaks[breaks > limits[1] & breaks < limits[2]]
  counts <- as.integer(table(cut(x, c(limits[1], breaks, limits[2]), include.lowest = TRUE)))
  structure(breaks, limits = limits, digits = as.integer(digits), counts = counts)
}

# the gaps between neighbouring classes of sorted values: a list of
# c(top of the lower class, bottom of the upper class)
class_gaps <- function(v, method, n) {
  if (length(v) < 2) return(list())
  # quartiles and a standard deviation mean little across a handful of values
  if (method %in% c("boxplot", "sd") && length(v) < 5) return(list())
  ends <- switch(method,
    jenks = jenks_ends(v, n),
    quantile = quantile_ends(v, n),
    boxplot = threshold_ends(v, boxplot_thresholds(v)),
    sd = threshold_ends(v, sd_thresholds(v)),
    headtails = threshold_ends(v, headtails_thresholds(v, n)),
    geometric = threshold_ends(v, v[1] * (v[length(v)] / v[1])^(seq_len(n - 1) / n))
  )
  # a class never splits equal values, and the gap has to have width
  ends <- unique(ends[ends >= 1 & ends < length(v)])
  ends <- ends[v[ends] < v[ends + 1]]
  lapply(ends, function(i) c(v[i], v[i + 1]))
}

# the last index of each class but the last, where the classes are set by
# thresholds: everything at or below a threshold is in the class below it
threshold_ends <- function(v, thresholds) {
  thresholds <- thresholds[is.finite(thresholds)]
  if (!length(thresholds)) return(integer())
  sort(unique(findInterval(thresholds, v)))
}

# classes with the same count of values each, by rank
quantile_ends <- function(v, n) {
  m <- length(v)
  n <- min(n, length(unique(v)))
  if (n < 2) return(integer())
  ends <- round(m * seq_len(n - 1) / n)
  # a class ending inside a run of equal values ends after the run
  vapply(ends, function(i) max(which(v == v[i])), integer(1))
}

# the quartiles, with the fences at 1.5 times the interquartile range
boxplot_thresholds <- function(v) {
  q <- stats::quantile(v, c(0.25, 0.5, 0.75), names = FALSE)
  iqr <- q[3] - q[1]
  t <- c(q[1] - 1.5 * iqr, q, q[3] + 1.5 * iqr)
  t[t > v[1] & t < v[length(v)]]
}

# the edges of bins one standard deviation wide, centred on the mean
sd_thresholds <- function(v) {
  s <- stats::sd(v)
  if (!is.finite(s) || s == 0) return(numeric())
  m <- mean(v)
  k <- ceiling(max(abs(v - m)) / s) + 1
  t <- m + s * (seq(-k, k) + 0.5)
  t[t > v[1] & t < v[length(v)]]
}

# head/tail breaks: split at the mean while the part above it is the
# minority, to at most n bins
headtails_thresholds <- function(v, n) {
  out <- numeric()
  part <- v
  while (length(out) < n - 1 && length(part) > 1) {
    m <- mean(part)
    head <- part[part > m]
    if (!length(head) || length(head) == length(part)) break
    out <- c(out, m)
    if (length(head) / length(part) > 0.4) break
    part <- head
  }
  out
}

# Fisher's exact natural breaks: the partition of sorted values into n
# classes with the least variation within them. Dynamic programming over
# the running sums, one vector operation per class and value.
jenks_ends <- function(v, n) {
  m <- length(v)
  n <- min(n, length(unique(v)))
  if (n < 2) return(integer())
  s1 <- c(0, cumsum(v))
  s2 <- c(0, cumsum(v^2))
  sse <- function(from, to) {
    count <- to - from + 1
    pmax((s2[to + 1] - s2[from]) - (s1[to + 1] - s1[from])^2 / count, 0)
  }
  cost <- matrix(Inf, n, m)
  start <- matrix(0L, n, m)
  cost[1, ] <- sse(1, seq_len(m))
  start[1, ] <- 1L
  for (j in 2:n) {
    for (i in j:m) {
      from <- j:i
      total <- cost[j - 1, from - 1] + sse(from, i)
      best <- which.min(total)
      cost[j, i] <- total[best]
      start[j, i] <- from[best]
    }
  }
  ends <- integer()
  i <- m
  for (j in n:2) {
    i <- start[j, i] - 1L
    ends <- c(i, ends)
  }
  ends
}

# a break in each gap, at the coarsest precision that keeps every break
# strictly inside its own gap and the breaks apart; the midpoint of the
# gap otherwise
place_breaks <- function(gaps) {
  if (!length(gaps)) return(list(breaks = numeric(), digits = 0L))
  lo <- vapply(gaps, `[`, numeric(1), 1)
  hi <- vapply(gaps, `[`, numeric(1), 2)
  mid <- (lo + hi) / 2
  for (digits in -9:9) {
    r <- round(mid, digits)
    if (all(r > lo & r < hi) && !anyDuplicated(r)) return(list(breaks = r, digits = digits))
  }
  list(breaks = mid, digits = 10L)
}
