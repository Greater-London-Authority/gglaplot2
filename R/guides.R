#' A legend with one swatch per bin
#'
#' The legend the stepped scales draw by default: a swatch for every bin,
#' labelled with the bin's range, `-6% to -4%`, highest bin at the top.
#' The ends are always labelled, since the first and last bins run to the
#' limits; where the scale's `limits` leave values outside them, the end bin
#' those values take reads `Below -4%` or `Above 4%`. The labels come from
#' the scale's `labels` function, so `labels = label_gla_percent(scale = 1)`
#' on the scale puts the sign on both ends of each range. A house label
#' function ([label_gla_percent()] and the rest) set to whole numbers gains
#' the decimals narrow steps need, so steps of 0.2 read `0% to 0.2%`, and a
#' short label the decimals its own unit needs, `1m to 1.025m`. With no
#' `labels` the ranges print the decimals their ends have, `0 to 12.5`. The
#' web maps' legends read this way; ggplot2's `guide_coloursteps()` draws a
#' bar labelled at the cuts instead, and remains a `guide` away.
#'
#' @param title The legend title. The chart's subtitle usually says what
#'   the values are, so the stepped scales pass `name = NULL`.
#' @param theme A [ggplot2::theme()] for the legend alone.
#' @param position,direction Where it sits and which way it runs. The
#'   stepped scales put it on the right, vertical, whatever the chart
#'   theme's legend position.
#' @param sep What joins the two ends of a range.
#' @param reverse Lowest bin at the top instead.
#' @param order The order of this guide among several.
#' @param ... Passed to [ggplot2::guide_legend()].
#' @return A guide for a binned colour or fill scale.
#' @examples
#' library(ggplot2)
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   b <- gla_boroughs
#'   b$change <- seq(-5, 2, length.out = nrow(b))
#'   ggplot(b) +
#'     geom_gla_sf(aes(fill = change)) +
#'     scale_fill_gla_b("diverging", name = NULL, steps = 3,
#'                      labels = label_gla_percent(scale = 1)) +
#'     theme_gla_map()
#'   # the same with the ends joined by a dash
#'   ggplot(b) +
#'     geom_gla_sf(aes(fill = change)) +
#'     scale_fill_gla_b("diverging", name = NULL, steps = 3,
#'                      guide = guide_gla_bins(sep = " – ")) +
#'     theme_gla_map()
#' }
#' @export
guide_gla_bins <- function(title = ggplot2::waiver(), theme = NULL, position = "right",
                           direction = "vertical", sep = " to ", reverse = FALSE,
                           order = 0, ...) {
  ggplot2::new_guide(
    title = title, theme = theme, position = position, direction = direction,
    sep = sep, reverse = reverse, order = order,
    available_aes = c("colour", "fill"), name = "gla_bins", super = GuideGlaBins, ...
  )
}

#' @rdname guide_gla_bins
#' @format NULL
#' @usage NULL
#' @export
GuideGlaBins <- ggplot2::ggproto(
  "GuideGlaBins", ggplot2::GuideLegend,
  params = c(ggplot2::GuideLegend$params, list(sep = " to ")),
  # one key per bin, between the limits and the cuts, coloured by its
  # middle and labelled by its two ends; highest bin first so the legend
  # reads top to bottom as the stepped bar would
  extract_key = function(scale, aesthetic, sep = " to ", ...) {
    # breaks = NULL asks for no legend, as it does of ggplot2's
    if (is.null(scale$get_breaks())) return(NULL)
    bins <- bin_ranges(scale, sep)
    if (is.null(bins)) return(NULL)
    key <- data.frame(scale$map(bins$mid), stringsAsFactors = FALSE)
    names(key) <- aesthetic
    key$.value <- bins$mid
    key$.label <- bins$label
    key[rev(seq_len(nrow(key))), , drop = FALSE]
  }
)

# the bins of a stepped scale, lowest first: the middle of each and the
# label of its range, read from its low end. An end bin that holds values
# beyond the limits (the scale squishes them in) says so, "Below -4%",
# rather than giving a range the values in it are outside. A scale with
# nothing in it has no bins.
bin_ranges <- function(scale, sep = " to ") {
  limits <- scale$get_limits()
  if (!all(is.finite(limits))) return(NULL)
  # a break beyond the limits bounds no bin a value is drawn in
  breaks <- scale$get_breaks()
  breaks <- breaks[is.finite(breaks) & breaks > min(limits) & breaks < max(limits)]
  edges <- sort(unique(c(limits[1], breaks, limits[2])))
  n <- length(edges) - 1
  if (n < 1) return(NULL)
  labels <- bin_edge_labels(scale, edges)
  mid <- (edges[-1] + edges[-length(edges)]) / 2
  if (is.null(labels)) return(list(mid = mid, label = rep("", n)))
  # on a reversed scale the first bin holds the highest values
  ends <- scale$get_transformation()$inverse(edges[c(1, n + 1)])
  reversed <- isTRUE(ends[1] > ends[2])
  low <- labels[-length(labels)]
  high <- labels[-1]
  ranges <- if (reversed) paste0(high, sep, low) else paste0(low, sep, high)
  data <- scale$range$range
  # only where the scale squishes such values into the end bin; one that
  # censors them draws them as missing
  beyond <- edges[c(1, n + 1)] + c(-1, 1)
  squished <- !anyNA(tryCatch(scale$oob(beyond, range = edges[c(1, n + 1)]),
                              error = function(e) NA))
  if (n > 1 && length(data) == 2 && all(is.finite(c(data, edges))) && squished) {
    tol <- 1e-8 * max(abs(edges), 1)
    words <- if (reversed) c("Above", "Below") else c("Below", "Above")
    if (data[1] < edges[1] - tol) ranges[1] <- paste(words[1], labels[2])
    if (data[2] > edges[n + 1] + tol) ranges[n] <- paste(words[2], labels[n])
  }
  list(mid = mid, label = ranges)
}

# the labels at the edges of the bins. A house label function set to a
# fixed accuracy is asked again at a finer one when the edges do not sit on
# it, so steps of 0.2 read "0% to 0.2%", not "0% to 0%". A scale given no
# labels takes the house number the same way, with its thousands marked.
# Labels given as text name the breaks; the limits, which have none, take
# the house number.
bin_edge_labels <- function(scale, edges) {
  labels <- scale_labels(scale)
  if (is.null(labels)) return(NULL)
  transformation <- scale$get_transformation()
  values <- transformation$inverse(edges)
  plain <- inherits(labels, "waiver") && inherits(transformation, "transform") &&
    transformation$name == "identity"
  if (plain) labels <- label_gla_number(accuracy = 1)
  refine <- attr(labels, "gla_refine")
  if (!is.null(refine) && all(is.finite(values)) && length(unique(values)) > 1) {
    shown <- values * refine$scale
    if (isTRUE(refine$short)) shown <- short_units(shown)
    accuracy <- bin_accuracy(shown, refine$accuracy)
    if (accuracy < refine$accuracy) return(refine$at(accuracy)(values))
  }
  if (plain) return(labels(values))
  if (!is.function(labels) && !inherits(labels, "waiver")) {
    breaks <- scale$get_breaks()
    named <- as.character(scale$get_labels(breaks))
    at <- match(edges, breaks)
    out <- label_gla_number()(values)
    out[!is.na(at)] <- named[at[!is.na(at)]]
    return(out)
  }
  scale$get_labels(edges)
}

# a scale's labels as they were given: `$` hands a function back wrapped as
# a ggproto method, without the attributes a house label function carries
scale_labels <- function(scale) {
  labels <- scale$labels
  if (inherits(labels, "ggproto_method")) environment(labels)$f else labels
}

# the accuracy a set of bin edges needs: the one asked for when every edge
# sits on it, else a tenth or a hundredth of it, else one figure beyond the
# narrowest step (limits cut in three give edges no decimal lands on)
bin_accuracy <- function(values, accuracy) {
  on_grid <- function(a) all(abs(values / a - round(values / a)) < 1e-6)
  for (a in accuracy / c(1, 10, 100)) if (on_grid(a)) return(a)
  step <- min(diff(sort(unique(values))))
  min(accuracy, 10^(floor(log10(step)) - 1))
}
