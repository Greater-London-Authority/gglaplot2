#' A legend with one swatch per bin
#'
#' The legend the stepped scales draw by default: a swatch for every bin,
#' labelled with the bin's range, `-6% to -4%`, highest bin at the top.
#' The ends are always labelled, since the first and last bins run to the
#' limits. The labels come from the scale's `labels` function, so
#' `labels = label_gla_percent(scale = 1)` on the scale puts the sign on
#' both ends of each range. The web maps' legends read this way; ggplot2's
#' `guide_coloursteps()` draws a bar labelled at the cuts instead, and
#' remains a `guide` away.
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
    breaks <- scale$get_breaks()
    if (length(breaks) == 0) return(NULL)
    limits <- scale$get_limits()
    edges <- sort(unique(c(limits[1], breaks[is.finite(breaks)], limits[2])))
    if (length(edges) < 2) return(NULL)
    lo <- edges[-length(edges)]
    hi <- edges[-1]
    labels <- scale$get_labels(edges)
    key <- data.frame(scale$map((lo + hi) / 2), stringsAsFactors = FALSE)
    names(key) <- aesthetic
    key$.value <- (lo + hi) / 2
    key$.label <- paste0(labels[-length(labels)], sep, labels[-1])
    key[rev(seq_len(nrow(key))), , drop = FALSE]
  }
)
