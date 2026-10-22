#' Save a chart at the web charts' size
#'
#' The web charts are drawn 880 by 440 pixels, a 2:1 chart at half a page
#' width. This saves a plot at that size, doubled for a sharp image on a
#' high density screen, on the chart surface colour, through ragg where it
#' is installed so the bundled Inter is used. Any size in pixels works; the
#' type sizes stay the design system's whatever the scale.
#'
#' @param plot A ggplot.
#' @param filename Where to write it. The extension picks the format: png,
#'   svg, pdf, jpeg.
#' @param width,height Size in CSS pixels.
#' @param scale Device pixels per CSS pixel. 2 is a retina screen.
#' @param theme `"light"` or `"dark"`, for the background colour. Defaults
#'   to the mode set by [gla_mode()].
#' @param ... Passed to [ggplot2::ggsave()].
#' @return The filename, invisibly.
#' @examples
#' library(ggplot2)
#' p <- ggplot(gla_yearly, aes(year, average, colour = series)) +
#'   geom_gla_line() +
#'   scale_colour_gla() +
#'   theme_gla()
#' gla_save(p, file.path(tempdir(), "chart.png"))
#' @export
gla_save <- function(plot, filename, width = 880, height = 440, scale = 2,
                     theme = gla_mode(), ...) {
  theme <- match_mode(theme)
  ggplot2::ggsave(
    filename, plot = plot, width = width * scale, height = height * scale, units = "px",
    dpi = 96 * scale, bg = unname(gla_token("chart-surface", theme)), ...
  )
  invisible(filename)
}
