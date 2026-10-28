#' Save a chart at the web charts' size
#'
#' The web charts are drawn 880 by 440 pixels, a 2:1 chart at half a page
#' width. This saves a plot at that size, doubled for a sharp image on a
#' high density screen, on the chart surface colour, through ragg where it
#' is installed so the bundled Inter is used. Any size in pixels works; the
#' type sizes stay the design system's whatever the scale.
#'
#' An SVG, written through svglite, declares Inter as a web font from the
#' Fontsource CDN, so it reads in the typeface in a browser or a document
#' on a machine without Inter installed. `web_fonts = FALSE` leaves that
#' out, for an SVG that must not reach the network.
#'
#' @param plot A ggplot.
#' @param filename Where to write it. The extension picks the format: png,
#'   svg, pdf, jpeg.
#' @param width,height Size in CSS pixels.
#' @param scale Device pixels per CSS pixel. 2 is a retina screen.
#' @param theme `"light"` or `"dark"`, for the background colour. Defaults
#'   to the mode set by [gla_mode()].
#' @param web_fonts Whether an SVG declares Inter as a web font.
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
                     theme = gla_mode(), web_fonts = TRUE, ...) {
  theme <- match_mode(theme)
  args <- list(...)
  svg <- identical(tolower(tools::file_ext(filename)), "svg")
  if (svg && isTRUE(web_fonts) && is.null(args$device) &&
        requireNamespace("svglite", quietly = TRUE)) {
    fonts <- gla_web_fonts()
    args$device <- function(filename, ...) {
      svglite::svglite(filename, ..., web_fonts = fonts)
    }
  }
  rlang::exec(
    ggplot2::ggsave, filename, plot = plot, width = width * scale, height = height * scale,
    units = "px", dpi = 96 * scale, bg = unname(gla_token("chart-surface", theme)), !!!args
  )
  invisible(filename)
}

# Inter from the Fontsource CDN, under the family names the package
# registers, so an SVG's text finds its face by name
gla_web_fonts <- function() {
  url <- function(weight) {
    sprintf("https://cdn.jsdelivr.net/npm/@fontsource/inter@5/files/inter-latin-%d-normal.woff2",
            weight)
  }
  list(
    svglite::font_face("Inter", woff2 = url(400), weight = 400),
    svglite::font_face("Inter", woff2 = url(700), weight = 700),
    svglite::font_face("Inter Medium", woff2 = url(500)),
    svglite::font_face("Inter SemiBold", woff2 = url(600))
  )
}
