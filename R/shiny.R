#' A download button for a chart in a Shiny app
#'
#' The web charts' container offers the chart as an image. These do the
#' same for a chart drawn with this package in a Shiny app:
#' `gla_download_button()` is the button for the UI and `gla_download()`
#' the handler for the server, writing the plot through [gla_save()] at the
#' web charts' size. The file is named from the chart's title, `londons-
#' gender-pay-gap-has-narrowed.svg`, unless `filename` says otherwise. An
#' SVG carries the Inter web font, so it reads in the typeface wherever it
#' is opened.
#'
#' @param id The output id: the button's, and the name of the output the
#'   handler is assigned to.
#' @param label The button's text.
#' @param ... Passed to [shiny::downloadButton()].
#' @param plot A ggplot, or a reactive expression or function that returns
#'   one.
#' @param filename The file name without its extension. `NULL`, the
#'   default, slugs the chart's title.
#' @param format `"svg"` or `"png"`.
#' @inheritParams gla_save
#' @return `gla_download_button()` returns a button tag; `gla_download()` a
#'   download handler to assign to an output.
#' @examples
#' if (requireNamespace("shiny", quietly = TRUE)) {
#'   library(ggplot2)
#'   ui <- shiny::fluidPage(
#'     shiny::plotOutput("chart"),
#'     gla_download_button("save")
#'   )
#'   server <- function(input, output, session) {
#'     chart <- shiny::reactive({
#'       ggplot(gla_yearly, aes(year, average, colour = series)) +
#'         geom_gla_line() +
#'         scale_colour_gla() +
#'         gla_labs(title = "Every variable fell") +
#'         theme_gla()
#'     })
#'     output$chart <- shiny::renderPlot(chart())
#'     output$save <- gla_download(chart)
#'   }
#'   if (interactive()) shiny::shinyApp(ui, server)
#' }
#' @name gla_download
NULL

#' @rdname gla_download
#' @export
gla_download_button <- function(id, label = "Download SVG", ...) {
  rlang::check_installed("shiny", "for the download button.")
  shiny::downloadButton(id, label, ...)
}

#' @rdname gla_download
#' @export
gla_download <- function(plot, filename = NULL, format = c("svg", "png"), width = 880,
                         height = 440, scale = 2, theme = gla_mode()) {
  rlang::check_installed("shiny", "for the download handler.")
  format <- match.arg(format)
  get_plot <- if (is.function(plot)) plot else function() plot
  shiny::downloadHandler(
    filename = function() {
      paste0(filename %||% chart_slug(get_plot()), ".", format)
    },
    content = function(file) {
      gla_save(get_plot(), file, width = width, height = height, scale = scale, theme = theme)
    },
    contentType = if (format == "svg") "image/svg+xml" else "image/png"
  )
}

# a file name from the chart's title: lower case, words joined by hyphens
chart_slug <- function(plot) {
  title <- plot$labels$title
  title <- if (is.null(title) || inherits(title, "waiver")) "" else as.character(title)[1]
  slug <- tolower(gsub("[^A-Za-z0-9]+", "-", title))
  slug <- gsub("^-+|-+$", "", slug)
  if (nzchar(slug)) slug else "chart"
}
