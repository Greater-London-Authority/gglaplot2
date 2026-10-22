#' Light or dark, for every function at once
#'
#' Every function that colours something takes a `theme` argument,
#' `"light"` or `"dark"`, and each defaults to the mode set here. So a dark
#' chart needs the mode set once, not `theme = "dark"` on the theme, the
#' scale, the reference line and the range separately, and nothing is left
#' in light-mode grey on a dark surface.
#'
#' `gla_mode()` reads the mode; `gla_mode("dark")` sets it for the session
#' and returns the previous mode invisibly. `with_gla_mode()` sets it for
#' one block of code, for a dark chart in a light document. Dark is for a
#' chart on an inverse surface, not a reading of the device's dark-mode
#' setting.
#'
#' @param mode `"light"` or `"dark"`, or `NULL` to read the current mode.
#' @param code Code to run with the mode set.
#' @return `gla_mode()` the current mode, or the previous one invisibly
#'   when setting. `with_gla_mode()` the value of `code`.
#' @examples
#' library(ggplot2)
#' gla_mode()
#' with_gla_mode("dark", {
#'   ggplot(gla_yearly, aes(year, average, colour = series)) +
#'     geom_gla_line() +
#'     gla_reference_line(30000, "Target") +
#'     scale_colour_gla() +
#'     theme_gla()
#' })
#' @name gla_mode
NULL

#' @rdname gla_mode
#' @export
gla_mode <- function(mode = NULL) {
  current <- match_mode(getOption("gglaplot2.mode", "light"))
  if (is.null(mode)) return(current)
  options(gglaplot2.mode = match_mode(mode))
  invisible(current)
}

#' @rdname gla_mode
#' @export
with_gla_mode <- function(mode, code) {
  old <- options(gglaplot2.mode = match_mode(mode))
  on.exit(options(old), add = TRUE)
  force(code)
}

# the two modes, by their names or by the option
match_mode <- function(theme) {
  if (is.null(theme)) return(gla_mode())
  match.arg(theme, c("light", "dark"))
}
