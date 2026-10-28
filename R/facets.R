#' Small multiples, one above the other
#'
#' The web charts' vertically faceted chart: one panel per level of a
#' variable, stacked in a single column, each a full chart with its own
#' axes and the level as its heading. [ggplot2::facet_wrap()] with
#' `ncol = 1` and the axes drawn on every panel; this names the layout and
#' fixes those two choices.
#'
#' The web chart gives each panel its own y range, which is `scales =
#' "free_y"`, the default here. Shared scales (`"fixed"`) make the panels
#' comparable at a glance and are the better choice when the levels are on
#' the same measure at a similar size; pick them deliberately. Each panel
#' is coloured by its own series when `colour` is mapped to the facet
#' variable, so the legend repeats the headings: `theme_gla(legend =
#' "none")` drops it.
#'
#' @param facets The variable to facet by, as [ggplot2::vars()] or a
#'   formula.
#' @param scales Which scales vary between panels: `"free_y"` (the
#'   default), `"fixed"`, `"free_x"` or `"free"`.
#' @param ... Passed on to [ggplot2::facet_wrap()].
#' @return A facet specification to add to a plot.
#' @examples
#' library(ggplot2)
#' ggplot(gla_monthly, aes(month, value, colour = series)) +
#'   geom_gla_line() +
#'   facet_gla_rows(vars(series)) +
#'   scale_colour_gla() +
#'   gla_labs(title = "Each variable on its own", subtitle = "Monthly value, GBP",
#'            x = NULL) +
#'   theme_gla(legend = "none")
#' @export
facet_gla_rows <- function(facets, scales = c("free_y", "fixed", "free_x", "free"), ...) {
  scales <- match.arg(scales)
  ggplot2::facet_wrap(facets, ncol = 1, scales = scales, axes = "all", ...)
}
