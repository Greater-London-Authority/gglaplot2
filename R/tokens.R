#' Read GLA design tokens by name
#'
#' Vectorised. An unknown name is an error rather than `NA`, because a typo in
#' a token name should fail at the call and not surface as a missing colour
#' on a chart.
#'
#' @param name One or more token names as in [gla_tokens], e.g. `"text"`,
#'   `"chart-grid"`, `"data-primary"`.
#' @param theme `"light"` or `"dark"`.
#' @return A character vector of hex colours, named by token.
#' @examples
#' gla_token("data-primary")
#' gla_token(c("chart-axis", "chart-grid"), theme = "dark")
#' @export
gla_token <- function(name, theme = c("light", "dark")) {
  theme <- match.arg(theme)
  hit <- match(name, gglaplot2::gla_tokens$token)
  if (anyNA(hit)) {
    stop("Unknown token: ", paste(name[is.na(hit)], collapse = ", "), call. = FALSE)
  }
  stats::setNames(gglaplot2::gla_tokens[[theme]][hit], name)
}

# the chart.* group as a list, the same keys the dashboards read
chart_tokens <- function(theme = "light") {
  t <- function(x) unname(gla_token(x, theme))
  list(
    axis = t("chart-axis"), grid = t("chart-grid"), label = t("chart-label"),
    label_muted = t("chart-label-muted"), surface = t("chart-surface"),
    text = t("text"), text_muted = t("text-muted")
  )
}
