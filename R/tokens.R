#' Read GLA design tokens by name
#'
#' Vectorised. An unknown name is an error rather than `NA`, because a typo in
#' a token name should fail at the call and not surface as a missing colour
#' on a chart.
#'
#' @param name One or more token names as in [gla_tokens], e.g. `"text"`,
#'   `"chart-grid"`, `"data-primary"`.
#' @param theme `"light"` or `"dark"`. Defaults to the mode set by [gla_mode()].
#' @return A character vector of hex colours, named by token.
#' @examples
#' gla_token("data-primary")
#' gla_token(c("chart-axis", "chart-grid"), theme = "dark")
#' @export
gla_token <- function(name, theme = gla_mode()) {
  theme <- match_mode(theme)
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
    text = t("text"), text_muted = t("text-muted"),
    primary = t("data-primary"), context = t("data-context")
  )
}

#' The design system's typeface, by weight
#'
#' Inter is the font the tokens name. The package bundles it and registers
#' it with systemfonts when the package loads, so ragg, svglite and the
#' knitr and pkgdown devices built on them draw it on any machine. The base
#' graphics devices (quartz, pdf, the Windows device) only know fonts
#' installed on the system, so the family is only used by default when Inter
#' is installed or the current device can see registered fonts. Set
#' `options(gglaplot2.font = "inter")` to use it regardless, or
#' `options(gglaplot2.font = "sans")` to never use it.
#'
#' @param weight `"regular"`, `"medium"`, `"semibold"` or `"bold"`. The
#'   chart roles use regular for ticks and labels, medium for axis titles and
#'   semibold for the title.
#' @return A family name for `ggplot2::element_text()`, or `""` for the
#'   device's sans when Inter is not available.
#' @examples
#' gla_font()
#' gla_font("semibold")
#' @export
gla_font <- function(weight = c("regular", "medium", "semibold", "bold")) {
  weight <- match.arg(weight)
  family <- switch(weight, regular = "Inter", bold = "Inter", medium = "Inter Medium",
                   semibold = "Inter SemiBold")
  if (!inter_available(family)) return("")
  family
}

# whether a family can be drawn on the device the user is likely to use
inter_available <- function(family) {
  pref <- getOption("gglaplot2.font", "auto")
  if (identical(pref, "sans")) return(FALSE)
  if (identical(pref, "inter")) return(TRUE)
  if (!requireNamespace("systemfonts", quietly = TRUE)) return(FALSE)
  if (family %in% systemfonts::system_fonts()$family) return(TRUE)
  # registered only: fine for ragg and svglite, which the agg devices and
  # knitr use, not for the base devices
  dev <- names(grDevices::dev.cur())
  grepl("agg|svglite|ragg", dev) && family %in% systemfonts::registry_fonts()$family
}

#' The chart typography roles
#'
#' The design system's chart type set, in CSS pixels: `title` 20 semibold,
#' `subtitle` 16, `axis_title` 14 medium, `label` 14, `tick` 14, `tick_sm`
#' 12 and `caption` 12. [theme_gla()] sets its text from these.
#'
#' @return A list of roles, each with `size` (px) and `weight`.
#' @examples
#' gla_type()$title
#' @export
gla_type <- function() {
  list(
    title = list(size = 20, weight = "semibold"),
    subtitle = list(size = 16, weight = "regular"),
    axis_title = list(size = 14, weight = "medium"),
    label = list(size = 14, weight = "regular"),
    tick = list(size = 14, weight = "regular"),
    tick_sm = list(size = 12, weight = "regular"),
    caption = list(size = 12, weight = "regular")
  )
}
