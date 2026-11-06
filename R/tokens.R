#' Read GLA design tokens by name
#'
#' Vectorised. An unknown name is an error rather than `NA`, because a typo in
#' a token name should fail at the call and not surface as a missing colour
#' on a chart.
#'
#' A name an earlier release of the tokens used (`container-level-1`,
#' `chart-inverse-axis`) reads the token that holds that colour now, with
#' a message naming it; see `renamed_tokens()`.
#'
#' @param name One or more token names as in [gla_tokens], e.g. `"text"`,
#'   `"chart-grid"`, `"data-primary"`.
#' @param theme `"light"` or `"dark"`. Defaults to the mode set by [gla_mode()].
#' @return A character vector of hex colours, named by the token asked for.
#' @examples
#' gla_token("data-primary")
#' gla_token(c("chart-axis", "chart-grid"), theme = "dark")
#' @export
gla_token <- function(name, theme = gla_mode()) {
  theme <- match_mode(theme)
  renamed <- renamed_tokens()
  old <- name[name %in% names(renamed)]
  if (length(old)) {
    rlang::inform(paste0(
      "Token ", old, " is now ", renamed[old], " in @ldn-viz/themes ",
      gla_token_version()$version, "; reading ", renamed[old]
    ))
  }
  hit <- match(ifelse(name %in% names(renamed), renamed[name], name), gglaplot2::gla_tokens$token)
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
#'   chart roles use regular for ticks and the footer, medium for the
#'   eyebrow, axis titles and labels, and semibold for the title; see
#'   [gla_type()].
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
#' The design system describes type by role, and its stylesheet sets each
#' piece of chart text on one of the product roles: the title on
#' `title-md` (20px semibold), the subtitle on `subtitle-sm` (16px), the
#' eyebrow on `eyebrow` (14px medium), axis titles and value labels on
#' `label-md` (14px medium), tick labels on `caption-md` (14px) and the
#' smaller tick tier and the footer on `caption-sm` (12px). This reads
#' those roles from [gla_type_roles], the table generated from the
#' vendored tokens, so a release that moves a size moves it here too.
#' [theme_gla()] sets its text from these.
#'
#' @return A list of the eight chart roles, `title`, `subtitle`, `eyebrow`,
#'   `axis_title`, `label`, `tick`, `tick_sm` and `caption`, each a list
#'   with `size` (CSS px), `weight` (`"regular"`, `"medium"`, `"semibold"`
#'   or `"bold"`), `line_height` (a multiple of the size) and `role`, the
#'   product role it is set on.
#' @examples
#' gla_type()$title
#' @export
gla_type <- function() {
  roles <- gglaplot2::gla_type_roles
  on <- c(
    title = "title-md", subtitle = "subtitle-sm", eyebrow = "eyebrow",
    axis_title = "label-md", label = "label-md", tick = "caption-md",
    tick_sm = "caption-sm", caption = "caption-sm"
  )
  lapply(on, function(role) {
    r <- roles[roles$role == role, ]
    list(size = r$size, weight = weight_name(r$weight), line_height = r$line_height,
         role = role)
  })
}

# a CSS font weight as the name gla_font() takes
weight_name <- function(weight) {
  if (weight >= 700) return("bold")
  if (weight >= 600) return("semibold")
  if (weight >= 500) return("medium")
  "regular"
}
