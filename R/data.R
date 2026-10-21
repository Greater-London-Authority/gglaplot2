#' GLA semantic design tokens
#'
#' One row per token in the visualisation team's package, with its light and
#' dark mode values. The names are the Sass variable names minus the leading
#' `$color-`, so `text`, `chart-grid`, `data-primary`, `palette-blue-700` and
#' so on. Generated from `inst/tokens/_colors.scss` by `data-raw/tokens.R`.
#'
#' @format A data frame with columns `token`, `light` and `dark`, the last two
#'   hex colour strings.
#' @seealso [gla_token()] to read one, [gla_pal()] for the palettes built on them,
#'   [gla_show_tokens()] to see a group.
"gla_tokens"

#' GLA brand colours
#'
#' The fixed brand statics from the token package: `blue`, `darkblue`,
#' `cyan`, `green`, `yellow`, `mustard`, `orange`, `red`, `pink`,
#' `darkpink`, `purple` and `grey`. These are identity colours. For data,
#' use the `data-*` tokens through [gla_pal()], which are chosen for
#' separability and contrast on a chart.
#'
#' @format A named character vector of hex colours.
"gla_brand"

#' Demo data: three monthly series
#'
#' The series the visualisation team's Storybook demonstrates its line
#' charts with, so the examples here draw the same pictures. Synthetic
#' values in pounds, January 2015 to March 2024, for Variable A, B and C.
#' Built by `data-raw/demo-data.R` from the public ldn-viz-tools repository.
#'
#' @format A data frame with 333 rows: `month` (Date, the first of the
#'   month), `series` (character) and `value` (numeric).
#' @source <https://github.com/Greater-London-Authority/ldn-viz-tools>
"gla_monthly"

#' Demo data: three yearly series
#'
#' The yearly averages of [gla_monthly], the Storybook's bar chart data.
#'
#' @format A data frame with 30 rows: `year` (integer, 2015 to 2024),
#'   `series` (character) and `average` (numeric).
#' @source <https://github.com/Greater-London-Authority/ldn-viz-tools>
"gla_yearly"

#' London borough boundaries
#'
#' The 33 London boroughs, including the City of London, as an sf data
#' frame in WGS 84, generalised for a small-scale map. From the ONS Open
#' Geography local authority boundaries (December 2022). Contains OS data
#' (c) Crown copyright and database right 2022, Open Government Licence
#' v3. Needs the sf package to use.
#'
#' @format An sf data frame with 33 rows: `code` (the GSS code, E09...),
#'   `name` and `geometry`.
#' @source <https://geoportal.statistics.gov.uk/>
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(gla_boroughs) + geom_gla_sf() + theme_gla_map()
#' }
"gla_boroughs"
