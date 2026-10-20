#' GLA semantic design tokens
#'
#' One row per token in the visualisation team's package, with its light and
#' dark mode values. The names are the Sass variable names minus the leading
#' `$color-`, so `text`, `chart-grid`, `data-primary`, `palette-blue-700` and
#' so on. Generated from `inst/tokens/_colors.scss` by `data-raw/tokens.R`.
#'
#' @format A data frame with columns `token`, `light` and `dark`, the last two
#'   hex colour strings.
#' @seealso [gla_token()] to read one, [gla_pal()] for the palettes built on them.
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
