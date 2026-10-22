# Reads the vendored token package under inst/tokens/. Base R only, so
# data-raw/tokens.R can source this file on its own before the package is
# built, and the tests can read the vendored files the same way the build
# does.

#' The tokens the package reads by name
#'
#' Every token `theme_gla()`, the palettes, the marks and the site read.
#' A refresh of the token package that drops or renames one of these fails
#' in `data-raw/tokens.R` and in the test suite with the name, rather than
#' on somebody's chart.
#'
#' @return A character vector of token names.
#' @keywords internal
required_tokens <- function() {
  scales <- c("blue", "darkpink", "green", "grey", "orange", "pink", "purple", "red",
              "turquoise", "yellow")
  c(
    # the chart's non-data ink
    "text", "text-muted", "chart-axis", "chart-grid", "chart-label", "chart-label-muted",
    "chart-surface",
    # what data is drawn in
    "data-primary", "data-secondary", "data-tertiary", "data-context", "data-empty",
    "data-positive", "data-negative", "data-female", "data-male", "data-neutral-0",
    paste0("data-categorical-", c("blue", "darkpink", "green", "orange", "pink", "purple",
                                  "red", "turquoise", "yellow")),
    # the ramps the sequential and diverging palettes are built from
    paste0("palette-", rep(scales, each = 9), "-", rep(seq(100, 900, by = 100), length(scales))),
    # the documentation site
    "canvas", "canvas-muted", "surface", "surface-muted", "container-level-1", "border",
    "border-muted", "text-wash", "interactive-primary-text", "interactive-primary-text-hover",
    "surface-accent", "surface-accent-muted", "static-brand"
  )
}

#' Read the vendored token files
#'
#' The visualisation team publish the tokens as `@ldn-viz/themes` on npm,
#' whose `styles/` folder has `light.css`, `dark.css` and `primitives.css`
#' as CSS custom properties. This reads those, resolving a `var()`
#' reference to its fallback or to the property it names. It also reads the
#' Sass form (`_colors.scss`, `_primitives.scss`) an older checkout vendored,
#' so either regenerates.
#'
#' @param dir The folder holding the files.
#' @return A list: `light`, `dark` and `primitives`, each a character vector
#'   of hex colours named by token, and `version`, the package version
#'   recorded by `data-raw/refresh-tokens.R` or `NA`.
#' @keywords internal
read_token_files <- function(dir) {
  css <- file.path(dir, c("light.css", "dark.css", "primitives.css"))
  scss <- file.path(dir, c("_colors.scss", "_primitives.scss"))
  if (all(file.exists(css))) {
    out <- list(
      light = read_css_tokens(css[1]), dark = read_css_tokens(css[2]),
      primitives = read_css_tokens(css[3])
    )
  } else if (all(file.exists(scss))) {
    colours <- read_scss_tokens(scss[1])
    is_dark <- grepl("-dark$", names(colours))
    dark <- colours[is_dark]
    names(dark) <- sub("-dark$", "", names(dark))
    out <- list(light = colours[!is_dark], dark = dark, primitives = read_scss_tokens(scss[2]))
  } else {
    stop("No token package under ", dir, ": expected light.css, dark.css and primitives.css ",
         "(or the Sass pair _colors.scss and _primitives.scss)", call. = FALSE)
  }
  out$light <- strip_prefix(out$light, "^color-")
  out$dark <- strip_prefix(out$dark, "^color-")
  for (part in c("light", "dark", "primitives")) {
    if (!length(out[[part]])) {
      stop("No tokens read from the ", part, " file under ", dir, call. = FALSE)
    }
  }
  version_file <- file.path(dir, "VERSION")
  out$version <- if (file.exists(version_file)) read_token_version(version_file)$version else NA
  out
}

# "--color-text: #353d42;" and "--color-data-primary: var(--color-x, #007acc);"
# both give a hex; "var(--color-x)" with no fallback is resolved against the
# other properties in the same file
read_css_tokens <- function(path) {
  lines <- readLines(path, warn = FALSE)
  re <- "^\\s*--([a-z0-9-]+):\\s*(.+?);\\s*$"
  m <- regmatches(lines, regexec(re, lines, perl = TRUE))
  m <- m[lengths(m) == 3]
  values <- stats::setNames(vapply(m, `[[`, character(1), 3), vapply(m, `[[`, character(1), 2))
  resolve <- function(v, depth = 0) {
    if (grepl("^#[0-9a-fA-F]{6}([0-9a-fA-F]{2})?$", v)) return(tolower(v))
    fallback <- regmatches(v, regexec("var\\(--[a-z0-9-]+,\\s*(#[0-9a-fA-F]{6,8})\\)", v))[[1]]
    if (length(fallback) == 2) return(tolower(fallback[2]))
    ref <- regmatches(v, regexec("^var\\(--([a-z0-9-]+)\\)$", v))[[1]]
    if (length(ref) == 2 && ref[2] %in% names(values) && depth < 10) {
      return(resolve(values[[ref[2]]], depth + 1))
    }
    NA_character_
  }
  hex <- vapply(values, resolve, character(1))
  # a reference that resolves to nothing is a broken file; a plain value
  # that is not a colour (a font family, a weight) is just not a colour token
  bad <- names(hex)[is.na(hex) & grepl("var(", values, fixed = TRUE)]
  if (length(bad)) {
    stop(basename(path), " has ", length(bad), " token(s) whose reference resolves to no colour: ",
         paste(utils::head(bad, 5), collapse = ", "), call. = FALSE)
  }
  hex[!is.na(hex)]
}

# the Sass form: a $color-<name> variable declared !default with a hex value
read_scss_tokens <- function(path) {
  lines <- readLines(path, warn = FALSE)
  re <- "^\\$([a-z0-9-]+):\\s*(#[0-9a-fA-F]{6,8})\\s*!default;"
  m <- regmatches(lines, regexec(re, lines, perl = TRUE))
  m <- m[lengths(m) == 3]
  stats::setNames(tolower(vapply(m, `[[`, character(1), 3)), vapply(m, `[[`, character(1), 2))
}

strip_prefix <- function(x, prefix) {
  names(x) <- sub(prefix, "", names(x))
  x
}

# the VERSION file data-raw/refresh-tokens.R writes: "key: value" lines
read_token_version <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[grepl(":", lines, fixed = TRUE)]
  key <- trimws(sub(":.*$", "", lines))
  value <- trimws(sub("^[^:]*:", "", lines))
  as.list(stats::setNames(value, key))
}

#' Build the token data from the vendored files
#'
#' One row per semantic token with its light and dark values (a token with
#' no dark value keeps its light one, so either mode reads without a
#' missing-colour branch), and the brand statics by short name.
#'
#' @param src What [read_token_files()] returned.
#' @return A list of `gla_tokens` (a data frame) and `gla_brand` (a named
#'   character vector).
#' @keywords internal
build_token_data <- function(src) {
  light <- src$light
  dark <- src$dark[names(light)]
  dark <- ifelse(is.na(dark), light, dark)
  tokens <- data.frame(token = names(light), light = unname(light), dark = unname(dark),
                       stringsAsFactors = FALSE)
  tokens <- tokens[order(tokens$token), ]
  rownames(tokens) <- NULL
  brand <- src$primitives[grepl("^primitive-color-static-gla-", names(src$primitives))]
  brand <- strip_prefix(brand, "^primitive-color-static-gla-")
  list(gla_tokens = tokens, gla_brand = brand[order(names(brand))])
}

#' Check a token set has everything the package reads
#'
#' @param tokens The token names a build produced.
#' @param required The names the package needs.
#' @return `tokens`, invisibly. Errors with the missing names.
#' @keywords internal
check_required_tokens <- function(tokens, required = required_tokens()) {
  missing <- setdiff(required, tokens)
  if (length(missing)) {
    stop("The token package is missing ", length(missing), " token(s) this package reads: ",
         paste(missing, collapse = ", "),
         ". Either the tokens were renamed upstream, in which case update required_tokens() ",
         "and the code that reads them, or the vendored files are incomplete.", call. = FALSE)
  }
  invisible(tokens)
}

#' The release of the token package the data was built from
#'
#' The visualisation team publish their design tokens as `@ldn-viz/themes`
#' on npm. `data-raw/refresh-tokens.R` vendors a release and records it; this
#' reads the record, so a chart can say which tokens it was drawn with.
#'
#' @return A list with `package`, `version`, `url` and `date`, or `NULL`
#'   when no record is present.
#' @examples
#' gla_token_version()$version
#' @export
gla_token_version <- function() {
  path <- system.file("tokens", "VERSION", package = "gglaplot2")
  if (!nzchar(path)) return(NULL)
  read_token_version(path)
}
