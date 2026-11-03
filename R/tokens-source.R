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
    "canvas", "canvas-muted", "surface", "surface-muted", "border", "border-wash",
    "text-wash", "interactive-primary-text", "interactive-primary-text-hover",
    "surface-accent", "surface-accent-muted", "static-brand",
    # the names an earlier release used, which gla_token() still answers to
    unname(renamed_tokens())
  )
}

#' Tokens an earlier release of the token package named differently
#'
#' `@ldn-viz/themes` 10 renamed the container tokens to surface and canvas
#' names and moved `inverse` to the front of the chart and geo inverse
#' tokens. A script written against the old names still reads: the pairs
#' here are the old name and the token that holds the same colour now, and
#' [gla_token()] translates and says so.
#'
#' @return A named character vector, the old name to the current one.
#' @keywords internal
renamed_tokens <- function() {
  c(
    "container" = "surface", "container-level-1" = "surface-muted",
    "container-level-2" = "palette-grey-200", "container-level-3" = "palette-grey-300",
    "inverse-container" = "inverse-canvas", "inverse-container-level-1" = "inverse-surface",
    "inverse-container-level-2" = "palette-grey-1000",
    "inverse-container-level-3" = "palette-grey-900",
    "chart-inverse-axis" = "inverse-chart-axis", "chart-inverse-grid" = "inverse-chart-grid",
    "chart-inverse-label" = "inverse-chart-label",
    "chart-inverse-label-muted" = "inverse-chart-label-muted",
    "chart-inverse-surface" = "inverse-chart-surface",
    "geo-inverse-feature" = "inverse-geo-feature",
    "geo-inverse-feature-muted" = "inverse-geo-feature-muted"
  )
}

#' Read the vendored token files
#'
#' The visualisation team publish the tokens as `@ldn-viz/themes` on npm,
#' whose `styles/tokens.css` carries every token as a CSS custom property:
#' the light mode and the primitives in a `:root` block and the dark mode
#' in a `:root.dark, .dark` block after it. This reads the colour tokens
#' out of both, resolving a `var()` reference to its fallback or to the
#' property it names, and leaves the typography, spacing and shadow tokens
#' in the file.
#'
#' @param dir The folder holding `tokens.css`.
#' @return A list: `light`, `dark` and `primitives`, each a character vector
#'   of hex colours named by token, and `version`, the package version
#'   recorded by `data-raw/refresh-tokens.R` or `NA`.
#' @keywords internal
read_token_files <- function(dir) {
  path <- file.path(dir, "tokens.css")
  if (!file.exists(path)) {
    stop("No token package under ", dir, ": expected tokens.css", call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  root <- css_block(lines, "^:root\\s*\\{", path)
  dark <- css_block(lines, "^:root\\.dark\\b.*\\{", path)
  root <- read_css_tokens(root, path)
  primitive <- grepl("^primitive-", names(root))
  out <- list(
    light = strip_prefix(root[!primitive], "^color-"),
    dark = strip_prefix(read_css_tokens(dark, path), "^color-"),
    primitives = root[primitive]
  )
  for (part in c("light", "dark", "primitives")) {
    if (!length(out[[part]])) {
      stop("No ", part, " tokens read from ", path, call. = FALSE)
    }
  }
  version_file <- file.path(dir, "VERSION")
  out$version <- if (file.exists(version_file)) read_token_version(version_file)$version else NA
  out
}

# the lines inside the first rule set whose selector line matches
css_block <- function(lines, selector, path) {
  from <- grep(selector, lines)
  if (!length(from)) {
    stop(basename(path), " has no block matching ", selector, call. = FALSE)
  }
  from <- from[1]
  to <- grep("^\\}", lines)
  to <- to[to > from]
  if (!length(to)) stop(basename(path), " has an unclosed block at line ", from, call. = FALSE)
  lines[seq(from + 1, to[1] - 1)]
}

# "--color-text: #353d42;" and "--color-data-primary: var(--color-x, #007acc);"
# both give a hex; "var(--color-x)" with no fallback is resolved against the
# other properties in the same block
read_css_tokens <- function(lines, path = "tokens.css") {
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
  # a colour token that resolves to no colour is a broken file; a token of
  # another kind (a font family, a shadow, a length) is just not a colour
  bad <- names(hex)[is.na(hex) & grepl("^(color|primitive-color)-", names(hex))]
  if (length(bad)) {
    stop(basename(path), " has ", length(bad), " colour token(s) that resolve to no colour: ",
         paste(utils::head(bad, 5), collapse = ", "), call. = FALSE)
  }
  hex[!is.na(hex)]
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
