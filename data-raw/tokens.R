# Regenerates the token data from the vendored GLA design tokens under
# inst/tokens/. Those files are the visualisation team's package, dropped in
# unchanged; this script is the only thing that reads them, so R never carries
# a hand-copied colour.
#
# Run after refreshing the token package:
#   Rscript data-raw/tokens.R
#
# _colors.scss carries both modes: $color-<x> is the light value and
# $color-<x>-dark the dark one, each declared `!default`. _primitives.scss
# carries the brand statics ($primitive-color-static-gla-<name>) and the
# palette scales the semantic tokens are built from.

token_re <- "^\\$([a-z0-9-]+):\\s*(#[0-9a-fA-F]{6,8})\\s*!default;"

read_tokens <- function(path) {
  lines <- readLines(path)
  m <- regmatches(lines, regexec(token_re, lines, perl = TRUE))
  m <- m[lengths(m) == 3]
  stats::setNames(vapply(m, `[[`, character(1), 3), vapply(m, `[[`, character(1), 2))
}

colours <- read_tokens(file.path("inst", "tokens", "_colors.scss"))
primitives <- read_tokens(file.path("inst", "tokens", "_primitives.scss"))
stopifnot(length(colours) > 0, length(primitives) > 0)

# the semantic set: one row per token with its light and dark values. A token
# with no dark variant keeps its light value in both, so a theme can be read
# in either mode without a missing-colour branch.
is_dark <- grepl("-dark$", names(colours))
light <- colours[!is_dark]
dark <- colours[is_dark]
names(dark) <- sub("-dark$", "", names(dark))
names(light) <- sub("^color-", "", names(light))
names(dark) <- sub("^color-", "", names(dark))

gla_tokens <- data.frame(
  token = names(light),
  light = unname(light),
  dark = unname(ifelse(is.na(dark[names(light)]), light, dark[names(light)])),
  stringsAsFactors = FALSE
)
gla_tokens <- gla_tokens[order(gla_tokens$token), ]
rownames(gla_tokens) <- NULL

# the brand statics, keyed by their short name: "blue", "darkpink", ...
brand <- primitives[grepl("^primitive-color-static-gla-", names(primitives))]
names(brand) <- sub("^primitive-color-static-gla-", "", names(brand))
gla_brand <- brand[order(names(brand))]

usethis::use_data(gla_tokens, gla_brand, overwrite = TRUE)
cat(sprintf("Wrote %d semantic tokens and %d brand colours\n", nrow(gla_tokens), length(gla_brand)))
