# Pulls a release of the visualisation team's token package from npm and
# vendors it under inst/tokens/, then regenerates everything built from it.
#
#   Rscript data-raw/refresh-tokens.R          # the latest release
#   Rscript data-raw/refresh-tokens.R 9.2.0    # a named release
#
# The package is @ldn-viz/themes, published from the ldn-viz-tools
# repository. Its styles/ folder carries the tokens as CSS custom properties
# in light.css, dark.css and primitives.css; those three files and the
# licence are copied in unchanged and the release is recorded in VERSION.
# data-raw/tokens.R then rebuilds gla_tokens and gla_brand, checking every
# token the package reads is still there and printing what changed; the
# compatibility colour lists and the site styling are rebuilt from those.
#
# After it runs: devtools::document(), devtools::test(), and review any
# visual snapshot that moved with testthat::snapshot_review().

package <- "@ldn-viz/themes"
registry <- "https://registry.npmjs.org/"
args <- commandArgs(trailingOnly = TRUE)
version <- if (length(args)) args[1] else "latest"

fetch <- function(url) {
  con <- url(url)
  on.exit(close(con))
  paste(readLines(con, warn = FALSE), collapse = "\n")
}

if (version == "latest") {
  meta <- fetch(paste0(registry, utils::URLencode(package, reserved = TRUE), "/latest"))
  version <- regmatches(meta, regexec('"version"\\s*:\\s*"([^"]+)"', meta))[[1]][2]
  if (is.na(version)) stop("Could not read the latest version of ", package, " from npm")
}
tarball <- sprintf("%s%s/-/%s-%s.tgz", registry, package, sub("^@[^/]+/", "", package), version)

work <- tempfile("ldn-viz-themes-")
dir.create(work)
archive <- file.path(work, "package.tgz")
cat("Fetching", tarball, "\n")
utils::download.file(tarball, archive, mode = "wb", quiet = TRUE)
utils::untar(archive, exdir = work)
styles <- file.path(work, "package", "styles")
wanted <- c("light.css", "dark.css", "primitives.css")
present <- file.exists(file.path(styles, wanted))
if (!all(present)) {
  stop("Release ", version, " does not carry ", paste(wanted[!present], collapse = ", "),
       " under styles/. The package layout has changed; look at ", styles,
       " and update this script and R/tokens-source.R together.")
}

dest <- file.path("inst", "tokens")
unlink(dest, recursive = TRUE)
dir.create(dest, recursive = TRUE)
file.copy(file.path(styles, wanted), dest)
licence <- file.path(work, "package", "LICENSE.txt")
if (file.exists(licence)) file.copy(licence, file.path(dest, "LICENSE.txt"))
writeLines(c(
  paste("package:", package),
  paste("version:", version),
  paste("url:", tarball),
  paste("date:", format(Sys.Date())),
  "files: styles/light.css, styles/dark.css, styles/primitives.css, LICENSE.txt"
), file.path(dest, "VERSION"))
cat("Vendored", package, version, "into", dest, "\n")

source("data-raw/tokens.R")
source("data-raw/compat.R")
source("data-raw/site.R")
cat("\nNow run devtools::document() and devtools::test().\n")
