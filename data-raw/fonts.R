# Fetches the Inter font files the package bundles under inst/fonts/. Inter
# is the design system's typeface (the primitive-typography-font-family
# token) and is licensed under the SIL Open Font License, so it can ship with
# the package; the licence text comes along with it. Bundling it means a
# chart renders with the right face on any machine through ragg or svglite,
# and the visual regression snapshots measure the same font everywhere.
#
#   Rscript data-raw/fonts.R [version]

version <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(version)) version <- "4.1"
url <- sprintf("https://github.com/rsms/inter/releases/download/v%s/Inter-%s.zip", version, version)
tmp <- tempfile(fileext = ".zip")
download.file(url, tmp, mode = "wb", quiet = TRUE)
unzipped <- tempfile()
utils::unzip(tmp, exdir = unzipped)

wanted <- c("Inter-Regular.ttf", "Inter-Italic.ttf", "Inter-Medium.ttf",
            "Inter-SemiBold.ttf", "Inter-Bold.ttf", "Inter-BoldItalic.ttf")
found <- list.files(unzipped, pattern = paste0("^(", paste(wanted, collapse = "|"), ")$"),
                    recursive = TRUE, full.names = TRUE)
found <- found[grepl("/ttf/", found)]
stopifnot(setequal(basename(found), wanted))
licence <- list.files(unzipped, pattern = "^LICENSE", recursive = TRUE, full.names = TRUE)[1]

dir.create(file.path("inst", "fonts"), showWarnings = FALSE, recursive = TRUE)
file.copy(found, file.path("inst", "fonts", basename(found)), overwrite = TRUE)
file.copy(licence, file.path("inst", "fonts", "LICENSE.txt"), overwrite = TRUE)
writeLines(sprintf("Inter %s from https://github.com/rsms/inter, SIL Open Font License 1.1 (LICENSE.txt).", version),
           file.path("inst", "fonts", "README"))
cat(sprintf("Inter %s: %d files in inst/fonts\n", version, length(found) + 1))
