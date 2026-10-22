# Regenerates gla_tokens and gla_brand from the vendored token package under
# inst/tokens/ (see data-raw/refresh-tokens.R for how it gets there). This
# script is the only thing that reads those files, so R never carries a
# hand-copied colour.
#
#   Rscript data-raw/tokens.R
#
# It fails, naming the token, if the package has lost or renamed one of the
# tokens this package reads (required_tokens() in R/tokens-source.R, plus
# any token name written as a string in R/), and prints what changed against
# the data already built, so a refresh is reviewed rather than discovered.

source(file.path("R", "tokens-source.R"))

src <- read_token_files(file.path("inst", "tokens"))
built <- build_token_data(src)
gla_tokens <- built$gla_tokens
gla_brand <- built$gla_brand

# every token the package reads, declared or written as a literal in R/
literals <- unlist(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), function(f) {
  txt <- readLines(f, warn = FALSE)
  txt <- txt[!grepl("^\\s*#", txt)]
  m <- regmatches(txt, gregexpr('"[a-z]+(-[a-z0-9]+)+"', txt, perl = TRUE))
  gsub('"', "", unlist(m))
}))
groups <- "^(chart|data|palette|text|canvas|surface|border|interactive|static|container)-"
literals <- unique(literals[grepl(groups, literals) & !grepl("-$", literals)])
check_required_tokens(gla_tokens$token, union(required_tokens(), literals))
stopifnot(length(gla_brand) >= 12)

# what changed since the last build
old_file <- file.path("data", "gla_tokens.rda")
if (file.exists(old_file)) {
  env <- new.env()
  load(old_file, envir = env)
  old <- env$gla_tokens
  added <- setdiff(gla_tokens$token, old$token)
  removed <- setdiff(old$token, gla_tokens$token)
  common <- intersect(gla_tokens$token, old$token)
  new_v <- gla_tokens[match(common, gla_tokens$token), ]
  old_v <- old[match(common, old$token), ]
  changed <- common[new_v$light != old_v$light | new_v$dark != old_v$dark]
  cat(sprintf("Against the data already built: %d added, %d removed, %d changed\n",
              length(added), length(removed), length(changed)))
  if (length(added)) cat("  added:  ", paste(added, collapse = ", "), "\n")
  if (length(removed)) cat("  removed:", paste(removed, collapse = ", "), "\n")
  if (length(changed)) {
    for (t in changed) {
      cat(sprintf("  %s: %s/%s -> %s/%s\n", t,
                  old_v$light[old_v$token == t], old_v$dark[old_v$token == t],
                  new_v$light[new_v$token == t], new_v$dark[new_v$token == t]))
    }
  }
}

usethis::use_data(gla_tokens, gla_brand, overwrite = TRUE)
cat(sprintf("Wrote %d semantic tokens and %d brand colours from %s %s\n",
            nrow(gla_tokens), length(gla_brand), "@ldn-viz/themes",
            if (is.na(src$version)) "(unrecorded version)" else src$version))
