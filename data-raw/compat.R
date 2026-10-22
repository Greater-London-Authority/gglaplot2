# Rebuilds gglaplot's colour lists (gla_default, gla_inverse, gla_colours)
# from the token data, so a script that reads gla_colours$blue_core gets the
# token that colour is now. Run after data-raw/tokens.R:
#
#   Rscript data-raw/compat.R

env <- new.env()
load(file.path("data", "gla_tokens.rda"), envir = env)
load(file.path("data", "gla_brand.rda"), envir = env)
tk <- function(name, mode = "light") {
  i <- match(name, env$gla_tokens$token)
  if (anyNA(i)) stop("No such token: ", paste(name[is.na(i)], collapse = ", "))
  unname(env$gla_tokens[[mode]][i])
}

# gglaplot's theme colour names, each the token that plays the part now
theme_map <- c(
  "axis text & labels" = "chart-label-muted", "background" = "chart-surface",
  "body text" = "text", "context data" = "data-context", "headlines" = "text",
  "highlight area" = "chart-label", "highlighted labels" = "text", "label" = "chart-label",
  "light grid" = "chart-grid", "mid point" = "data-neutral-0", "strong grid" = "chart-axis"
)
gla_default <- as.list(stats::setNames(tk(theme_map, "light"), names(theme_map)))
gla_inverse <- as.list(stats::setNames(tk(theme_map, "dark"), names(theme_map)))

# gglaplot's 30 colour names: the core set is the categorical tokens, the
# light and dark sets are steps on the palette ramps, the brand set the
# brand statics
core <- c(blue = "blue", ldndkpink = "darkpink", yellow = "yellow", red = "red", green = "green",
          purple = "purple", turquoise = "turquoise", pink = "pink", orange = "orange",
          ldnpink = "pink")
light <- c(blue = "blue", purple = "purple", yellow = "yellow", red = "red", green = "green",
           turquoise = "turquoise", orange = "orange")
dark <- c(blue = "blue", magenta = "darkpink", yellow = "yellow", red = "red", green = "green",
          turquoise = "turquoise", purple = "purple", orange = "orange")
brand <- c(pink = "pink", cyan = "cyan", dkpink = "darkpink", mustard = "mustard", red = "red")
gla_colours <- c(
  stats::setNames(as.list(tk(paste0("data-categorical-", core))), paste0(names(core), "_core")),
  stats::setNames(as.list(tk(paste0("palette-", light, "-500"))), paste0(names(light), "_light")),
  stats::setNames(as.list(tk(paste0("palette-", dark, "-800"))), paste0(names(dark), "_dark")),
  stats::setNames(as.list(unname(env$gla_brand[brand])), paste0(names(brand), "_brand"))
)
stopifnot(length(gla_colours) == 30, !anyNA(unlist(gla_colours)))

usethis::use_data(gla_default, gla_inverse, gla_colours, overwrite = TRUE)
cat("Wrote gla_default, gla_inverse and gla_colours from the tokens\n")
