library(ggplot2)

# the snapshots are drawn with the bundled Inter through svglite, which
# knows registered fonts, so they measure the same on every machine
options(gglaplot2.font = "inter")

# the once-a-session notes from the gglaplot names are tested on their own
options(rlib_message_verbosity = "quiet")

build <- function(p) ggplot2::ggplot_build(p)
a_series <- gla_monthly[gla_monthly$series == "Variable A", ]

# draw on ragg so the registered font is found and no base device warns
draw <- function(p) {
  f <- tempfile(fileext = ".png")
  ragg::agg_png(f)
  on.exit({
    grDevices::dev.off()
    unlink(f)
  }, add = TRUE)
  print(p)
  invisible(TRUE)
}

# evaluate with a ragg device open, for grobs that measure text
with_device <- function(expr) {
  f <- tempfile(fileext = ".png")
  ragg::agg_png(f)
  on.exit({
    grDevices::dev.off()
    unlink(f)
  }, add = TRUE)
  expr
}
