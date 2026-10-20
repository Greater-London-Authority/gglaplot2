library(ggplot2)

.build <- function(p) ggplot2::ggplot_build(p)

test_that("theme_gla is a complete theme a plot can build with", {
  th <- theme_gla()
  expect_s3_class(th, "theme")
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + th
  expect_no_error(.build(p))
})

test_that("the theme reads the chart tokens", {
  th <- theme_gla()
  expect_equal(th$plot.background$fill, unname(gla_token("chart-surface")))
  expect_equal(th$panel.grid.major.y$colour, unname(gla_token("chart-grid")))
  expect_equal(th$axis.text$colour, unname(gla_token("chart-label-muted")))
  expect_equal(th$plot.title$colour, unname(gla_token("text")))
  expect_equal(th$plot.title.position, "plot")

  dark <- theme_gla(theme = "dark")
  expect_equal(dark$plot.background$fill, unname(gla_token("chart-surface", "dark")))
})

test_that("gridlines go on one axis and the axis line on the other", {
  y <- theme_gla(gridlines = "y")
  expect_s3_class(y$panel.grid.major.y, "element_line")
  expect_s3_class(y$panel.grid.major.x, "element_blank")
  expect_s3_class(y$axis.line.x, "element_blank")
  expect_s3_class(y$axis.line.y, "element_line")

  x <- theme_gla(gridlines = "x")
  expect_s3_class(x$panel.grid.major.x, "element_line")
  expect_s3_class(x$axis.line.y, "element_blank")
  expect_s3_class(x$axis.line.x, "element_line")

  none <- theme_gla(gridlines = "none")
  expect_s3_class(none$panel.grid.major.x, "element_blank")
  expect_s3_class(none$panel.grid.major.y, "element_blank")
})

test_that("the discrete scales colour series in palette order", {
  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point() + scale_colour_gla()
  b <- .build(p)
  used <- unique(b$data[[1]]$colour)
  expect_setequal(tolower(used), tolower(unname(gla_pal(n = 3))))
})

test_that("the sex scale maps by level name whatever the order", {
  d <- data.frame(x = 1:4, y = 1:4, sex = c("Men", "Women", "Men", "Women"))
  p <- ggplot(d, aes(x, y, colour = sex)) + geom_point() + scale_colour_gla("sex")
  b <- .build(p)
  got <- unique(b$data[[1]][, c("colour")])
  expect_true(tolower(gla_pal("sex")[["Women"]]) %in% tolower(got))
  expect_true(tolower(gla_pal("sex")[["Men"]]) %in% tolower(got))
})

test_that("too many levels for the palette is an error at build", {
  d <- data.frame(x = 1:8, y = 1:8, g = letters[1:8])
  p <- ggplot(d, aes(x, y, colour = g)) + geom_point() + scale_colour_gla()
  expect_error(.build(p), "palette has 7 colours")
})

test_that("the continuous scales build", {
  p <- ggplot(mtcars, aes(wt, mpg, colour = hp)) + geom_point() + scale_colour_gla_c()
  expect_no_error(.build(p))
  q <- ggplot(mtcars, aes(wt, mpg, fill = hp)) + geom_point(shape = 21) +
    scale_fill_gla_c("diverging")
  expect_no_error(.build(q))
})
