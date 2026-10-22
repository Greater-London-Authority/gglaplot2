test_that("theme_gla is a complete theme a plot can build with", {
  th <- theme_gla()
  expect_s3_class(th, "theme")
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + th
  expect_no_error(build(p))
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
  expect_equal(dark$geom@accent, unname(gla_token("data-primary", "dark")))
})

test_that("the type sizes are the chart roles, in points", {
  th <- theme_gla()
  roles <- gla_type()
  expect_equal(th$plot.title$size, roles$title$size * 0.75)
  expect_equal(th$plot.subtitle$size, roles$subtitle$size * 0.75)
  expect_equal(th$axis.text$size, roles$tick$size * 0.75)
  expect_equal(th$plot.caption$size, roles$caption$size * 0.75)
  # the semibold title is a named Inter family, not a bold face
  expect_equal(th$plot.title$family, "Inter SemiBold")
  expect_equal(th$axis.title$family, "Inter Medium")
  # scaled from the tick size
  expect_equal(theme_gla(base_size = 28)$plot.title$size, roles$title$size * 2 * 0.75)
})

test_that("without Inter the weights fall back to faces on the base family", {
  th <- withr::with_options(list(gglaplot2.font = "sans"), theme_gla())
  expect_equal(th$plot.title$family, "")
  expect_equal(th$plot.title$face, "bold")
  expect_equal(th$axis.title$face, "plain")
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

test_that("axis titles are off on y by default, as on the web charts", {
  th <- theme_gla()
  expect_s3_class(th$axis.title.y, "element_blank")
  expect_s3_class(th$axis.title.x, "element_text")
  both <- theme_gla(axis_titles = "both")
  expect_s3_class(both$axis.title.y, "element_text")
  none <- theme_gla(axis_titles = "none")
  expect_s3_class(none$axis.title.x, "element_blank")
})

test_that("the geom defaults are the design system's marks", {
  g <- theme_gla()$geom
  expect_equal(g@accent, unname(gla_token("data-primary")))
  expect_equal(g@paper, unname(gla_token("chart-surface")))
  expect_equal(g@ink, unname(gla_token("chart-label")))
  # a 2px line, a 1px edge
  expect_equal(g@linewidth, 2 * 0.75 / .pt)
  expect_equal(g@borderwidth, 0.75 / .pt)
})

test_that("the map theme strips the axes", {
  th <- theme_gla_map()
  expect_s3_class(th$axis.text, "element_blank")
  expect_s3_class(th$panel.grid, "element_blank")
  expect_equal(th$legend.position, "right")
})

test_that("gla_breaks aims for four", {
  b <- gla_breaks()(c(0, 100))
  expect_true(length(b) >= 3 && length(b) <= 6)
})
