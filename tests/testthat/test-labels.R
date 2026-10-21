test_that("gla_caption writes the footer in the web order", {
  expect_equal(gla_caption(source = "ONS", note = "Provisional", byline = "GLA"),
               "GLA\nSource: ONS\nNote: Provisional")
  expect_equal(gla_caption(source = "ONS"), "Source: ONS")
  expect_equal(gla_caption(), "")
})

test_that("gla_labs sets title, subtitle and caption", {
  l <- gla_labs(title = "T", subtitle = "S", source = "ONS", byline = "GLA")
  expect_equal(l$title, "T")
  expect_equal(l$caption, "GLA\nSource: ONS")
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + gla_labs(title = "T", source = "ONS")
  expect_equal(p$labels$caption, "Source: ONS")
})

test_that("gla_describe reads the chart type, the series and the range", {
  p <- ggplot(gla_yearly, aes(year, average, colour = series)) + geom_gla_line()
  expect_equal(
    gla_describe(p),
    paste("Line chart, with lines for Variable A, Variable B and Variable C,",
          "showing values between 2015 and 2024.")
  )
  one <- ggplot(a_series, aes(month, value)) + geom_gla_line()
  expect_equal(gla_describe(one), "Line chart, showing values between 2015 and 2024.")
  bars <- ggplot(gla_yearly, aes(factor(year), average, fill = series)) + geom_gla_col()
  expect_match(gla_describe(bars), "^Stacked bar chart, with bars for Variable A")
  expect_match(gla_describe(bars), "showing values for 2015, 2016")
  dots <- ggplot(mtcars, aes(wt, mpg)) + geom_gla_dot()
  expect_match(gla_describe(dots), "^Scatter plot, showing values between")
  hist <- ggplot(mtcars, aes(mpg)) + geom_histogram(bins = 5)
  expect_match(gla_describe(hist), "^Histogram")
})

test_that("gla_alt sets the alt text on the plot", {
  p <- gla_alt(ggplot(gla_yearly, aes(year, average, colour = series)) + geom_gla_line())
  expect_match(get_alt_text(p), "^Line chart")
})
