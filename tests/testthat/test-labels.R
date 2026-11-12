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
  # the eyebrow rides on the title
  e <- gla_labs(eyebrow = "London", title = "T")
  expect_equal(as.character(e$title), "T")
  expect_equal(attr(e$title, "eyebrow"), "London")
  expect_equal(as.character(gla_labs(eyebrow = "London")$title), "")
  expect_null(attr(gla_labs(title = "T")$title, "eyebrow"))
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

test_that("gla_describe names grouped bars, one series, treemaps and paired dots rightly", {
  grouped <- ggplot(gla_yearly, aes(factor(year), average, fill = series)) +
    geom_gla_col(position = "dodge")
  expect_match(gla_describe(grouped), "^Grouped bar chart, with bars for Variable A")
  one <- ggplot(a_series, aes(month, value, colour = series)) + geom_gla_dot()
  expect_match(gla_describe(one), "with a single series for Variable A")
  rev <- ggplot(mtcars, aes(wt, mpg)) + geom_gla_dot() + scale_x_reverse()
  expect_match(gla_describe(rev), "between 1.513 and 5.424")
  logged <- ggplot(mtcars, aes(disp, mpg)) + geom_gla_dot() + scale_x_log10()
  expect_match(gla_describe(logged), "between 71.1 and 472")
  tm <- data.frame(b = c("Camden", "Barnet"), g = c("Inner", "Outer"), v = c(2, 3))
  expect_equal(gla_describe(ggplot(tm, aes(area = v, label = b, fill = g)) + geom_gla_treemap()),
               "Treemap, with series for Inner and Outer.")
  d <- gla_yearly[gla_yearly$year %in% c(2015, 2024), ]
  d$year <- factor(d$year)
  expect_match(gla_describe(ggplot(d, aes(average, series, colour = year)) + geom_gla_paired()),
               "^Paired dot plot")
  expect_equal(gla_describe(ggplot(a_series[0, ], aes(month, value)) + geom_gla_line()),
               "Line chart.")
})
