test_that("the house geoms build and take their colour from the theme", {
  p <- ggplot(a_series, aes(month, value)) + geom_gla_area() + geom_gla_line() +
    geom_gla_dot() + geom_gla_marker() + theme_gla()
  b <- build(p)
  primary <- tolower(unname(gla_token("data-primary")))
  expect_equal(tolower(unique(b$data[[1]]$fill)), primary)
  expect_equal(tolower(unique(b$data[[2]]$colour)), primary)
  expect_equal(tolower(unique(b$data[[3]]$colour)), primary)
  expect_equal(tolower(unique(b$data[[4]]$fill)), tolower(unname(gla_token("chart-surface"))))

  dark <- build(p + theme_gla(theme = "dark"))
  expect_equal(tolower(unique(dark$data[[2]]$colour)),
               tolower(unname(gla_token("data-primary", "dark"))))
})

test_that("a line is 2px with round ends", {
  p <- ggplot(a_series, aes(month, value)) + geom_gla_line() + theme_gla()
  b <- build(p)
  expect_equal(unique(b$data[[1]]$linewidth), 2 * 0.75 / .pt)
  expect_equal(p$layers[[1]]$geom_params$lineend, "round")
})

test_that("bars are edged in the surface colour and sit in half the slot", {
  d <- gla_yearly[gla_yearly$series == "Variable A", ]
  p <- ggplot(d, aes(factor(year), average)) + geom_gla_col() + theme_gla()
  b <- build(p)
  expect_equal(tolower(unique(b$data[[1]]$colour)), tolower(unname(gla_token("chart-surface"))))
  expect_equal(tolower(unique(b$data[[1]]$fill)), tolower(unname(gla_token("data-primary"))))
  expect_equal(unique(round(b$data[[1]]$xmax - b$data[[1]]$xmin, 6)), 0.6)
  q <- ggplot(mtcars, aes(factor(cyl))) + geom_gla_bar() + theme_gla()
  expect_no_error(build(q))
})

test_that("stat_gla_end keeps one point per series", {
  p <- ggplot(gla_yearly, aes(year, average, colour = series)) + geom_gla_line() +
    geom_gla_endpoint() + geom_gla_endlabel(aes(label = series), where = "max")
  b <- build(p)
  ends <- b$data[[2]]
  expect_equal(nrow(ends), 3)
  expect_true(all(ends$x == 2024))
  maxes <- b$data[[3]]
  expect_equal(nrow(maxes), 3)
  top <- tapply(gla_yearly$average, gla_yearly$series, max)
  expect_setequal(round(maxes$y), round(unname(top)))
  expect_setequal(maxes$label, names(top))
})

test_that("the end label draws, with and without dodging", {
  p <- ggplot(gla_yearly, aes(year, average, colour = series)) + geom_gla_line() +
    geom_gla_endlabel(aes(label = series)) + theme_gla()
  expect_no_error(draw(p))
  q <- ggplot(gla_yearly, aes(year, average, colour = series)) + geom_gla_line() +
    geom_gla_endlabel(aes(label = series), dodge = FALSE) + theme_gla()
  expect_no_error(draw(q))
})

test_that("separate_labels pushes overlaps apart and keeps the rest", {
  expect_equal(separate_labels(c(10, 50, 90), 8, 0, 100), c(10, 50, 90))
  out <- separate_labels(c(10, 12, 50), 8, 0, 100)
  expect_equal(out[3], 50)
  expect_equal(out[2] - out[1], 8)
  expect_equal(mean(out[1:2]), 11)
  # bounds hold, and the gap survives being pushed
  out <- separate_labels(c(1, 2), 8, 0, 100)
  expect_equal(out, c(0, 8))
  out <- separate_labels(c(99, 100), 8, 0, 100)
  expect_equal(out, c(92, 100))
  # order and NA preserved
  out <- separate_labels(c(12, NA, 10), 8, 0, 100)
  expect_true(is.na(out[2]))
  expect_true(out[1] > out[3])
})

test_that("the rules and ranges build", {
  p <- ggplot(gla_yearly, aes(year, average, colour = series)) +
    annotate_gla_range(xmin = 2020, xmax = 2021) +
    geom_gla_line() +
    gla_zero_line() +
    gla_reference_line(30000, "Target") +
    theme_gla()
  b <- build(p)
  expect_equal(length(p$layers), 5)
  expect_equal(tolower(unique(b$data[[3]]$colour)), tolower(unname(gla_token("chart-label"))))
  expect_equal(b$data[[4]]$linetype, "42")
  expect_equal(tolower(b$data[[4]]$colour), tolower(unname(gla_token("data-context"))))
  expect_no_error(draw(p))
  expect_no_error(ggplot(mtcars, aes(mpg, wt)) + geom_point() + gla_zero_line("x") + theme_gla())
})

test_that("geom_gla_sf draws the boroughs with the empty fill", {
  skip_if_not_installed("sf")
  p <- ggplot(gla_boroughs) + geom_gla_sf() + theme_gla_map()
  b <- build(p)
  expect_equal(tolower(unique(b$data[[1]]$fill)), tolower(unname(gla_token("data-empty"))))
  q <- ggplot(gla_boroughs) + geom_gla_sf(aes(fill = nchar(name))) + scale_fill_gla_c() +
    theme_gla_map()
  expect_no_error(build(q))
})
