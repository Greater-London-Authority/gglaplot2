test_that("the mode is light unless set, and setting it returns the previous mode", {
  withr::local_options(list(gglaplot2.mode = NULL))
  expect_equal(gla_mode(), "light")
  expect_equal(gla_mode("dark"), "light")
  expect_equal(gla_mode(), "dark")
  expect_equal(gla_mode("light"), "dark")
  expect_error(gla_mode("sepia"), "light")
  withr::local_options(list(gglaplot2.mode = "sepia"))
  expect_error(gla_mode(), "light")
})

test_that("with_gla_mode sets the mode for the code and puts it back", {
  withr::local_options(list(gglaplot2.mode = "light"))
  got <- with_gla_mode("dark", gla_mode())
  expect_equal(got, "dark")
  expect_equal(gla_mode(), "light")
  expect_error(with_gla_mode("dark", stop("boom")), "boom")
  expect_equal(gla_mode(), "light")
})

test_that("every function that colours something follows the mode", {
  explicit <- ggplot(gla_yearly, aes(year, average, colour = series)) +
    annotate_gla_range(xmin = 2020, xmax = 2021, theme = "dark") +
    annotate_gla_projection(from = 2022, theme = "dark") +
    geom_gla_line(projected_from = 2022) +
    gla_reference_line(30000, "Target", theme = "dark") +
    scale_colour_gla(theme = "dark") +
    theme_gla(theme = "dark")
  switched <- with_gla_mode("dark", {
    ggplot(gla_yearly, aes(year, average, colour = series)) +
      annotate_gla_range(xmin = 2020, xmax = 2021) +
      annotate_gla_projection(from = 2022) +
      geom_gla_line(projected_from = 2022) +
      gla_reference_line(30000, "Target") +
      scale_colour_gla() +
      theme_gla()
  })
  a <- build(explicit)
  b <- build(switched)
  for (i in seq_along(a$data)) expect_equal(b$data[[i]], a$data[[i]], info = paste("layer", i))
  expect_equal(switched$theme$plot.background$fill, unname(gla_token("chart-surface", "dark")))

  with_gla_mode("dark", {
    expect_equal(gla_token("chart-surface"), gla_token("chart-surface", theme = "dark"))
    expect_equal(gla_pal(), gla_pal(theme = "dark"))
    expect_equal(gla_series_colours(c("England", "London")),
                 gla_series_colours(c("England", "London"), theme = "dark"))
    expect_equal(theme_gla_map()$plot.background$fill, unname(gla_token("chart-surface", "dark")))
    expect_no_error(build(gla_show_pal()))
    f <- withr::local_tempfile(fileext = ".png")
    p <- ggplot(mtcars, aes(wt, mpg)) + geom_gla_dot() + theme_gla()
    gla_save(p, f, width = 200, height = 100, scale = 1)
    expect_true(file.exists(f))
  })
  skip_if_not_installed("sf")
  m <- with_gla_mode("dark", build(ggplot(gla_boroughs) + geom_gla_sf() + theme_gla_map()))
  expect_equal(tolower(unique(m$data[[1]]$fill)), tolower(unname(gla_token("data-empty", "dark"))))
})

test_that("an explicit theme argument still wins over the mode", {
  with_gla_mode("dark", {
    expect_equal(theme_gla(theme = "light")$plot.background$fill,
                 unname(gla_token("chart-surface", "light")))
  })
})
