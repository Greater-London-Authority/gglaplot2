test_that("numbers take a comma every three digits and cut short on request", {
  expect_equal(label_gla_number()(c(1234, 1234567)), c("1,234", "1,234,567"))
  expect_equal(label_gla_number(accuracy = 0.1)(1234.56), "1,234.6")
  expect_equal(label_gla_number(suffix = " people")(1500), "1,500 people")
  expect_equal(label_gla_number(short = TRUE)(c(500, 45000, 3.5e6, 1.2e9)),
               c("500", "45k", "3.5m", "1.2bn"))
  expect_equal(label_gla_number()(c(10, NA)), c("10", NA))
})

test_that("percentages are whole by default, from fractions, and take points with scale = 1", {
  expect_equal(label_gla_percent()(c(0.05, 0.123)), c("5%", "12%"))
  expect_equal(label_gla_percent(accuracy = 0.1)(0.123), "12.3%")
  expect_equal(label_gla_percent(scale = 1)(c(5, 12.3)), c("5%", "12%"))
  expect_equal(label_gla_percent(accuracy = 0.1, scale = 1)(12.34), "12.3%")
})

test_that("pounds sit against the number", {
  expect_equal(label_gla_pounds()(c(50, 1234)), c("£50", "£1,234"))
  expect_equal(label_gla_pounds(accuracy = 0.01)(9.5), "£9.50")
  expect_equal(label_gla_pounds(short = TRUE)(c(45000, 1.2e9)), c("£45k", "£1.2bn"))
})

test_that("dates are written out, without a zero on the day", {
  withr::with_locale(c(LC_TIME = "C"), {
    expect_equal(label_gla_date()(as.Date(c("2024-03-05", "2024-11-21"))),
                 c("5 March 2024", "21 November 2024"))
    expect_equal(label_gla_date("%b %Y")(as.Date("2024-03-05")), "Mar 2024")
    expect_equal(label_gla_date()(as.Date(NA)), NA_character_)
  })
})

test_that("the labels work on a scale", {
  p <- ggplot(gla_yearly, aes(year, average, colour = series)) + geom_gla_line() +
    scale_y_continuous(labels = label_gla_pounds(short = TRUE)) + theme_gla()
  labels <- build(p)$layout$panel_params[[1]]$y$get_labels()
  expect_true(all(grepl("^£", labels[!is.na(labels)])))
})
