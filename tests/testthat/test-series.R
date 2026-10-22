test_that("London against one comparator is primary against context", {
  s <- gla_series_colours(c("England", "London"))
  expect_equal(unname(s["London"]), unname(gla_token("data-primary")))
  expect_equal(unname(s["England"]), unname(gla_token("data-context")))
  # the same whichever order the levels come in
  expect_equal(gla_series_colours(c("London", "England"))[["London"]], s[["London"]])
})

test_that("the named pairs are primary against secondary", {
  s <- gla_series_colours(c("International", "Domestic"))
  expect_equal(unname(s["Domestic"]), unname(gla_token("data-primary")))
  expect_equal(unname(s["International"]), unname(gla_token("data-secondary")))
  s <- gla_series_colours(c("Inner London", "Outer London"))
  expect_equal(unname(s["Inner London"]), unname(gla_token("data-primary")))
  expect_equal(unname(s["Outer London"]), unname(gla_token("data-secondary")))
})

test_that("anything else is the palette in order, with London first", {
  s <- gla_series_colours(c("Variable A", "Variable B", "Variable C"))
  expect_equal(unname(s), unname(gla_pal(n = 3)))
  s <- gla_series_colours(c("England", "Wales", "London"))
  expect_equal(unname(s["London"]), unname(gla_pal(n = 1)))
  expect_equal(names(s), c("England", "Wales", "London"))
})

test_that("more levels than colours is an error, not a repeat", {
  expect_equal(length(gla_series_colours(letters[1:8])), 8)
  expect_error(gla_series_colours(letters[1:9]), "Fold the categories")
})

test_that("dark mode reads the dark tokens", {
  s <- gla_series_colours(c("England", "London"), theme = "dark")
  expect_equal(unname(s["London"]), unname(gla_token("data-primary", "dark")))
})
