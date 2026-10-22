test_that("the categorical palette is the web charts' order", {
  pal <- gla_pal()
  expect_equal(unname(pal), unname(gla_token(c(
    "data-primary", "data-secondary", "data-tertiary", "data-categorical-turquoise",
    "data-categorical-purple", "data-categorical-red", "data-categorical-green",
    "data-categorical-yellow"
  ))))
  expect_equal(names(pal), c("primary", "secondary", "tertiary", "turquoise", "purple", "red",
                             "green", "yellow"))
  expect_false(anyDuplicated(pal) > 0)
  expect_equal(length(gla_pal(n = 3)), 3)
})

test_that("asking for more categorical colours than exist is an error, not a repeat", {
  expect_error(gla_pal(n = 9), "Fold the categories")
  expect_equal(gla_max_series(), 3L)
  expect_true(gla_max_series() <= length(gla_pal()))
})

test_that("the ramps interpolate to any n and keep their ends", {
  seq5 <- gla_pal("sequential")
  expect_equal(length(seq5), 5)
  seq9 <- gla_pal("sequential", 9)
  expect_equal(length(seq9), 9)
  expect_equal(tolower(seq9[1]), tolower(seq5[1]))
  expect_equal(tolower(seq9[9]), tolower(seq5[5]))

  div <- gla_pal("diverging", 7)
  expect_equal(length(div), 7)
  # red on the down side, blue on the up side, a neutral in the middle
  expect_equal(tolower(div[1]), tolower(unname(gla_token("palette-red-700"))))
  expect_equal(tolower(div[7]), tolower(unname(gla_token("palette-blue-700"))))
})

test_that("the named palettes map by name", {
  sx <- gla_pal("sex")
  expect_equal(sx[["Women"]], sx[["Female"]])
  expect_equal(sx[["Men"]], sx[["Male"]])
  expect_false(sx[["Women"]] == sx[["Men"]])
  ch <- gla_pal("change")
  expect_equal(unname(ch["up"]), unname(gla_token("data-positive")))
  expect_equal(unname(ch["down"]), unname(gla_token("data-negative")))
})

test_that("dark mode palettes read the dark tokens", {
  expect_equal(unname(gla_pal("change", theme = "dark")["up"]),
               unname(gla_token("data-positive", "dark")))
})
