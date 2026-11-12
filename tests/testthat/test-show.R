test_that("the swatch charts build", {
  expect_no_error(build(gla_show_pal()))
  expect_no_error(build(gla_show_pal("sequential", theme = "dark")))
  expect_no_error(build(gla_show_tokens("^data-")))
  expect_error(gla_show_tokens("^no-such-"), "No tokens match")
})

test_that("the token swatches are labelled by their own lightness in either mode", {
  labels <- function() {
    b <- build(gla_show_tokens("^data-(primary|empty)$"))$data[[2]]
    stats::setNames(tolower(b$colour), b$label)
  }
  light <- labels()
  dark <- with_gla_mode("dark", labels())
  expect_equal(dark, light)
  expect_equal(light[["#D6D8DA"]], tolower(unname(gla_token("chart-label-muted", "light"))))
})
