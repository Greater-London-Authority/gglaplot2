test_that("the swatch charts build", {
  expect_no_error(build(gla_show_pal()))
  expect_no_error(build(gla_show_pal("sequential", theme = "dark")))
  expect_no_error(build(gla_show_tokens("^data-")))
  expect_error(gla_show_tokens("^no-such-"), "No tokens match")
})
