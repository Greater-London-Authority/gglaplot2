test_that("the token table is complete and every value is a hex colour", {
  expect_true(nrow(gla_tokens) > 100)
  expect_equal(names(gla_tokens), c("token", "light", "dark"))
  expect_false(anyDuplicated(gla_tokens$token) > 0)
  hex <- "^#[0-9a-f]{6}([0-9a-f]{2})?$"
  expect_true(all(grepl(hex, gla_tokens$light)))
  expect_true(all(grepl(hex, gla_tokens$dark)))
  # the dashboards read these by name, so they have to exist under these names
  for (t in c("text", "text-muted", "chart-axis", "chart-grid", "chart-label",
              "chart-label-muted", "chart-surface", "data-primary", "data-secondary",
              "data-tertiary", "data-positive", "data-negative", "data-female",
              "data-male", "palette-blue-700")) {
    expect_true(t %in% gla_tokens$token, info = t)
  }
})

test_that("dark mode differs where it should and falls back where it has no value", {
  expect_false(gla_token("chart-surface") == gla_token("chart-surface", "dark"))
  expect_false(gla_token("text") == gla_token("text", "dark"))
  # the brand accent is the same in both modes
  expect_equal(gla_token("static-brand"), gla_token("static-brand", "dark"))
})

test_that("gla_token is vectorised, named, and strict about names", {
  got <- gla_token(c("chart-axis", "chart-grid"))
  expect_equal(names(got), c("chart-axis", "chart-grid"))
  expect_equal(unname(got["chart-axis"]), gla_tokens$light[gla_tokens$token == "chart-axis"])
  expect_error(gla_token("no-such-token"), "Unknown token")
})

test_that("the brand colours are the twelve statics", {
  expect_equal(length(gla_brand), 12)
  expect_true(all(c("blue", "darkblue", "green", "red", "purple", "grey") %in% names(gla_brand)))
  expect_true(all(grepl("^#[0-9a-f]{6}$", gla_brand)))
})

test_that("the token data matches the vendored source file", {
  # a refreshed token package that was not regenerated into R would drift
  # silently; this reads the Sass the way data-raw/tokens.R does
  src <- system.file("tokens", "_colors.scss", package = "gglaplot2")
  expect_true(nzchar(src))
  lines <- readLines(src)
  re <- "^\\$color-([a-z0-9-]+):\\s*(#[0-9a-fA-F]{6,8})\\s*!default;"
  m <- regmatches(lines, regexec(re, lines, perl = TRUE))
  m <- m[lengths(m) == 3]
  names_in_file <- vapply(m, `[[`, character(1), 2)
  light <- names_in_file[!grepl("-dark$", names_in_file)]
  expect_setequal(light, gla_tokens$token)
})
