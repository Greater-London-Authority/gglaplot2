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

test_that("every token the package reads is in the data", {
  expect_true(all(required_tokens() %in% gla_tokens$token))
  expect_no_error(check_required_tokens(gla_tokens$token))
  expect_error(check_required_tokens(setdiff(gla_tokens$token, "data-primary")), "data-primary")
})

test_that("the token data matches the vendored package", {
  # a refreshed token package that was not regenerated into R would drift
  # silently; this reads the vendored files the way data-raw/tokens.R does
  dir <- system.file("tokens", package = "gglaplot2")
  expect_true(nzchar(dir))
  src <- read_token_files(dir)
  built <- build_token_data(src)
  expect_equal(built$gla_tokens, gla_tokens)
  expect_equal(built$gla_brand, gla_brand)
  expect_true(file.exists(file.path(dir, "LICENSE.txt")))
})

test_that("the vendored release is recorded", {
  v <- gla_token_version()
  expect_equal(v$package, "@ldn-viz/themes")
  expect_match(v$version, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
  expect_match(v$url, "registry.npmjs.org")
})

test_that("the parser reads the CSS the package publishes, resolving references", {
  dir <- withr::local_tempdir()
  writeLines(c(
    ":root {",
    "  --color-data-categorical-blue: #007acc;",
    "  --color-data-primary: var(--color-data-categorical-blue, #007acc);",
    "  --color-data-secondary: var(--color-data-categorical-blue);",
    "  --primitive-typography-font-family: Inter;",
    "}"
  ), file.path(dir, "light.css"))
  file.copy(file.path(dir, "light.css"), file.path(dir, "dark.css"))
  writeLines(c(":root {", "  --primitive-color-static-gla-blue: #007ACC;", "}"),
             file.path(dir, "primitives.css"))
  src <- read_token_files(dir)
  expect_equal(src$light[["data-primary"]], "#007acc")
  expect_equal(src$light[["data-secondary"]], "#007acc")
  expect_false("primitive-typography-font-family" %in% names(src$light))
  expect_true(is.na(src$version))
  built <- build_token_data(src)
  expect_equal(built$gla_brand, c(blue = "#007acc"))
  expect_equal(built$gla_tokens$dark, built$gla_tokens$light)
  # a reference that resolves to nothing is a broken file
  writeLines(c(":root {", "  --color-text: var(--color-missing);", "}"),
             file.path(dir, "light.css"))
  expect_error(read_token_files(dir), "resolves to no colour")
  # and no files at all says what it expected
  expect_error(read_token_files(withr::local_tempdir()), "light.css")
})

test_that("the Sass form an older checkout vendored still reads", {
  dir <- withr::local_tempdir()
  writeLines(c("$color-text: #353d42 !default;", "$color-text-dark: #ffffff !default;"),
             file.path(dir, "_colors.scss"))
  writeLines("$primitive-color-static-gla-blue: #007acc !default;",
             file.path(dir, "_primitives.scss"))
  src <- read_token_files(dir)
  expect_equal(src$light[["text"]], "#353d42")
  expect_equal(src$dark[["text"]], "#ffffff")
  expect_equal(build_token_data(src)$gla_brand, c(blue = "#007acc"))
})
