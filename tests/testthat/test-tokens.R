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

test_that("a name from before themes 10 reads the token that holds it now, and says so", {
  withr::local_options(list(rlib_message_verbosity = "verbose"))
  expect_message(got <- gla_token(c("container-level-1", "text")), "surface-muted")
  expect_equal(names(got), c("container-level-1", "text"))
  expect_equal(unname(got[1]), unname(gla_token("surface-muted")))
  expect_message(got <- gla_token("chart-inverse-axis", "dark"), "inverse-chart-axis")
  expect_equal(unname(got), unname(gla_token("inverse-chart-axis", "dark")))
  expect_no_message(gla_token("surface-muted"))
  # every current name the renames point at is in the data
  expect_true(all(renamed_tokens() %in% gla_tokens$token))
  expect_false(any(names(renamed_tokens()) %in% gla_tokens$token))
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

test_that("the type roles are the product set, and the chart roles read from them", {
  expect_equal(names(gla_type_roles), c("role", "size", "weight", "line_height", "letter_spacing"))
  expect_true(all(required_type_roles() %in% gla_type_roles$role))
  expect_false(anyDuplicated(gla_type_roles$role) > 0)
  expect_true(all(gla_type_roles$size > 0 & gla_type_roles$line_height > 1))
  expect_true(all(gla_type_roles$weight %in% c(400, 500, 600, 700)))
  expect_no_error(check_required_roles(gla_type_roles$role))
  expect_error(check_required_roles(setdiff(gla_type_roles$role, "label-md")), "label-md")

  roles <- gla_type()
  expect_equal(names(roles), c("title", "subtitle", "eyebrow", "axis_title", "label", "tick",
                               "tick_sm", "caption"))
  # the stylesheet's chart aliases: title-md, subtitle-sm, label-md for the
  # labels, caption-md for the ticks, caption-sm for the footer
  expect_equal(roles$title$role, "title-md")
  expect_equal(roles$label$role, "label-md")
  expect_equal(roles$caption$role, "caption-sm")
  title <- gla_type_roles[gla_type_roles$role == "title-md", ]
  expect_equal(roles$title$size, title$size)
  expect_equal(roles$title$line_height, title$line_height)
  expect_equal(roles$title$weight, "semibold")
  expect_equal(roles$label$weight, "medium")
  expect_equal(roles$tick$weight, "regular")
  expect_true(roles$tick_sm$size < roles$tick$size)
  expect_equal(weight_name(700), "bold")
  expect_equal(weight_name(300), "regular")
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
  expect_equal(built$gla_type_roles, gla_type_roles)
  expect_true(file.exists(file.path(dir, "LICENSE.txt")))
  expect_true(file.exists(file.path(dir, "tokens.scss")))
})

test_that("the vendored release is recorded", {
  v <- gla_token_version()
  expect_equal(v$package, "@ldn-viz/themes")
  expect_match(v$version, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
  expect_match(v$url, "registry.npmjs.org")
})

test_that("the parser reads the CSS the package publishes, resolving references", {
  dir <- withr::local_tempdir()
  css <- file.path(dir, "tokens.css")
  writeLines(c(
    ":root {",
    "  --primitive-color-static-gla-blue: #007ACC;",
    "  --primitive-typography-font-family-default: Inter, sans-serif;",
    "  --color-data-categorical-blue: #007acc;",
    "  --color-data-primary: var(--color-data-categorical-blue, #007acc);",
    "  --color-data-secondary: var(--color-data-categorical-blue);",
    "  --color-text: #353d42;",
    "  --shadow-sm: 0 1px 2px 0 #0000000d;",
    "  --elevation-1: var(--shadow-sm, 0 1px 2px 0 #0000000d);",
    "  --typography-base-product-title-md-font-size: var(--primitive-typography-scale-2, 1.25rem);",
    "  --typography-md-product-title-md-font-family: Inter, sans-serif;",
    "  --typography-md-product-title-md-font-size: var(--primitive-typography-scale-2, 1.25rem);",
    "  --typography-md-product-title-md-line-height: 1.4;",
    "  --typography-md-product-title-md-letter-spacing: 0;",
    paste0("  --typography-md-product-title-md-font-weight: ",
           "var(--primitive-typography-font-weight-semi-bold, 600);"),
    "  --typography-md-product-caption-sm-font-size: 12px;",
    "  --typography-md-product-caption-sm-line-height: 1.3333;",
    "  --typography-md-product-caption-sm-letter-spacing: 0;",
    "  --typography-md-product-caption-sm-font-weight: 400;",
    "  --typography-md-product-label-sm-font-size: 0.75rem;",
    "}",
    "",
    ":root.dark, .dark {",
    "  --color-text: #ffffff;",
    "}"
  ), css)
  src <- read_token_files(dir)
  expect_equal(src$light[["data-primary"]], "#007acc")
  expect_equal(src$light[["data-secondary"]], "#007acc")
  expect_equal(src$dark, c(text = "#ffffff"))
  expect_equal(src$primitives, c("primitive-color-static-gla-blue" = "#007acc"))
  expect_false(any(grepl("font-family|shadow|elevation|typography", names(src$light))))
  expect_true(is.na(src$version))
  # the md product roles: rem at 16px, px as given, the weight through its
  # fallback, and a role with a property missing is left out
  expect_equal(src$type$role, c("title-md", "caption-sm"))
  expect_equal(src$type$size, c(20, 12))
  expect_equal(src$type$weight, c(600, 400))
  expect_equal(src$type$line_height, c(1.4, 1.3333))
  built <- build_token_data(src)
  expect_equal(built$gla_brand, c(blue = "#007acc"))
  expect_equal(built$gla_type_roles$role, c("caption-sm", "title-md"))
  # a token with no dark value keeps its light one
  tokens <- built$gla_tokens
  expect_equal(tokens$dark[tokens$token == "text"], "#ffffff")
  expect_equal(tokens$dark[tokens$token == "data-primary"], "#007acc")
  # a colour token that resolves to nothing is a broken file
  writeLines(c(":root {", "  --primitive-color-static-gla-blue: #007acc;",
               "  --color-text: var(--color-missing);", "}",
               ":root.dark, .dark {", "  --color-text: #ffffff;", "}"), css)
  expect_error(read_token_files(dir), "resolve to no colour")
  # a file without the dark block, or no file at all, says what it expected
  writeLines(c(":root {", "  --color-text: #353d42;", "}"), css)
  expect_error(read_token_files(dir), "root\\\\.dark")
  expect_error(read_token_files(withr::local_tempdir()), "tokens.css")
  # a file with the colours but no type roles
  writeLines(c(":root {", "  --primitive-color-static-gla-blue: #007acc;",
               "  --color-text: #353d42;", "}",
               ":root.dark, .dark {", "  --color-text: #ffffff;", "}"), css)
  expect_error(read_token_files(dir), "No type tokens")
})
