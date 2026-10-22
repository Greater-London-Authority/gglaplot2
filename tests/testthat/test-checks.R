# The palette checks, and the package's palettes held to them. The
# snapshots are the baseline: a token release that moves a colour changes
# the report, and the change is reviewed rather than discovered.

test_that("the colour maths agrees with the design system's validator", {
  # the validator's own figures for the categorical palette on its light
  # surface, run on 22 October 2026
  viz <- unname(gla_pal())
  r <- gla_check_palette(viz, surface = "#fcfcfb")
  expect_equal(r$result, c("fail", "fail", "warn", "pass", "warn"))
  expect_match(r$detail[1], "#460d53 \\(0.297\\)")
  expect_match(r$detail[1], "#fff200 \\(0.941\\)")
  expect_match(r$detail[2], "#57b8a8 \\(0.095\\)")
  expect_match(r$detail[3], "#eb001b and #41a362, 7.7 \\(deutan\\); tritan 19.5")
  expect_match(r$detail[4], "#9e0059 and #f36428, 26.2")
  expect_match(r$detail[5], "#57b8a8 \\(2.32\\)")
  expect_match(r$detail[5], "#fff200 \\(1.14\\)")
  # and the primitives
  expect_equal(wcag_contrast("#ffffff", "#000000"), 21)
  expect_equal(delta_e("#007acc", "#007acc"), 0)
  expect_equal(unname(oklch("#ffffff")[1]), 1, tolerance = 1e-6)
  expect_error(gla_check_palette(c("#007acc", "blue")), "hex")
})

test_that("the first three categorical colours pass every check on the light surface", {
  r <- gla_check_palette(gla_pal(n = 3))
  expect_true(attr(r, "ok"))
  expect_equal(r$result, rep("pass", 5))
  r_all <- gla_check_palette(gla_pal(n = 3), pairs = "all")
  expect_true(attr(r_all, "ok"))
})

test_that("the palettes' reports are the baseline", {
  expect_snapshot(print(gla_check_palette(gla_pal()), right = FALSE))
  expect_snapshot(print(gla_check_palette(gla_pal(theme = "dark"), theme = "dark"), right = FALSE))
  expect_snapshot(print(gla_check_palette(gla_pal(n = 3, theme = "dark"), theme = "dark"),
                        right = FALSE))
  expect_snapshot(print(gla_check_palette(gla_pal("sex")), right = FALSE))
  expect_snapshot(print(gla_check_ramp(gla_pal("sequential", 5)), right = FALSE))
  expect_snapshot(print(gla_check_ramp(gla_pal("sequential", 5, theme = "dark"), theme = "dark"),
                        right = FALSE))
  div <- gla_pal("diverging", 7)
  expect_snapshot(print(gla_check_ramp(div[1:4]), right = FALSE))
  expect_snapshot(print(gla_check_ramp(div[4:7]), right = FALSE))
})

test_that("the ramps read as ramps", {
  for (mode in c("light", "dark")) {
    r <- gla_check_ramp(gla_pal("sequential", 5, theme = mode), theme = mode)
    expect_equal(r$result[r$check == "Lightness monotone"], "pass", info = mode)
    expect_equal(r$result[r$check == "Single hue"], "pass", info = mode)
    div <- gla_pal("diverging", 7, theme = mode)
    expect_equal(gla_check_ramp(div[1:4], theme = mode)$result[1], "pass", info = mode)
    expect_equal(gla_check_ramp(div[4:7], theme = mode)$result[1], "pass", info = mode)
  }
})

test_that("the text tokens are readable on the chart surface in both modes", {
  for (mode in c("light", "dark")) {
    surface <- unname(gla_token("chart-surface", mode))
    for (tk in c("text", "text-muted", "chart-label", "chart-label-muted")) {
      expect_gte(wcag_contrast(unname(gla_token(tk, mode)), surface), 4.5,
                 label = paste(tk, "on", mode))
    }
    expect_gte(wcag_contrast(unname(gla_token("data-primary", mode)), surface), 3)
  }
})

test_that("the sex and change pairs are told apart under colour vision deficiency", {
  for (mode in c("light", "dark")) {
    sx <- unname(gla_pal("sex", theme = mode)[1:2])
    expect_gte(min(delta_e(sx[1], sx[2], "protan"), delta_e(sx[1], sx[2], "deutan")), 8)
    ch <- unname(gla_pal("change", theme = mode))
    expect_gte(min(delta_e(ch[1], ch[2], "protan"), delta_e(ch[1], ch[2], "deutan")), 8)
  }
})
