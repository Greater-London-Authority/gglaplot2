# Visual regression. Drawn through svglite with the bundled Inter, which
# svglite finds through systemfonts, so the snapshots measure the same on
# every machine. After a change that moves pixels on purpose, review with
# testthat::snapshot_review().

skip_if_not_installed("vdiffr")
skip_if_not_installed("svglite")
# the check matrix diffs the snapshots on its reference leg only; the other
# platforms draw every chart in the other test files and skip the diff
skip_if(identical(Sys.getenv("GGLAPLOT2_VISUAL"), "false"),
        "visual snapshots are compared on the reference platform only")

gbp <- scales::label_dollar(prefix = "£", scale = 1e-3, suffix = "k")

test_that("a single line chart", {
  p <- ggplot(a_series, aes(month, value)) +
    geom_gla_line() +
    gla_zero_line() +
    scale_y_continuous(breaks = gla_breaks(), labels = gbp, expand = expansion(mult = c(0, 0.1))) +
    gla_labs(title = "Variable A has fallen since 2017", subtitle = "Monthly value, GBP",
             source = "Demo data", note = "For demonstration", byline = "GLA City Intelligence",
             x = NULL) +
    theme_gla()
  vdiffr::expect_doppelganger("single line", p)
})

test_that("lines labelled at their ends, light and dark", {
  lines <- function(mode) {
    ggplot(gla_yearly, aes(year, average, colour = series)) +
      geom_gla_line() +
      geom_gla_endpoint() +
      geom_gla_endlabel(aes(label = series)) +
      gla_reference_line(30000, "Target", theme = mode) +
      annotate_gla_range(xmin = 2020, xmax = 2021, theme = mode) +
      scale_x_continuous(expand = expansion(mult = c(0.02, 0.25))) +
      scale_y_continuous(breaks = gla_breaks(), labels = gbp) +
      scale_colour_gla(theme = mode) +
      gla_labs(title = "Three series", subtitle = "Yearly average, GBP", x = NULL) +
      theme_gla(theme = mode, legend = "none")
  }
  vdiffr::expect_doppelganger("end labels light", lines("light"))
  vdiffr::expect_doppelganger("end labels dark", lines("dark"))
})

test_that("stacked and horizontal bars", {
  p <- ggplot(gla_yearly, aes(factor(year), average, fill = series)) +
    geom_gla_col() +
    gla_zero_line() +
    scale_fill_gla() +
    scale_y_continuous(breaks = gla_breaks(), labels = gbp, expand = expansion(mult = c(0, 0.05))) +
    gla_labs(title = "Stacked", subtitle = "Yearly average, GBP", x = NULL) +
    theme_gla()
  vdiffr::expect_doppelganger("stacked bars", p)
  y24 <- gla_yearly[gla_yearly$year == 2024, ]
  h <- ggplot(y24, aes(average, reorder(series, average))) +
    geom_gla_col() +
    gla_zero_line("x") +
    scale_x_continuous(breaks = gla_breaks(), labels = gbp, expand = expansion(mult = c(0, 0.08))) +
    gla_labs(title = "Horizontal", subtitle = "2024 average, GBP", x = NULL) +
    theme_gla(gridlines = "x")
  vdiffr::expect_doppelganger("horizontal bars", h)
})

test_that("a scatter with both axes titled", {
  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_gla_dot(size = 2.5) +
    gla_labs(title = "Scatter", subtitle = "Weight against fuel economy",
             x = "Weight (1,000 lb)", y = "Miles per gallon") +
    theme_gla(gridlines = "both", axis_titles = "both")
  vdiffr::expect_doppelganger("scatter", p)
})

test_that("a choropleth", {
  skip_if_not_installed("sf")
  p <- ggplot(gla_boroughs) +
    geom_gla_sf(aes(fill = nchar(name))) +
    scale_fill_gla_c(name = NULL) +
    gla_labs(title = "Choropleth", subtitle = "Letters in the name") +
    theme_gla_map()
  vdiffr::expect_doppelganger("choropleth", p)
})

test_that("the swatch charts", {
  vdiffr::expect_doppelganger("palettes", gla_show_pal())
  vdiffr::expect_doppelganger("data tokens", gla_show_tokens("^data-"))
})
