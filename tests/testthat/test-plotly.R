skip_if_not_installed("plotly")

convert <- function(p) {
  warnings <- character()
  g <- withCallingHandlers(plotly::ggplotly(p), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(plot = g, warnings = warnings)
}

test_that("ggplotly draws the house marks as their base geoms", {
  p <- ggplot(gla_yearly, aes(year, average, colour = series)) +
    geom_gla_line() +
    geom_gla_endpoint() +
    geom_gla_endlabel(aes(label = series)) +
    gla_zero_line() +
    gla_reference_line(30000, "Target") +
    scale_colour_gla() +
    theme_gla()
  out <- convert(p)
  expect_false(any(grepl("yet to be implemented", out$warnings)))
  types <- vapply(out$plot$x$data, function(t) paste(t$type, if (is.null(t$mode)) "" else t$mode),
                  character(1))
  expect_true(any(grepl("scatter lines", types)))
  expect_true(any(grepl("scatter markers", types)))
  expect_true(any(grepl("scatter text", types)))

  b <- ggplot(gla_yearly, aes(factor(year), average, fill = series)) + geom_gla_col() +
    scale_fill_gla() + theme_gla()
  out <- convert(b)
  expect_false(any(grepl("yet to be implemented", out$warnings)))
  expect_true(all(vapply(out$plot$x$data, function(t) t$type, character(1)) == "bar"))

  a <- ggplot(a_series, aes(month, value)) + geom_gla_area() + geom_gla_dot() + theme_gla()
  out <- convert(a)
  expect_false(any(grepl("yet to be implemented", out$warnings)))
  expect_equal(length(out$plot$x$data), 2)
})

test_that("the plotly helpers set the surface, the type and the hover label from the tokens", {
  p <- ggplot(gla_yearly, aes(year, average, colour = series)) + geom_gla_line() +
    scale_colour_gla() + labs(title = "A title") + theme_gla()
  g <- plotly::ggplotly(p)
  s <- ggla_plotly_settings(g, gla_theme = "inverse", legend = FALSE)
  # layout() records its changes until the plot is built
  layout <- plotly::plotly_build(s)$x$layout
  expect_equal(layout$plot_bgcolor, unname(gla_token("chart-surface", "dark")))
  expect_equal(layout$paper_bgcolor, unname(gla_token("chart-surface", "dark")))
  expect_match(layout$font$family, "Inter")
  expect_equal(layout$title$font$size, gla_type()$title$size)
  expect_equal(layout$xaxis$tickfont$color, unname(gla_token("chart-label-muted", "dark")))
  expect_true(layout$yaxis$showgrid)
  expect_false(layout$xaxis$showgrid)
  expect_false(layout$showlegend)
  styled <- plotly::plotly_build(s)
  expect_false(styled$x$config$displayModeBar)
  expect_equal(styled$x$data[[1]]$hoverlabel$bgcolor, unname(gla_token("text", "dark")))

  l <- ggla_plotly_layout(g, xgridlines = TRUE, x_axis_title = TRUE,
                          annotations = list(x = 2020, y = 30000, text = "Note"),
                          xaxis = list(rangeslider = list(type = "date")))
  layout <- plotly::plotly_build(l)$x$layout
  expect_true(layout$xaxis$showgrid)
  expect_match(layout$annotations[[1]]$font$family, "Inter")
  expect_equal(layout$annotations[[1]]$text, "Note")
  expect_equal(layout$xaxis$rangeslider$type, "date")
})

test_that("ggplotly takes the stepped map, the paired dot plot and stacked panels", {
  skip_if_not_installed("sf")
  b <- gla_boroughs
  b$change <- seq(-6, 4, length.out = nrow(b))
  m <- ggplot(b) +
    geom_gla_sf(aes(fill = change)) +
    scale_fill_gla_b("diverging", steps = 3, labels = label_gla_percent(scale = 1), name = NULL) +
    theme_gla_map()
  out <- convert(m)
  expect_false(any(grepl("yet to be implemented", out$warnings)))
  expect_gt(length(out$plot$x$data), 1)

  d <- gla_yearly[gla_yearly$year %in% c(2015, 2024), ]
  d$year <- factor(d$year)
  p <- ggplot(d, aes(average, series, colour = year)) + geom_gla_paired() + scale_colour_gla() +
    theme_gla()
  out <- convert(p)
  expect_false(any(grepl("yet to be implemented", out$warnings)))
  modes <- vapply(out$plot$x$data, function(t) t$mode %||% "", character(1))
  expect_equal(sort(modes), c("lines", "markers", "markers"))
  shown <- vapply(out$plot$x$data, function(t) isTRUE(t$showlegend), logical(1))
  expect_equal(shown, modes == "markers")

  r <- ggplot(gla_monthly, aes(month, value, colour = series)) + geom_gla_line() +
    facet_gla_rows(vars(series)) + scale_colour_gla() + theme_gla(legend = "none")
  out <- convert(r)
  expect_false(any(grepl("yet to be implemented", out$warnings)))
  expect_equal(length(out$plot$x$data), 3)
})
