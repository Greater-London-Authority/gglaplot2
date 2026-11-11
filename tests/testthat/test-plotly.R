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
  # the end labels are placed by gla_plotly(); ggplotly() on its own leaves them off
  expect_false(any(grepl("scatter text", types)))

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

test_that("gla_plotly puts back the chrome and the stepped legend", {
  skip_if_not_installed("sf")
  b <- gla_boroughs
  b$change <- seq(-6, 4, length.out = nrow(b))
  m <- ggplot(b) +
    geom_gla_sf(aes(fill = change)) +
    scale_fill_gla_b("diverging", steps = 3, labels = label_gla_percent(scale = 1), name = NULL) +
    gla_labs(eyebrow = "Synthetic", title = "Change by borough", subtitle = "pp",
             source = "Demo", note = "Made up") +
    theme_gla_map()
  x <- plotly::plotly_build(gla_plotly(m))$x
  expect_match(x$layout$title$text, "Synthetic")
  expect_match(x$layout$title$text, "Change by borough")
  legend <- Filter(function(t) isTRUE(t$showlegend), x$data)
  expect_equal(vapply(legend, function(t) t$name, character(1)),
               c("4% to 6%", "2% to 4%", "0% to 2%", "-2% to 0%", "-4% to -2%", "-6% to -4%"))
  footer <- x$layout$annotations[[length(x$layout$annotations)]]$text
  expect_match(footer, "<b>Source:</b> Demo")
  expect_match(footer, "<b>Note:</b> Made up")
  # a chart with no stepped scale gets no legend traces added
  p <- ggplot(gla_yearly, aes(year, average, colour = series)) + geom_gla_line() +
    scale_colour_gla() + gla_labs(title = "Lines") + theme_gla()
  y <- plotly::plotly_build(gla_plotly(p))$x
  expect_equal(length(y$data), 3)
  expect_match(y$layout$title$text, "Lines")
  d <- data.frame(name = c("A", "B", "C"), value = c(3, 2, 1))
  t <- ggplot(d, aes(area = value, label = name)) + geom_gla_treemap() + theme_gla_map()
  out <- convert(t)
  expect_false(any(grepl("yet to be implemented", out$warnings)))
})

test_that("gla_plotly keeps what ggplotly drops from the theme and the marks", {
  d <- gla_monthly[gla_monthly$series == "Variable A", ]
  p <- ggplot(d, aes(month, value)) +
    annotate_gla_range(xmin = as.Date("2020-03-01"), xmax = as.Date("2021-06-01")) +
    geom_gla_line() +
    gla_reference_line(y = 30000, label = "Average") +
    theme_gla(legend = "none")
  x <- plotly::plotly_build(gla_plotly(p))$x
  primary <- grDevices::col2rgb(gla_token("data-primary"))
  line <- Filter(function(t) identical(t$mode, "lines") && length(t$x) > 10, x$data)[[1]]
  expect_equal(line$line$color, sprintf("rgba(%d,%d,%d,1)", primary[1], primary[2], primary[3]))
  # the shaded range reaches the panel edge, not infinity
  range <- Filter(function(t) !is.null(t$fill), x$data)[[1]]
  expect_true(all(is.finite(range$y)))
  # the reference line keeps the house dash and its label becomes an annotation
  rule <- Filter(function(t) identical(t$mode, "lines") && length(t$x) == 2, x$data)[[1]]
  expect_match(rule$line$dash, "^[0-9]+px,[0-9]+px$")
  labels <- vapply(x$layout$annotations, function(a) a$text, character(1))
  average <- x$layout$annotations[[match("Average", labels)]]
  expect_equal(average$xref, "paper")
  expect_equal(average$xanchor, "right")
  expect_false(x$layout$showlegend)
  expect_true(all(vapply(x$data, function(t) !is.null(t$type), logical(1))))
})

test_that("gla_plotly places the end labels, splits a forecast and reads the theme", {
  p <- ggplot(gla_yearly, aes(year, average, colour = series)) +
    geom_gla_line() +
    geom_gla_endlabel(aes(label = series)) +
    scale_colour_gla() +
    theme_gla(legend = "none")
  x <- plotly::plotly_build(gla_plotly(p))$x
  ends <- Filter(function(a) identical(a$xanchor, "left") && isTRUE(a$xshift > 0),
                 x$layout$annotations)
  expect_equal(sort(vapply(ends, function(a) a$text, character(1))),
               c("Variable A", "Variable B", "Variable C"))
  expect_equal(vapply(ends, function(a) a$yref, character(1)), rep("y", 3))
  d <- gla_monthly[gla_monthly$series == "Variable A", ]
  from <- as.Date("2022-12-01")
  f <- ggplot(d, aes(month, value)) +
    geom_gla_line(projected_from = from) +
    theme_gla()
  y <- plotly::plotly_build(gla_plotly(f))$x
  dashes <- vapply(y$data, function(t) t$line$dash %||% "solid", character(1))
  expect_equal(sum(grepl("px", dashes)), 1)
  expect_equal(sum(dashes == "solid"), 1)
  # a faceted chart's strips sit at the left of their panel in the typeface,
  # and a chart on the dark theme converts to a dark surface
  s <- ggplot(mtcars, aes(wt, mpg)) + geom_gla_dot() + facet_wrap(vars(cyl)) + theme_gla()
  z <- plotly::plotly_build(gla_plotly(s))$x
  strips <- Filter(function(a) a$text %in% c("4", "6", "8"), z$layout$annotations)
  expect_equal(length(strips), 3)
  expect_true(all(vapply(strips, function(a) a$xanchor, character(1)) == "left"))
  expect_true(all(grepl("Inter", vapply(strips, function(a) a$font$family, character(1)))))
  dark <- plotly::plotly_build(gla_plotly(p + theme_gla(theme = "dark", legend = "none")))$x
  expect_equal(dark$layout$paper_bgcolor, unname(gla_token("chart-surface", "dark")))
  # the axis text a theme blanks stays blank
  q <- p + theme(axis.text.y = element_blank())
  expect_false(plotly::plotly_build(gla_plotly(q))$x$layout$yaxis$showticklabels)
})

test_that("gla_plotly's hover label reads as the axes do", {
  gbp <- scales::label_dollar(prefix = "£", scale = 1e-3, suffix = "k")
  p <- ggplot(gla_monthly, aes(month, value, colour = series)) +
    geom_gla_line() +
    scale_y_continuous(labels = gbp) +
    scale_colour_gla() +
    labs(x = "Month", y = "Value") +
    theme_gla()
  x <- plotly::plotly_build(gla_plotly(p))$x
  expect_equal(x$data[[1]]$text[1], "Variable A<br>Month: 1 January 2015<br>Value: £53.62k")
  expect_equal(x$data[[1]]$hoverinfo, "text")
  b <- ggplot(gla_yearly, aes(factor(year), average, fill = series)) + geom_gla_col() +
    scale_fill_gla() + labs(x = "Year", y = "Average") + theme_gla()
  y <- plotly::plotly_build(gla_plotly(b))$x
  expect_equal(y$data[[1]]$text[1], "Variable A<br>Year: 2015<br>Average: 43,473")
})

test_that("gla_plotly names each area of a map and gives its value as the scale writes it", {
  skip_if_not_installed("sf")
  areas <- function(p, ...) {
    x <- plotly::plotly_build(gla_plotly(p, ...))$x
    Filter(function(t) !is.null(t$fill), x$data)
  }
  texts <- function(traces) unlist(lapply(traces, function(t) t$text))
  b <- gla_boroughs
  b$change <- seq(-6, 4, length.out = nrow(b))
  b$change[b$name == "Camden"] <- NA
  m <- ggplot(b) +
    geom_gla_sf(aes(fill = change)) +
    scale_fill_gla_b("diverging", steps = 3, labels = label_gla_percent(scale = 1), name = NULL) +
    theme_gla_map()
  out <- areas(m)
  # every area its own trace, hovered anywhere inside it
  expect_equal(length(out), nrow(b))
  expect_true(all(vapply(out, function(t) identical(t$hoveron, "fills"), logical(1))))
  expect_true(all(vapply(out, function(t) identical(t$hoverinfo, "text"), logical(1))))
  text <- texts(out)
  expect_false(any(grepl(plotly_mark, text, fixed = TRUE)))
  # the name from the name column, the value one figure finer than the legend
  expect_true("<b>Barking and Dagenham</b><br>-6%" %in% text)
  expect_true("<b>Barnet</b><br>-5.7%" %in% text)
  expect_true("<b>Westminster</b><br>4%" %in% text)
  expect_true("<b>Camden</b><br>No data" %in% text)
  # each name beside its own value
  expected <- paste0("<b>", b$name, "</b><br>",
                     ifelse(is.na(b$change), "No data",
                            label_gla_percent(0.1, scale = 1, drop0trailing = TRUE)(b$change)))
  expect_setequal(text, expected)

  # a label aesthetic names the areas, without a warning, and a scale name
  # heads the value
  expect_no_warning(
    coded <- ggplot(b) +
      geom_gla_sf(aes(fill = change, label = code)) +
      scale_fill_gla_b("diverging", steps = 3, name = "Change") +
      theme_gla_map()
  )
  expect_true("<b>E09000002</b><br>Change: -6" %in% texts(areas(coded)))
  expect_no_error(draw(coded))

  # ONS column names are found; a smooth scale and plain numbers
  ons <- b
  names(ons)[names(ons) == "name"] <- "LAD24NM"
  ons$count <- seq(1200, 98765.4, length.out = nrow(ons))
  smooth <- ggplot(ons) + geom_gla_sf(aes(fill = count)) + scale_fill_gla_c(name = NULL) +
    theme_gla_map()
  expect_true("<b>Westminster</b><br>98,770" %in% texts(areas(smooth)))

  # no name to be found: the value alone; nothing mapped: the name alone
  bare <- ons[, "count"]
  expect_true("1,200" %in% texts(areas(ggplot(bare) + geom_gla_sf(aes(fill = count)) +
                                         scale_fill_gla_c(name = NULL) + theme_gla_map())))
  outline <- areas(ggplot(b) + geom_gla_sf() + theme_gla_map())
  expect_true("<b>Barnet</b>" %in% texts(outline))

  # a text aesthetic of the caller's own is left alone
  own <- suppressWarnings(
    ggplot(b) + geom_gla_sf(aes(fill = change, text = paste("Mine:", name))) +
      scale_fill_gla_b("diverging", steps = 3, name = NULL) + theme_gla_map()
  )
  expect_true("Mine: Barnet" %in% texts(areas(own, tooltip = "text")))
})

test_that("the stepped legend survives the build a widget gets when it prints", {
  skip_if_not_installed("sf")
  b <- gla_boroughs
  b$change <- c(-38, seq(-3, 0.5, length.out = nrow(b) - 1))
  m <- ggplot(b) +
    geom_gla_sf(aes(fill = change)) +
    scale_fill_gla_b("diverging", steps = 3, limits = c(-3, 3),
                     labels = label_gla_percent(scale = 1), name = NULL) +
    theme_gla_map()
  # htmlwidgets builds the object once more on the way to the page
  x <- plotly::plotly_build(plotly::plotly_build(gla_plotly(m)))$x
  keys <- Filter(function(t) isTRUE(t$showlegend), x$data)
  expect_equal(vapply(keys, function(t) t$name, character(1)),
               c("2% to 3%", "1% to 2%", "0% to 1%", "-1% to 0%", "-2% to -1%", "Below -2%"))
  # an entry is only drawn for a trace with a point, here an empty one
  expect_true(all(vapply(keys, function(t) length(t$x) == 1 && is.na(t$x), logical(1))))
  expect_equal(x$layout$legend$orientation, "v")
  expect_true(x$layout$showlegend)
})

test_that("gla_plotly puts the legend where the theme has it, with its title", {
  base <- ggplot(gla_yearly, aes(year, average, colour = series)) + geom_gla_line() +
    scale_colour_gla() + labs(colour = "Series")
  legend <- function(p) plotly::plotly_build(gla_plotly(p))$x$layout$legend
  top <- legend(base + theme_gla())
  expect_equal(top[c("orientation", "x", "y", "yanchor")],
               list(orientation = "h", x = 0, y = 1, yanchor = "bottom"))
  expect_equal(top$title$text, "")
  right <- legend(base + theme_gla(legend = "right") + theme(legend.title = element_text()))
  expect_equal(right$orientation, "v")
  expect_gt(right$x, 1)
  expect_equal(right$title$text, "Series")
  # along the bottom the footer moves down to clear it
  below <- plotly::plotly_build(gla_plotly(
    base + gla_labs(source = "Demo") + theme_gla(legend = "bottom")
  ))$x$layout
  above <- plotly::plotly_build(gla_plotly(base + gla_labs(source = "Demo") + theme_gla()))$x$layout
  expect_lt(below$legend$y, 0)
  footer <- function(l) l$annotations[[length(l$annotations)]]$yshift
  expect_lt(footer(below), footer(above))
  expect_gt(below$margin$b, above$margin$b)
})
