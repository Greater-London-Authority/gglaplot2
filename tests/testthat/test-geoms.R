test_that("the house geoms build and take their colour from the theme", {
  p <- ggplot(a_series, aes(month, value)) + geom_gla_area() + geom_gla_line() +
    geom_gla_dot() + geom_gla_marker() + theme_gla()
  b <- build(p)
  primary <- tolower(unname(gla_token("data-primary")))
  expect_equal(tolower(unique(b$data[[1]]$fill)), primary)
  expect_equal(tolower(unique(b$data[[2]]$colour)), primary)
  expect_equal(tolower(unique(b$data[[3]]$colour)), primary)
  expect_equal(tolower(unique(b$data[[4]]$fill)), tolower(unname(gla_token("chart-surface"))))

  dark <- build(p + theme_gla(theme = "dark"))
  expect_equal(tolower(unique(dark$data[[2]]$colour)),
               tolower(unname(gla_token("data-primary", "dark"))))
})

test_that("a line is 2px with round ends", {
  p <- ggplot(a_series, aes(month, value)) + geom_gla_line() + theme_gla()
  b <- build(p)
  expect_equal(unique(b$data[[1]]$linewidth), 2 * 0.75 / .pt)
  expect_equal(p$layers[[1]]$geom_params$lineend, "round")
})

test_that("bars are edged in the surface colour and sit in half the slot", {
  d <- gla_yearly[gla_yearly$series == "Variable A", ]
  p <- ggplot(d, aes(factor(year), average)) + geom_gla_col() + theme_gla()
  b <- build(p)
  expect_equal(tolower(unique(b$data[[1]]$colour)), tolower(unname(gla_token("chart-surface"))))
  expect_equal(tolower(unique(b$data[[1]]$fill)), tolower(unname(gla_token("data-primary"))))
  expect_equal(unique(round(b$data[[1]]$xmax - b$data[[1]]$xmin, 6)), 0.6)
  q <- ggplot(mtcars, aes(factor(cyl))) + geom_gla_bar() + theme_gla()
  expect_no_error(build(q))
})

test_that("dodge is the house dodge: a wide group, close bars, a gap before the next", {
  d <- gla_yearly[gla_yearly$year >= 2022, ]
  p <- ggplot(d, aes(factor(year), average, fill = series)) + geom_gla_col(position = "dodge") +
    scale_fill_gla() + theme_gla()
  expect_s3_class(p$layers[[1]]$position, "PositionDodge2")
  b <- build(p)
  bars <- b$data[[1]]
  bars$slot <- round(bars$x)
  bars <- bars[order(bars$slot, bars$xmin), ]
  # three bars in each of three slots, the group spanning four fifths of it
  # less the half gap outside the end bars
  first <- bars[bars$slot == 1, ]
  expect_equal(nrow(first), 3)
  expect_equal(round(max(first$xmax) - min(first$xmin), 6), 0.8 - 0.8 / 3 * 0.15)
  # a gap between neighbours inside the group, and a bigger one to the next slot
  within <- first$xmin[-1] - first$xmax[-3]
  expect_true(all(within > 0))
  second <- bars[bars$slot == 2, ]
  expect_true(min(second$xmin) - max(first$xmax) > max(within) * 2)
  # the bars follow the series order
  expect_equal(as.character(first$fill), unname(gla_pal(n = 3)))
  # the same with the category on y
  h <- ggplot(d, aes(average, factor(year), fill = series)) + geom_gla_col(position = "dodge") +
    scale_fill_gla() + theme_gla(gridlines = "x")
  hb <- build(h)$data[[1]]
  expect_equal(sum(round(hb$y) == 1), 3)
  expect_true(all(hb$ymax - hb$ymin > 0))
  # read top to bottom, the first series comes first
  top <- hb[round(hb$y) == 1, ]
  expect_equal(as.character(top$fill[order(-top$ymin)]), unname(gla_pal(n = 3)))
  # a lone bar keeps its narrower width, and an explicit width wins
  lone <- build(ggplot(d, aes(factor(year), average)) + geom_gla_col())$data[[1]]
  expect_equal(unique(round(lone$xmax - lone$xmin, 6)), 0.6)
  wide <- build(ggplot(d, aes(factor(year), average)) + geom_gla_col(width = 0.9))$data[[1]]
  expect_equal(unique(round(wide$xmax - wide$xmin, 6)), 0.9)
  # a missing series in a category does not widen the others
  short <- d[!(d$year == 2023 & d$series == "Variable B"), ]
  sb <- build(ggplot(short, aes(factor(year), average, fill = series)) +
                geom_gla_col(position = "dodge"))$data[[1]]
  expect_equal(length(unique(round(sb$xmax - sb$xmin, 6))), 1)
})

test_that("a projected line goes dashed from the point, through it", {
  p <- ggplot(gla_yearly, aes(year, average, colour = series)) +
    geom_gla_line(projected_from = 2022) + theme_gla()
  expect_equal(p$layers[[1]]$geom_params$projected_from, 2022)
  runs <- ggplot2::layer_grob(p, 1)[[1]]
  expect_equal(length(runs), 2)
  # the first run is solid, the second dashed; both have every series
  expect_true(all(runs[[1]]$gp$lty %in% c(1, "solid")))
  expect_equal(unique(runs[[2]]$gp$lty), gla_linetypes()[["projected"]])
  expect_equal(length(unique(runs[[1]]$id)), 3)
  expect_equal(length(unique(runs[[2]]$id)), 3)
  # the point is on both runs
  x_at <- function(l) sort(unique(round(as.numeric(l$x), 4)))
  expect_equal(length(intersect(x_at(runs[[1]]), x_at(runs[[2]]))), 1)
  # a date axis takes a date
  q <- ggplot(a_series, aes(month, value)) +
    geom_gla_line(projected_from = as.Date("2023-01-01")) + theme_gla()
  expect_equal(length(ggplot2::layer_grob(q, 1)[[1]]), 2)
  expect_no_error(draw(q))
  # nothing projected draws one run
  r <- ggplot(gla_yearly, aes(year, average, colour = series)) + geom_gla_line() + theme_gla()
  expect_s3_class(ggplot2::layer_grob(r, 1)[[1]], "polyline")
})

test_that("the projection annotation is a wash, a rule and a label", {
  layers <- annotate_gla_projection(from = 2022, label = "Projected")
  expect_equal(length(layers), 3)
  p <- ggplot(gla_yearly, aes(year, average, colour = series)) + layers +
    geom_gla_line(projected_from = 2022) + theme_gla()
  b <- build(p)
  context <- tolower(unname(gla_token("data-context")))
  expect_equal(tolower(b$data[[1]]$fill), context)
  expect_equal(b$data[[1]]$xmin, 2022)
  expect_true(is.infinite(b$data[[1]]$xmax))
  expect_equal(b$data[[2]]$linetype, "42")
  expect_equal(tolower(b$data[[2]]$colour), context)
  expect_equal(b$data[[3]]$label, "Projected")
  expect_no_error(draw(p))
  expect_equal(length(annotate_gla_projection(from = 2022, label = NULL)), 2)
  dark <- annotate_gla_projection(from = 2022, theme = "dark")
  expect_equal(tolower(build(ggplot(gla_yearly, aes(year, average)) + dark)$data[[1]]$fill),
               tolower(unname(gla_token("data-context", "dark"))))
})

test_that("stat_gla_end keeps one point per series", {
  p <- ggplot(gla_yearly, aes(year, average, colour = series)) + geom_gla_line() +
    geom_gla_endpoint() + geom_gla_endlabel(aes(label = series), where = "max")
  b <- build(p)
  ends <- b$data[[2]]
  expect_equal(nrow(ends), 3)
  expect_true(all(ends$x == 2024))
  maxes <- b$data[[3]]
  expect_equal(nrow(maxes), 3)
  top <- tapply(gla_yearly$average, gla_yearly$series, max)
  expect_setequal(round(maxes$y), round(unname(top)))
  expect_setequal(maxes$label, names(top))
})

test_that("the end label draws, with and without dodging", {
  p <- ggplot(gla_yearly, aes(year, average, colour = series)) + geom_gla_line() +
    geom_gla_endlabel(aes(label = series)) + theme_gla()
  expect_no_error(draw(p))
  q <- ggplot(gla_yearly, aes(year, average, colour = series)) + geom_gla_line() +
    geom_gla_endlabel(aes(label = series), dodge = FALSE) + theme_gla()
  expect_no_error(draw(q))
})

test_that("separate_labels pushes overlaps apart and keeps the rest", {
  expect_equal(separate_labels(c(10, 50, 90), 8, 0, 100), c(10, 50, 90))
  out <- separate_labels(c(10, 12, 50), 8, 0, 100)
  expect_equal(out[3], 50)
  expect_equal(out[2] - out[1], 8)
  expect_equal(mean(out[1:2]), 11)
  # bounds hold, and the gap survives being pushed
  out <- separate_labels(c(1, 2), 8, 0, 100)
  expect_equal(out, c(0, 8))
  out <- separate_labels(c(99, 100), 8, 0, 100)
  expect_equal(out, c(92, 100))
  # order and NA preserved
  out <- separate_labels(c(12, NA, 10), 8, 0, 100)
  expect_true(is.na(out[2]))
  expect_true(out[1] > out[3])
})

test_that("the rules and ranges build", {
  p <- ggplot(gla_yearly, aes(year, average, colour = series)) +
    annotate_gla_range(xmin = 2020, xmax = 2021) +
    geom_gla_line() +
    gla_zero_line() +
    gla_reference_line(30000, "Target") +
    theme_gla()
  b <- build(p)
  expect_equal(length(p$layers), 5)
  expect_equal(tolower(unique(b$data[[3]]$colour)), tolower(unname(gla_token("chart-label"))))
  expect_equal(b$data[[4]]$linetype, "42")
  expect_equal(tolower(b$data[[4]]$colour), tolower(unname(gla_token("data-context"))))
  expect_no_error(draw(p))
  expect_no_error(ggplot(mtcars, aes(mpg, wt)) + geom_point() + gla_zero_line("x") + theme_gla())
})

test_that("geom_gla_sf draws the boroughs with the empty fill", {
  skip_if_not_installed("sf")
  p <- ggplot(gla_boroughs) + geom_gla_sf() + theme_gla_map()
  b <- build(p)
  expect_equal(tolower(unique(b$data[[1]]$fill)), tolower(unname(gla_token("data-empty"))))
  q <- ggplot(gla_boroughs) + geom_gla_sf(aes(fill = nchar(name))) + scale_fill_gla_c() +
    theme_gla_map()
  expect_no_error(build(q))
})

test_that("facet_gla_rows stacks full panels in one column", {
  f <- facet_gla_rows(vars(series))
  expect_s3_class(f, "FacetWrap")
  expect_equal(f$params$ncol, 1)
  expect_equal(f$params$free, list(x = FALSE, y = TRUE))
  expect_equal(facet_gla_rows(vars(series), scales = "fixed")$params$free,
               list(x = FALSE, y = FALSE))
  p <- ggplot(gla_monthly, aes(month, value, colour = series)) + geom_gla_line() +
    facet_gla_rows(vars(series)) + theme_gla(legend = "none")
  b <- build(p)
  expect_equal(nrow(b$layout$layout), 3)
  expect_equal(unique(b$layout$layout$COL), 1)
  # the axes are drawn on every panel
  g <- with_device(ggplotGrob(p))
  expect_equal(sum(grepl("^axis-b", g$layout$name)), 3)
  expect_no_error(draw(p))
})

test_that("geom_gla_paired draws an arrow per category between its two markers", {
  d <- gla_yearly[gla_yearly$year %in% c(2015, 2024), ]
  d$year <- factor(d$year)
  layers <- geom_gla_paired()
  expect_length(layers, 2)
  p <- ggplot(d, aes(average, series, colour = year)) + geom_gla_paired() +
    scale_colour_gla() + theme_gla()
  b <- build(p)
  arrows <- b$data[[1]]
  dots <- b$data[[2]]
  expect_equal(nrow(arrows), 3)
  expect_equal(nrow(dots), 6)
  # from the first level of the colour scale to the second
  first <- d[d$year == "2015", ]
  expect_equal(arrows$x[order(arrows$y)], first$average[order(first$series)])
  expect_equal(arrows$y, arrows$yend)
  expect_equal(layers[[1]]$geom_params$context, unname(gla_token("data-context")))
  expect_equal(unique(tolower(dots$fill)), tolower(unname(gla_token("chart-surface"))))
  # a category with one value keeps its marker and has no arrow
  d2 <- d
  d2$average[d2$series == "Variable C" & d2$year == "2015"] <- NA
  b2 <- build(ggplot(d2, aes(average, series, colour = year)) + geom_gla_paired(na.rm = TRUE) +
                scale_colour_gla())
  expect_equal(nrow(b2$data[[1]]), 2)
  expect_equal(sum(!is.na(b2$data[[2]]$x)), 5)
  # the connector stops short of the markers and ends in an arrowhead
  g <- with_device(ggplotGrob(p))
  panel <- g$grobs[[which(g$layout$name == "panel")]]
  seg <- Filter(function(x) inherits(x, "segments"), panel$children)[[1]]
  expect_false(is.null(seg$arrow))
  expect_true(grepl("points", as.character(seg$x0[1])))
  plain <- geom_gla_paired(arrow = FALSE)[[1]]
  expect_null(plain$geom_params$arrow)
  # dark mode colours the arrow from the dark tokens
  dark <- geom_gla_paired(theme = "dark")[[1]]
  expect_equal(dark$geom_params$context, unname(gla_token("data-context", "dark")))
  expect_no_error(draw(p))
})
