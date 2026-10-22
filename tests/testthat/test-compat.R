# gglaplot's API: every old name runs, builds what the new function builds,
# and says once that it is the old name

note <- function(expr) {
  withr::local_options(list(rlib_message_verbosity = "verbose"))
  expect_message(expr, class = "gglaplot2_superseded")
}
quiet <- function(expr) suppressMessages(expr)

test_that("the old names say once what they are now", {
  note(ggla_line())
  note(ggla_highlight())
  note(ggla_labelline(y1 = 1))
  note(ggla_axisat0())
  note(ggla_highlightarea())
  note(ggla_horizbar())
  note(ggla_sf())
  note(ggla_donut())
  note(show_gla_pals())
  note(left_align_labs(ggplot()))
  note(theme_gla(gla_theme = "default"))
  note(gla_pal(palette_type = "categorical"))
})

test_that("theme_gla takes gglaplot's arguments and keeps the old shape", {
  th <- quiet(theme_gla(gla_theme = "inverse", legend = FALSE))
  expect_equal(th$plot.background$fill, unname(gla_token("chart-surface", "dark")))
  expect_equal(th$legend.position, "none")
  expect_s3_class(th$axis.title.x, "element_blank")
  expect_s3_class(th$axis.title.y, "element_blank")
  expect_s3_class(th$panel.grid.major.x, "element_blank")

  th <- quiet(theme_gla(x_axis_title = TRUE, y_axis_title = TRUE, xgridlines = TRUE))
  expect_s3_class(th$axis.title.x, "element_text")
  expect_s3_class(th$axis.title.y, "element_text")
  expect_s3_class(th$panel.grid.major.x, "element_line")
  # the theme name as the first argument, as gglaplot took it
  expect_equal(quiet(theme_gla("inverse"))$plot.background$fill,
               unname(gla_token("chart-surface", "dark")))
  expect_equal(quiet(theme_gla(legend = TRUE))$legend.position, "top")
  expect_no_error(quiet(theme_gla(y_label_length = 25, free_y_facets = TRUE)))
  expect_error(quiet(theme_gla(gla_theme = "blue")), "default")
  expect_error(theme_gla(colour = "red"), "Unused")
  # the new defaults are untouched
  expect_s3_class(theme_gla()$axis.title.x, "element_text")
})

test_that("gla_pal takes gglaplot's arguments, from the current tokens", {
  cat4 <- quiet(gla_pal(gla_theme = "default", palette_type = "categorical", n = 4))
  expect_equal(cat4, unname(gla_pal(n = 4)))
  expect_equal(length(quiet(gla_pal(palette_type = "categorical"))), 6)
  expect_warning(quiet(gla_pal(palette_type = "categorical", n = 20)), "8 categorical")
  # named colours lead, under gglaplot's names
  led <- quiet(gla_pal(palette_type = "categorical", main_colours = c("ldndkpink", "green"), n = 3))
  expect_equal(led[1:2],
               unname(gla_token(c("data-categorical-darkpink", "data-categorical-green"))))
  expect_error(quiet(gla_pal(palette_type = "categorical", main_colours = "teal")),
               "Unknown colour")
  # the theme and the type as positional arguments, as gglaplot took them
  expect_equal(quiet(gla_pal("inverse", "categorical", n = 2)),
               unname(gla_pal(n = 2, theme = "dark")))

  hi <- quiet(gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 2)))
  expect_equal(hi, c(unname(gla_token("data-primary")), rep(unname(gla_token("data-context")), 2)))
  expect_error(quiet(gla_pal(palette_type = "highlight", n = 3)), "c\\(highlighted, context\\)")

  q <- quiet(gla_pal(palette_type = "quantitative", main_colours = "blue", n = 5))
  expect_equal(length(q), 5)
  expect_equal(tolower(q[1]), unname(gla_token("palette-blue-900")))
  q0 <- quiet(gla_pal(palette_type = "quantitative", n = 5, inc0 = TRUE))
  expect_equal(tolower(q0[5]), unname(gla_token("palette-blue-100")))
  qm <- quiet(gla_pal(palette_type = "quantitative", n = 3, remove_margin = "both"))
  expect_equal(tolower(qm[1]), unname(gla_token("palette-blue-800")))
  expect_equal(tolower(qm[3]), unname(gla_token("palette-blue-300")))

  d <- quiet(gla_pal(palette_type = "diverging", main_colours = c("blue", "red"), n = 6))
  expect_equal(length(d), 6)
  expect_equal(tolower(d[1]), unname(gla_token("palette-blue-900")))
  expect_equal(tolower(d[6]), unname(gla_token("palette-red-900")))
  expect_warning(d0 <- quiet(gla_pal(palette_type = "diverging", n = 6, inc0 = TRUE)), "odd")
  expect_equal(length(d0), 7)
  expect_equal(d0[4], unname(gla_token("data-neutral-0")))
  expect_error(quiet(gla_pal(palette_type = "diverging", main_colours = "blue")), "two colours")

  brand <- quiet(gla_pal(palette_type = "categorical", palette_name = "brand", n = 5))
  expect_equal(brand, unname(gla_brand[c("pink", "cyan", "darkpink", "mustard", "red")]))
  expect_error(quiet(gla_pal(palette_type = "pie")), "palette_type")
  expect_error(gla_pal(colours = 3), "Unused")
})

test_that("the line, highlight and annotation layers build as their new forms", {
  pal <- quiet(gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1)))
  plot <- ggplot(LDNUK, aes(Year, GPG, group = location, colour = location)) +
    scale_colour_manual(values = pal) +
    quiet(ggla_line()) +
    quiet(theme_gla(y_label_length = 25))
  b <- build(plot)
  expect_equal(unique(b$data[[1]]$linewidth), 2 * 0.75 / .pt)
  expect_equal(plot$layers[[1]]$geom_params$lineend, "round")
  expect_no_error(draw(plot))

  for (ft in c("start", "end", "max", "min")) {
    p <- plot + quiet(ggla_highlight(filter_type = ft))
    expect_equal(nrow(build(p)$data[[2]]), 2, info = ft)
  }
  p <- plot + quiet(ggla_highlight(filter_type = "xy", x_filt = as.Date("2007-01-01")))
  xy <- build(p)$data[[2]]
  expect_equal(nrow(xy), 2)
  expect_true(all(xy$x == as.numeric(as.Date("2007-01-01"))))
  expect_error(quiet(ggla_highlight(filter_type = "middle")), "filter_type")
  p <- plot + quiet(ggla_highlight(aes(label = location), filter_type = "end",
                                   geom = GeomGLATextHighlight,
                                   position = position_nudge(x = -50, y = 0.5)))
  expect_equal(as.character(build(p)$data[[2]]$label), c("London", "UK"))
  expect_no_error(draw(p))

  p <- plot +
    quiet(ggla_highlightarea(xmin = as.Date("2007-01-01"), xmax = as.Date("2012-01-01"))) +
    annotate(geom = GeomGLAAnnotate, x = as.Date("2007-02-01"), y = 30.25, label = "Highlight") +
    quiet(ggla_labelline(x1 = as.Date("2000-01-01"))) +
    quiet(ggla_labelline(y1 = 20)) +
    quiet(ggla_labelline(x1 = as.Date("2000-01-01"), x2 = as.Date("2010-01-01"),
                         y1 = 20, y2 = 25)) +
    quiet(ggla_axisat0())
  b <- build(p)
  # the axis extension is a blank layer, so eight
  expect_equal(length(p$layers), 8)
  expect_equal(tolower(b$data[[2]]$fill), tolower(unname(gla_token("chart-label"))))
  expect_equal(tolower(b$data[[3]]$colour), tolower(unname(gla_token("chart-label"))))
  expect_equal(b$data[[4]]$linetype, "42")
  expect_true(b$layout$panel_params[[1]]$y.range[1] <= 0)
  expect_no_error(draw(p))
})

test_that("ggla_horizbar flips the bars and puts the gridlines on the value axis", {
  p <- ggplot(EYFSP, aes(x = Ethnicity, y = eyfsp, fill = Region)) +
    quiet(theme_gla(legend = FALSE)) +
    quiet(ggla_horizbar(to100 = TRUE)) +
    facet_grid(. ~ Region) +
    scale_fill_gla()
  expect_s3_class(p$coordinates, "CoordFlip")
  b <- build(p)
  expect_equal(length(p$layers), 2)
  expect_equal(tolower(unique(b$data[[1]]$fill)), tolower(unname(gla_token("data-empty"))))
  expect_true(all(b$data[[1]]$ymax == 100))
  expect_s3_class(p$theme$panel.grid.major.x, "element_line")
  expect_s3_class(p$theme$panel.grid.major.y, "element_blank")
  expect_no_error(draw(p))
  q <- ggplot(AvAtt, aes(x = Region, y = Score, fill = Subject)) + theme_gla() +
    quiet(ggla_horizbar()) + scale_fill_gla()
  expect_equal(length(q$layers), 1)
  expect_no_error(draw(q))
})

test_that("ggla_donut draws a labelled ring with a title in the middle", {
  p <- ggplot(Internet, aes(fill = usage)) +
    theme_gla() +
    quiet(ggla_donut(aes(label = usage, y = numUsers), stat = "identity",
                     title = "Internet usage\nin London", base_size = 10)) +
    scale_fill_gla()
  expect_s3_class(p$coordinates, "CoordPolar")
  b <- build(p)
  expect_equal(length(p$layers), 3)
  expect_equal(b$data[[2]]$label, "Internet usage\nin London")
  expect_setequal(b$data[[3]]$label, Internet$usage)
  expect_equal(p$theme$legend.position, "none")
  expect_no_error(draw(p))
  # count stat with no label mapping
  q <- ggplot(mtcars, aes(fill = factor(cyl))) + theme_gla() + quiet(ggla_donut())
  expect_equal(length(q$layers), 1)
  expect_no_error(build(q))
})

test_that("ggla_sf draws gglaplot's referendum map", {
  skip_if_not_installed("sf")
  p <- ggplot(EURef, aes(geometry = geometry, fill = CatLeave)) + quiet(ggla_sf()) + theme_gla_map()
  b <- build(p)
  expect_equal(nrow(b$data[[1]]), 33)
  expect_equal(tolower(unique(b$data[[1]]$colour)), tolower(unname(gla_token("chart-surface"))))
  expect_no_error(draw(p))
})

test_that("the rest of the old names resolve", {
  expect_s3_class(quiet(show_gla_pals("inverse")), "ggplot")
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  expect_identical(quiet(left_align_labs(p)), p)
  expect_equal(mm_to_pt, 0.75 / .pt)
  expect_identical(GeomGLALine, GeomGlaLine)
  expect_identical(GeomGLAPointHighlight, GeomGlaMarker)
  expect_identical(GeomGLATextHighlight, GeomGlaText)
  expect_identical(StatHighlight, StatGlaEnd)
  expect_identical(GeomGLASf, ggplot2::GeomSf)
  expect_true(inherits(GeomGLAAnnotate, "GeomGlaText"))
})

test_that("gglaplot's README example runs as written", {
  pal <- quiet(gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1)))
  p <- ggplot(data = LDNUK, mapping = aes(x = Year, y = GPG, group = location, colour = location)) +
    quiet(ggla_line(aes(linewidth = location))) +
    scale_linewidth_manual(values = c(4 * mm_to_pt, 2 * mm_to_pt)) +
    scale_colour_manual(values = pal) +
    quiet(ggla_highlight(filter_type = "end")) +
    quiet(ggla_axisat0()) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 32.5),
                       labels = scales::label_percent(scale = 1)) +
    scale_x_date(date_breaks = "1 year", date_labels = "'%y",
                 expand = expansion(mult = c(0.05, 0.01))) +
    labs(title = "Gender Pay Gap - Total (Median)",
         subtitle = "Gender Pay Gap - Total (Median) - London VS UK",
         caption = paste0("Note: 2017 data is provisional\n",
                          "Chart: GLA City Intelligence  Source: London Datastore")) +
    quiet(theme_gla(gla_theme = "default"))
  expect_no_error(draw(p))
})
