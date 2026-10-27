test_that("the auto scale colours by level name", {
  d <- data.frame(x = 1:4, y = 1:4, area = c("England", "London", "England", "London"))
  p <- ggplot(d, aes(x, y, colour = area)) + geom_point() + scale_colour_gla()
  b <- build(p)
  london <- unique(b$data[[1]]$colour[d$area == "London"])
  england <- unique(b$data[[1]]$colour[d$area == "England"])
  expect_equal(tolower(london), tolower(unname(gla_token("data-primary"))))
  expect_equal(tolower(england), tolower(unname(gla_token("data-context"))))
})

test_that("the auto scale is the categorical palette for plain names", {
  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point() + scale_colour_gla()
  b <- build(p)
  used <- unique(b$data[[1]]$colour)
  expect_setequal(tolower(used), tolower(unname(gla_pal(n = 3))))
})

test_that("the categorical scale colours series in palette order", {
  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point() +
    scale_colour_gla("categorical")
  b <- build(p)
  used <- unique(b$data[[1]]$colour)
  expect_setequal(tolower(used), tolower(unname(gla_pal(n = 3))))
})

test_that("the sex scale maps by level name whatever the order", {
  d <- data.frame(x = 1:4, y = 1:4, sex = c("Men", "Women", "Men", "Women"))
  p <- ggplot(d, aes(x, y, colour = sex)) + geom_point() + scale_colour_gla("sex")
  b <- build(p)
  got <- unique(b$data[[1]][, c("colour")])
  expect_true(tolower(gla_pal("sex")[["Women"]]) %in% tolower(got))
  expect_true(tolower(gla_pal("sex")[["Men"]]) %in% tolower(got))
})

test_that("too many levels for the palette is an error at build", {
  d <- data.frame(x = 1:9, y = 1:9, g = letters[1:9])
  p <- ggplot(d, aes(x, y, colour = g)) + geom_point() + scale_colour_gla()
  expect_error(build(p), "Fold the categories")
  q <- ggplot(d, aes(x, y, colour = g)) + geom_point() + scale_colour_gla("categorical")
  expect_error(build(q), "Fold the categories")
})

test_that("missing values take data-empty", {
  d <- data.frame(x = 1:3, y = 1:3, g = c("a", NA, "b"))
  p <- ggplot(d, aes(x, y, colour = g)) + geom_point() + scale_colour_gla()
  b <- build(p)
  expect_equal(tolower(b$data[[1]]$colour[2]), tolower(unname(gla_token("data-empty"))))
})

test_that("the continuous scales build", {
  p <- ggplot(mtcars, aes(wt, mpg, colour = hp)) + geom_point() + scale_colour_gla_c()
  expect_no_error(build(p))
  q <- ggplot(mtcars, aes(wt, mpg, fill = hp)) + geom_point(shape = 21) +
    scale_fill_gla_c("diverging")
  expect_no_error(build(q))
})

lopsided <- data.frame(x = 1:6, y = 1:6, v = c(-80, -45, -30, -5, 2, 5))
fills <- function(p) tolower(build(p)$data[[1]]$fill)
fill_scale <- function(p) build(p)$plot$scales$get_scales("fill")

test_that("the continuous diverging scale puts the neutral at the midpoint, not mid-data", {
  neutral <- tolower(unname(gla_token("data-neutral-0")))
  d <- transform(lopsided, v = c(-80, -30, 0, 2, 5, 80))
  p <- ggplot(d, aes(x, y, fill = v)) + geom_point(shape = 21) + scale_fill_gla_c("diverging")
  got <- fills(p)
  expect_equal(got[3], neutral)
  expect_equal(got[1], tolower(gla_pal("diverging")[1]))
  expect_equal(got[6], tolower(gla_pal("diverging")[5]))
  q <- ggplot(d, aes(x, y, fill = v + 100)) + geom_point(shape = 21) +
    scale_fill_gla_c("diverging", midpoint = 100)
  expect_equal(fills(q)[3], neutral)
  # the sequential ramp is untouched by midpoint
  s <- ggplot(d, aes(x, y, fill = v)) + geom_point(shape = 21) + scale_fill_gla_c()
  expect_equal(fills(s)[1], tolower(gla_pal("sequential")[1]))
})

test_that("the binned diverging scale counts bins out from zero and colours each side", {
  p <- ggplot(lopsided, aes(x, y, fill = v)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging")
  sc <- fill_scale(p)
  br <- sc$get_breaks()
  lim <- sc$get_limits()
  expect_true(0 %in% br)
  step <- diff(br)[1]
  expect_true(all(abs(diff(br) - step) < 1e-8))
  # the limits widen to whole bins
  edges <- sort(unique(c(lim, br)))
  expect_true(all(abs(edges / step - round(edges / step)) < 1e-8))
  expect_lte(lim[1], -80)
  expect_gte(lim[2], 5)
  # four bins below zero, one above: the reds of a four-step ramp and one light blue
  n_down <- sum(edges < 0)
  n_up <- sum(edges > 0)
  expect_equal(c(n_down, n_up), c(4, 1))
  pal <- tolower(gla_pal("diverging", c(4, 1)))
  bins <- cut(lopsided$v, edges, labels = FALSE, include.lowest = TRUE)
  expect_equal(fills(p), pal[bins])
  expect_equal(fills(p)[5], fills(p)[6])
  expect_no_error(draw(p))
})

test_that("the binned diverging scale takes a midpoint, limits, breaks and a guide", {
  neutral <- tolower(unname(gla_token("data-neutral-0")))
  m <- ggplot(lopsided, aes(x, y, fill = v + 100)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging", midpoint = 100)
  expect_true(100 %in% fill_scale(m)$get_breaks())
  expect_equal(fills(m), fills(ggplot(lopsided, aes(x, y, fill = v)) + geom_point(shape = 21) +
                                 scale_fill_gla_b("diverging")))
  # values past the limits take the end step rather than going missing
  q <- ggplot(lopsided, aes(x, y, fill = v)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging", limits = c(-20, 20))
  got <- fills(q)
  expect_false(anyNA(got))
  expect_equal(fill_scale(q)$get_breaks(), c(-10, 0, 10))
  expect_equal(got[1:3], rep(tolower(gla_pal("diverging", c(2, 2))[1]), 3))
  expect_equal(got[4], tolower(gla_pal("diverging", c(2, 2))[2]))
  # a bin that holds the midpoint is the neutral
  s <- ggplot(transform(lopsided, v = replace(v, 4, 0)), aes(x, y, fill = v)) +
    geom_point(shape = 21) + scale_fill_gla_b("diverging", breaks = c(-40, -1, 1))
  expect_equal(fills(s)[3:5], c(tolower(gla_pal("diverging", c(2, 1))[2]), neutral,
                                tolower(gla_pal("diverging", c(2, 1))[3])))
  # ggplot2's other arguments pass through
  g <- ggplot(lopsided, aes(x, y, fill = v)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging", n.breaks = 10, guide = "bins", name = "Change")
  expect_gt(length(fill_scale(g)$get_breaks()), 4)
  expect_no_error(draw(g))
  # dark mode reads the dark tokens
  dk <- ggplot(lopsided, aes(x, y, fill = v)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging", theme = "dark")
  expect_equal(fills(dk)[1], tolower(gla_pal("diverging", c(4, 1), theme = "dark")[1]))
  # missing values take data-empty
  na <- ggplot(transform(lopsided, v = replace(v, 2, NA)), aes(x, y, fill = v)) +
    geom_point(shape = 21) + scale_fill_gla_b("diverging")
  expect_equal(fills(na)[2], tolower(unname(gla_token("data-empty"))))
})

test_that("the binned sequential scale cuts the ramp into as many steps as bins", {
  p <- ggplot(lopsided, aes(x, y, fill = v)) + geom_point(shape = 21) + scale_fill_gla_b()
  sc <- fill_scale(p)
  edges <- sort(unique(c(sc$get_limits(), sc$get_breaks())))
  bins <- length(edges) - 1
  steps <- tolower(sc$map(edges[-1] - diff(edges) / 2))
  expect_equal(length(unique(steps)), bins)
  expect_true(all(fills(p) %in% steps))
  expect_equal(tolower(sc$map(sc$get_limits()[1])), tolower(unname(gla_token("palette-blue-200"))))
  expect_equal(tolower(sc$map(sc$get_limits()[2])), tolower(unname(gla_token("palette-blue-900"))))
  expect_no_error(draw(p))
  c2 <- ggplot(lopsided, aes(x, y, colour = v)) + geom_point() + scale_colour_gla_b()
  expect_no_error(build(c2))
})

test_that("the line styles are solid then dashed, and the scale maps them in order", {
  styles <- gla_linetypes()
  expect_equal(unname(styles[1:2]), c("solid", "22"))
  expect_equal(length(gla_linetypes(2)), 2)
  expect_error(gla_linetypes(5), "facet")
  d <- rbind(cbind(gla_yearly, kind = "Yearly average"),
             cbind(transform(gla_yearly, average = mean(average)), kind = "Mean"))
  p <- ggplot(d, aes(year, average, colour = series, linetype = kind)) + geom_gla_line() +
    scale_colour_gla() + scale_linetype_gla() + theme_gla()
  b <- build(p)
  sc <- b$plot$scales$get_scales("linetype")
  expect_equal(unname(sc$map(c("Mean", "Yearly average"))), c("solid", "22"))
  expect_setequal(unique(b$data[[1]]$linetype), c("solid", "22"))
  expect_no_error(draw(p))
})

test_that("the ramp guides stand on the right whatever the theme's legend position", {
  drawn_at <- function(p) {
    f <- tempfile(fileext = ".png")
    ragg::agg_png(f)
    on.exit({
      grDevices::dev.off()
      unlink(f)
    }, add = TRUE)
    g <- ggplot2::ggplotGrob(p)
    is_box <- grepl("^guide-box-", g$layout$name)
    drawn <- vapply(g$grobs[is_box], function(x) !inherits(x, "zeroGrob"), logical(1))
    sub("^guide-box-", "", g$layout$name[is_box][drawn])
  }
  p <- ggplot(lopsided, aes(x, y, fill = v)) + geom_point(shape = 21)
  expect_equal(drawn_at(p + scale_fill_gla_b("diverging") + theme_gla()), "right")
  expect_equal(drawn_at(p + scale_fill_gla_c() + theme_gla(legend = "bottom")), "right")
  expect_equal(drawn_at(p + scale_fill_gla_b("diverging") + theme_gla(legend = "none")),
               character())
  # a guide passed to the scale keeps the theme's position
  expect_equal(drawn_at(p + scale_fill_gla_b("diverging", guide = "bins") + theme_gla()), "top")
})
