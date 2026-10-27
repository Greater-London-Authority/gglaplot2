bin_key <- function(p) {
  b <- build(p)
  sc <- b$plot$scales$get_scales("fill")
  GuideGlaBins$extract_key(sc, "fill", sep = " to ")
}

test_that("the bins guide draws one swatch per bin, labelled by its range, highest first", {
  d <- data.frame(x = 1:7, y = 1, v = c(-5, -3, -1, 0.5, 1.5, 2.5, -4.5))
  p <- ggplot(d, aes(x, y, fill = v)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging", steps = 3)
  key <- bin_key(p)
  expect_equal(nrow(key), 6)
  expect_equal(key$.label, c("2 to 3", "1 to 2", "0 to 1", "-2 to 0", "-4 to -2", "-6 to -4"))
  expect_equal(tolower(key$fill), tolower(gla_pal("diverging", c(3, 3))[6:1]))
  # the scale's labels shape both ends
  q <- ggplot(d, aes(x, y, fill = v)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging", steps = 3, labels = label_gla_percent(scale = 1))
  expect_equal(bin_key(q)$.label[1], "2% to 3%")
  # the sequential scale too
  s <- ggplot(d, aes(x, y, fill = v + 10)) + geom_point(shape = 21) + scale_fill_gla_b()
  ks <- bin_key(s)
  expect_gt(nrow(ks), 2)
  expect_true(all(grepl(" to ", ks$.label)))
  # the ends are the limits even when they hold clipped values
  lim <- ggplot(d, aes(x, y, fill = v)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging", steps = 2, limits = c(-4, 2))
  expect_equal(bin_key(lim)$.label[c(1, 4)], c("1 to 2", "-4 to -2"))
})

test_that("the bins guide is the stepped scales' default and draws", {
  d <- data.frame(x = 1:7, y = 1, v = c(-5, -3, -1, 0.5, 1.5, 2.5, -4.5))
  p <- ggplot(d, aes(x, y, fill = v)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging", steps = 3) + theme_gla()
  expect_s3_class(build(p)$plot$scales$get_scales("fill")$guide, "GuideGlaBins")
  expect_no_error(draw(p))
  dashed <- guide_gla_bins(sep = " \u2013 ", reverse = TRUE)
  expect_no_error(draw(p + scale_fill_gla_b("diverging", guide = dashed)))
  expect_no_error(draw(p + scale_fill_gla_b("diverging", guide = "coloursteps")))
  skip_if_not_installed("sf")
  m <- ggplot(gla_boroughs) + geom_gla_sf(aes(fill = nchar(name))) + scale_fill_gla_b() +
    theme_gla_map()
  expect_no_error(draw(m))
})
