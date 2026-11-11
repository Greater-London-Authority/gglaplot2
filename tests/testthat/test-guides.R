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
})

test_that("an end bin holding values beyond the limits says so", {
  d <- data.frame(x = 1:7, y = 1, v = c(-5, -3, -1, 0.5, 1.5, 2.5, -4.5))
  lim <- ggplot(d, aes(x, y, fill = v)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging", steps = 2, limits = c(-4, 2))
  expect_equal(bin_key(lim)$.label, c("Above 1", "0 to 1", "-2 to 0", "Below -2"))
  # only the side the data leaves
  low <- ggplot(d, aes(x, y, fill = v)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging", steps = 2, limits = c(-4, 4))
  expect_equal(bin_key(low)$.label[c(1, 4)], c("2 to 4", "Below -2"))
  # limits that hold the data leave the ranges alone
  wide <- ggplot(d, aes(x, y, fill = v)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging", steps = 2, limits = c(-6, 4))
  expect_true(all(grepl(" to ", bin_key(wide)$.label)))
})

test_that("range labels take the decimals the steps need", {
  # one far fall beside small rises: the rises get steps of 0.2
  d <- data.frame(x = 1:5, y = 1, v = c(-38, -3, -1, 0.2, 0.5))
  p <- ggplot(d, aes(x, y, fill = v)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging", steps = c(2, 3), labels = label_gla_percent(scale = 1))
  expect_equal(bin_key(p)$.label,
               c("0.4% to 0.6%", "0.2% to 0.4%", "0% to 0.2%", "-20% to 0%", "-40% to -20%"))
  # limits cut in three sit on no decimal: one figure beyond the step
  q <- ggplot(d, aes(x, y, fill = v)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging", steps = 3, limits = c(-5, 5),
                     labels = label_gla_percent(scale = 1))
  expect_equal(bin_key(q)$.label[1:3], c("3.3% to 5%", "1.7% to 3.3%", "0% to 1.7%"))
  # fractions, and pounds at a fixed accuracy
  f <- ggplot(d, aes(x, y, fill = v / 1000)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging", steps = c(2, 2), limits = c(-0.05, 0.005),
                     labels = label_gla_percent())
  expect_equal(bin_key(f)$.label[1:2], c("0.25% to 0.5%", "0% to 0.25%"))
  # a label function from elsewhere is used as given
  s <- ggplot(d, aes(x, y, fill = v)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging", steps = c(2, 3), labels = scales::label_percent(1, scale = 1))
  expect_equal(bin_key(s)$.label[3], "0% to 0%")
  expect_equal(bin_accuracy(c(0, 2.5, 5), 1), 0.1)
  expect_equal(bin_accuracy(c(0, 2, 4), 1), 1)
  expect_equal(bin_accuracy(c(0, 0.025, 0.05), 1), 0.001)
})

test_that("a stepped scale with no labels marks its thousands", {
  d <- data.frame(x = 1:3, y = 1, v = c(1200, 24000, 61000))
  p <- ggplot(d, aes(x, y, fill = v)) + geom_point(shape = 21) + scale_fill_gla_b(steps = 4)
  expect_equal(bin_key(p)$.label[4], "0 to 20,000")
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

test_that("the end labels follow what the scale does with values beyond its limits", {
  d <- data.frame(x = 1:7, y = 1, v = c(-5, -3, -1, 0.5, 1.5, 2.5, -4.5))
  # censored values are drawn as missing, so the end bins keep their ranges
  cens <- ggplot(d, aes(x, y, fill = v)) + geom_point(shape = 21) +
    scale_fill_gla_b("diverging", steps = 2, limits = c(-4, 2), oob = scales::censor)
  expect_equal(bin_key(cens)$.label[c(1, 4)], c("1 to 2", "-4 to -2"))
  # a reversed scale: the bin of the lowest values is the one that reads Below
  rev <- ggplot(d, aes(x, y, fill = v + 10)) + geom_point(shape = 21) +
    scale_fill_gla_b(transform = "reverse", limits = c(11, 7), breaks = c(8, 9, 10))
  labels <- bin_key(rev)$.label
  expect_true("Below 8" %in% labels)
  expect_true("Above 10" %in% labels)
  # a scale with nothing in it has no finite edges: the labels are the
  # scale's own, with no error from working out an accuracy. Asked of the
  # scale directly, since whether such a chart draws is ggplot2's and R's.
  empty <- scale_fill_gla_b(labels = label_gla_percent())
  expect_equal(bin_edge_labels(empty, c(-Inf, Inf)), c("-Inf", "Inf"))
  expect_equal(bin_edge_labels(empty, c(2, 2)), c("200%", "200%"))
})
