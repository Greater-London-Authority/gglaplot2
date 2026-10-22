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
