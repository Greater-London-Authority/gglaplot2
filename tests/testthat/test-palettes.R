test_that("the categorical palette is the web charts' order", {
  pal <- gla_pal()
  expect_equal(unname(pal), unname(gla_token(c(
    "data-primary", "data-secondary", "data-tertiary", "data-categorical-turquoise",
    "data-categorical-purple", "data-categorical-red", "data-categorical-green",
    "data-categorical-yellow"
  ))))
  expect_equal(names(pal), c("primary", "secondary", "tertiary", "turquoise", "purple", "red",
                             "green", "yellow"))
  expect_false(anyDuplicated(pal) > 0)
  expect_equal(length(gla_pal(n = 3)), 3)
})

test_that("asking for more categorical colours than exist is an error, not a repeat", {
  expect_error(gla_pal(n = 9), "Fold the categories")
  expect_equal(gla_max_series(), 3L)
  expect_true(gla_max_series() <= length(gla_pal()))
})

test_that("the ramps interpolate to any n and keep their ends", {
  seq5 <- gla_pal("sequential")
  expect_equal(length(seq5), 5)
  seq9 <- gla_pal("sequential", 9)
  expect_equal(length(seq9), 9)
  expect_equal(tolower(seq9[1]), tolower(seq5[1]))
  expect_equal(tolower(seq9[9]), tolower(seq5[5]))

  div <- gla_pal("diverging", 7)
  expect_equal(length(div), 7)
  # red on the down side, blue on the up side, a neutral in the middle
  expect_equal(tolower(div[1]), tolower(unname(gla_token("palette-red-800"))))
  expect_equal(tolower(div[4]), tolower(unname(gla_token("data-neutral-0"))))
  expect_equal(tolower(div[7]), tolower(unname(gla_token("palette-blue-800"))))
})

test_that("the diverging steps are cut from one ramp, so the shorter side stays light", {
  tk <- function(x) tolower(unname(gla_token(x)))
  five <- gla_pal("diverging")
  expect_equal(length(five), 5)
  expect_equal(tolower(five[c(2, 4)]), tk(c("palette-red-400", "palette-blue-400")))
  # five a side are the tokens themselves
  ten <- gla_pal("diverging", 10)
  expect_equal(tolower(ten[1:5]), tk(paste0("palette-red-", seq(800, 400, by = -100))))
  expect_equal(tolower(ten[6:10]), tk(paste0("palette-blue-", seq(400, 800, by = 100))))
  # four down and one up: the reds of a four-step ramp, and the lightest blue of one
  lop <- gla_pal("diverging", c(4, 1))
  expect_equal(length(lop), 5)
  expect_equal(lop, gla_pal("diverging", 8)[1:5])
  # the step nearest zero is the same depth on both sides
  expect_equal(unname(oklch(lop[4])[1]), unname(oklch(lop[5])[1]), tolerance = 0.03)
  # no neutral when n is even or two-sided; one side can be empty
  neutral <- unname(gla_token("data-neutral-0"))
  expect_false(neutral %in% gla_pal("diverging", 6))
  expect_false(neutral %in% lop)
  expect_equal(gla_pal("diverging", c(0, 3)), gla_pal("diverging", 6)[4:6])
  expect_equal(gla_pal("diverging", 1), neutral)
  expect_error(gla_pal("diverging", c(1, 2, 3)), "down, up")
  expect_error(gla_pal("diverging", -1), "down, up")
  expect_error(gla_pal("diverging", 2.5), "down, up")
})

test_that("each side of the diverging palette reads as a ramp in both modes", {
  for (mode in c("light", "dark")) {
    neutral <- unname(gla_token("data-neutral-0", mode))
    for (k in 1:5) {
      pal <- gla_pal("diverging", c(k, k), theme = mode)
      down <- rev(pal[seq_len(k)])
      up <- pal[k + seq_len(k)]
      expect_true(attr(gla_check_ramp(down, theme = mode), "ok"), label = paste(mode, k, "down"))
      expect_true(attr(gla_check_ramp(up, theme = mode), "ok"), label = paste(mode, k, "up"))
      expect_gte(min(vapply(pal, delta_e, 0, neutral)), 8, label = paste(mode, k, "from neutral"))
    }
  }
})

test_that("the named palettes map by name", {
  sx <- gla_pal("sex")
  expect_equal(sx[["Women"]], sx[["Female"]])
  expect_equal(sx[["Men"]], sx[["Male"]])
  expect_false(sx[["Women"]] == sx[["Men"]])
  ch <- gla_pal("change")
  expect_equal(unname(ch["up"]), unname(gla_token("data-positive")))
  expect_equal(unname(ch["down"]), unname(gla_token("data-negative")))
})

test_that("dark mode palettes read the dark tokens", {
  expect_equal(unname(gla_pal("change", theme = "dark")["up"]),
               unname(gla_token("data-positive", "dark")))
})
