test_that("the demo data is the Storybook's", {
  expect_equal(nrow(gla_monthly), 333)
  expect_equal(sort(unique(gla_monthly$series)), paste("Variable", c("A", "B", "C")))
  expect_s3_class(gla_monthly$month, "Date")
  expect_equal(range(gla_monthly$month), as.Date(c("2015-01-01", "2024-03-01")))
  expect_equal(nrow(gla_yearly), 30)
  expect_equal(range(gla_yearly$year), c(2015L, 2024L))
})

test_that("the boroughs are the 33 with their GSS codes", {
  skip_if_not_installed("sf")
  expect_equal(nrow(gla_boroughs), 33)
  expect_true(all(grepl("^E09", gla_boroughs$code)))
  expect_true("City of London" %in% gla_boroughs$name)
})

test_that("the fonts are bundled and registered", {
  skip_if_not_installed("systemfonts")
  dir <- system.file("fonts", package = "gglaplot2")
  expect_true(file.exists(file.path(dir, "Inter-SemiBold.ttf")))
  expect_true(file.exists(file.path(dir, "LICENSE.txt")))
  fam <- c(systemfonts::registry_fonts()$family, systemfonts::system_fonts()$family)
  expect_true(all(c("Inter", "Inter Medium", "Inter SemiBold") %in% fam))
  expect_equal(gla_font(), "Inter")
  expect_equal(gla_font("semibold"), "Inter SemiBold")
  expect_equal(withr::with_options(list(gglaplot2.font = "sans"), gla_font()), "")
})
