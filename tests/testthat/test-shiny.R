skip_if_not_installed("shiny")

test_that("gla_download writes the chart under a name slugged from its title", {
  p <- ggplot(gla_yearly, aes(year, average, colour = series)) + geom_gla_line() +
    scale_colour_gla() + gla_labs(title = "London's gender pay gap has narrowed!") + theme_gla()
  expect_equal(chart_slug(p), "london-s-gender-pay-gap-has-narrowed")
  expect_equal(chart_slug(ggplot()), "chart")
  button <- gla_download_button("save")
  expect_s3_class(button, "shiny.tag")
  expect_match(as.character(button), "Download SVG")
  expect_match(as.character(button), "id=\"save\"")
  server <- function(input, output, session) {
    chart <- shiny::reactive(p)
    output$svg <- gla_download(chart)
    output$png <- gla_download(p, filename = "fixed", format = "png")
  }
  shiny::testServer(server, {
    expect_type(output$svg, "character")
    expect_type(output$png, "character")
  })
  h <- gla_download(p)
  expect_s3_class(h, "shiny.render.function")
})
