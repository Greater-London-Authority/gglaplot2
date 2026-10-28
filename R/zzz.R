.onLoad <- function(libname, pkgname) {
  S7::methods_register()
  register_element_methods()
  register_gla_fonts()
}

# the chrome elements are S7 classes named with the package, which S3
# dispatch on element_grob() needs registered under that full name
register_element_methods <- function() {
  ns <- asNamespace("ggplot2")
  registerS3method("element_grob", "gglaplot2::element_gla_title",
                   draw_gla_title, envir = ns)
  registerS3method("element_grob", "gglaplot2::element_gla_caption",
                   draw_gla_caption, envir = ns)
}

# Registers the bundled Inter files with systemfonts so ragg, svglite and
# the devices built on them can draw the design system's typeface wherever
# the package is installed. A family already known (Inter installed on the
# machine) is left alone. Nothing happens without systemfonts.
register_gla_fonts <- function() {
  if (!requireNamespace("systemfonts", quietly = TRUE)) return(invisible(FALSE))
  dir <- system.file("fonts", package = "gglaplot2")
  if (!nzchar(dir)) return(invisible(FALSE))
  f <- function(x) file.path(dir, x)
  known <- c(systemfonts::registry_fonts()$family, systemfonts::system_fonts()$family)
  if (!"Inter" %in% known) {
    systemfonts::register_font(
      "Inter", plain = f("Inter-Regular.ttf"), bold = f("Inter-Bold.ttf"),
      italic = f("Inter-Italic.ttf"), bolditalic = f("Inter-BoldItalic.ttf")
    )
  }
  if (!"Inter Medium" %in% known) {
    systemfonts::register_font("Inter Medium", plain = f("Inter-Medium.ttf"),
                               bold = f("Inter-Bold.ttf"))
  }
  if (!"Inter SemiBold" %in% known) {
    systemfonts::register_font("Inter SemiBold", plain = f("Inter-SemiBold.ttf"),
                               bold = f("Inter-Bold.ttf"))
  }
  invisible(TRUE)
}
