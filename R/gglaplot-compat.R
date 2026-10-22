# The gglaplot API, kept so a script written against the old package runs
# against this one. Each function maps its old arguments onto the new one
# and says so once a session. The old theme names ("default", "inverse")
# and colour names ("blue", "ldndkpink") are translated to the tokens, so
# an old chart comes out in the current style rather than the 2019 colours.

# says once a session that a gglaplot name was used, and what it is now
superseded <- function(old, new) {
  rlang::inform(
    c(sprintf("`%s` is gglaplot's name. It still works; in gglaplot2 it is `%s`.", old, new),
      i = "The Migrating from gglaplot article on the site has the full list."),
    class = "gglaplot2_superseded", .frequency = "once",
    .frequency_id = paste0("gglaplot2-superseded-", old)
  )
}

# "default"/"light" -> "light", "inverse"/"dark" -> "dark"
compat_theme <- function(gla_theme) {
  if (is.null(gla_theme)) return("light")
  switch(tolower(gla_theme),
    default = , light = "light",
    inverse = , dark = "dark",
    stop("gla_theme must be \"default\" or \"inverse\"", call. = FALSE)
  )
}

# gglaplot's colour names, under the names the tokens use
compat_colour_name <- function(x) {
  x <- tolower(x)
  aliases <- c(ldndkpink = "darkpink", dkpink = "darkpink", magenta = "darkpink",
               ldnpink = "pink", cyan = "blue", mustard = "yellow", mayoral = "blue")
  out <- ifelse(x %in% names(aliases), aliases[x], x)
  known <- c("blue", "darkpink", "green", "grey", "orange", "pink", "purple", "red",
             "turquoise", "yellow")
  bad <- out[!out %in% known]
  if (length(bad)) {
    stop("Unknown colour: ", paste(bad, collapse = ", "), ". Use one of ",
         paste(known, collapse = ", "), call. = FALSE)
  }
  unname(out)
}

# the token a colour name is drawn in on a chart
categorical_token <- function(colour) {
  ifelse(colour == "grey", "data-context", paste0("data-categorical-", colour))
}

# gla_pal() with gglaplot's arguments: palette_type, palette_name,
# main_colours, inc0, remove_margin and gla_theme
compat_gla_pal <- function(n, theme, dots) {
  allowed <- c("gla_theme", "palette_type", "palette_name", "main_colours", "inc0", "remove_margin")
  unknown <- setdiff(names(dots), allowed)
  if (length(unknown)) {
    stop("Unused arguments to gla_pal(): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  if (!is.null(dots$gla_theme)) theme <- compat_theme(dots$gla_theme)
  if (is.character(n)) {
    # gglaplot's second argument was the palette type
    dots$palette_type <- n
    n <- NULL
  }
  palette_type <- tolower(dots$palette_type %||% "categorical")
  palette_name <- tolower(dots$palette_name %||% "core")
  main <- dots$main_colours
  if (!is.null(main)) main <- compat_colour_name(main)
  inc0 <- isTRUE(dots$inc0)
  margin <- dots$remove_margin %||% "none"
  if (!palette_type %in% c("categorical", "quantitative", "highlight", "diverging")) {
    stop("palette_type must be categorical, quantitative, highlight or diverging", call. = FALSE)
  }
  if (!palette_name %in% c("core", "light", "dark", "brand")) {
    stop("palette_name must be core, light, dark or brand", call. = FALSE)
  }
  superseded(
    sprintf("gla_pal(palette_type = \"%s\")", palette_type),
    switch(palette_type, categorical = "gla_pal(\"categorical\")",
           quantitative = "gla_pal(\"sequential\")", diverging = "gla_pal(\"diverging\")",
           highlight = "scale_colour_gla() with London against a comparator")
  )
  t <- function(x) unname(gla_token(x, theme))

  if (palette_type == "categorical") {
    pal <- if (palette_name == "brand") {
      unname(gglaplot2::gla_brand[c("pink", "cyan", "darkpink", "mustard", "red")])
    } else {
      unname(gla_pal("categorical", theme = theme))
    }
    if (!is.null(main)) {
      chosen <- t(categorical_token(main))
      pal <- c(chosen, pal[!pal %in% chosen])
    }
    n <- n %||% min(6L, length(pal))
    if (n > length(pal)) {
      warning("Only ", length(pal), " categorical colours are available; ", n, " were asked for",
              call. = FALSE)
      n <- length(pal)
    }
    return(pal[seq_len(n)])
  }
  if (palette_type == "highlight") {
    n <- n %||% c(1, 5)
    if (length(n) != 2) stop("For a highlight palette n is c(highlighted, context)", call. = FALSE)
    hi <- if (is.null(main)) {
      unname(gla_pal("categorical", theme = theme))
    } else {
      t(categorical_token(main))
    }
    if (n[1] > length(hi)) {
      warning("Only ", length(hi), " highlight colours are available; ", n[1], " were asked for",
              call. = FALSE)
      n[1] <- length(hi)
    }
    return(c(hi[seq_len(n[1])], rep(t("data-context"), n[2])))
  }
  if (palette_type == "quantitative") {
    main <- (main %||% "blue")[1]
    return(compat_ramp(main, n %||% 6, inc0, margin, theme))
  }
  main <- main %||% c("blue", "darkpink")
  if (length(main) != 2) stop("A diverging palette takes two colours", call. = FALSE)
  compat_diverge(main, n %||% 6, inc0, margin, theme)
}

# the palette-<colour>-* steps, dark to light as gglaplot ran them
compat_steps <- function(colour, theme, drop_dark = FALSE, drop_light = FALSE) {
  levels <- seq(900, 200, by = -100)
  if (drop_dark) levels <- levels[-1]
  if (drop_light) levels <- levels[-length(levels)]
  unname(gla_token(paste0("palette-", colour, "-", levels), theme))
}

# a single-hue ramp of n colours, dark to light; with inc0 the last is the
# lightest step, standing for zero
compat_ramp <- function(colour, n, inc0, margin, theme) {
  steps <- compat_steps(colour, theme, drop_dark = margin %in% c("left", "both"),
                        drop_light = margin %in% c("right", "both"))
  if (inc0) {
    zero <- unname(gla_token(paste0("palette-", colour, "-100"), theme))
    c(grDevices::colorRampPalette(steps)(n - 1), zero)
  } else {
    grDevices::colorRampPalette(steps)(n)
  }
}

# two hues through the neutral midpoint; the midpoint is in the palette
# when inc0 is set, which makes n odd
compat_diverge <- function(pair, n, inc0, margin, theme) {
  want_odd <- inc0
  if ((n %% 2 == 1) != want_odd) {
    warning("For this combination of inc0 and remove_margin n must be ",
            if (want_odd) "odd" else "even", ". Returning ", n + 1, " colours instead of ", n,
            call. = FALSE)
    n <- n + 1
  }
  half <- if (inc0) (n - 1) / 2 else n / 2
  a <- compat_steps(pair[1], theme, drop_dark = margin %in% c("left", "both"))
  b <- rev(compat_steps(pair[2], theme, drop_dark = margin %in% c("right", "both")))
  c(grDevices::colorRampPalette(a)(half),
    if (inc0) unname(gla_token("data-neutral-0", theme)),
    grDevices::colorRampPalette(b)(half))
}

# theme_gla() with gglaplot's arguments. When any of them is given the
# others take gglaplot's defaults (no axis titles, no x gridlines), so a
# chart written against the old theme keeps its shape.
compat_theme_args <- function(dots, theme, legend) {
  allowed <- c("gla_theme", "x_axis_title", "y_axis_title", "xgridlines", "free_y_facets",
               "y_label_length")
  unknown <- setdiff(names(dots), allowed)
  if (length(unknown)) {
    stop("Unused arguments to theme_gla(): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  superseded("theme_gla(gla_theme = , x_axis_title = , xgridlines = )",
             "theme_gla(theme = , axis_titles = , gridlines = )")
  if (!is.null(dots$gla_theme)) theme <- compat_theme(dots$gla_theme)
  x <- isTRUE(dots$x_axis_title)
  y <- isTRUE(dots$y_axis_title)
  axis_titles <- if (x && y) "both" else if (x) "x" else if (y) "y" else "none"
  gridlines <- if (isTRUE(dots$xgridlines)) "both" else "y"
  if (is.logical(legend)) legend <- if (legend) "top" else "none"
  if (!is.null(dots$free_y_facets) || !is.null(dots$y_label_length)) {
    rlang::inform(
      "free_y_facets and y_label_length have no effect: tick labels sit beside the axis now.",
      .frequency = "once", .frequency_id = "gglaplot2-y-label-length"
    )
  }
  list(theme = theme, axis_titles = axis_titles, gridlines = gridlines, legend = legend)
}

#' gglaplot's functions
#'
#' The layers and helpers of gglaplot, kept so an old script runs here.
#' Each is the new function under the old name with the old arguments
#' translated, and each says so once a session. New code should use the
#' gglaplot2 names, which the Migrating from gglaplot article lists side by
#' side.
#'
#' @section What each one is now:
#' - `ggla_line()` is [geom_gla_line()].
#' - `ggla_highlight()` is [geom_gla_endpoint()] (`filter_type` is `where`),
#'   with `geom = GeomGLATextHighlight` it is [geom_gla_endlabel()].
#' - `ggla_labelline()` is [gla_reference_line()].
#' - `ggla_axisat0()` is [gla_zero_line()] with the axis extended to zero.
#' - `ggla_highlightarea()` is [annotate_gla_range()].
#' - `ggla_horizbar()` is [geom_gla_col()] with the category on `y` and
#'   `theme_gla(gridlines = "x")`.
#' - `ggla_sf()` is [geom_gla_sf()].
#' - `ggla_donut()` has no successor; it is kept as it was, on the tokens.
#' - `show_gla_pals()` is [gla_show_pal()].
#' - `left_align_labs()` does nothing: [theme_gla()] aligns the titles.
#' - `mm_to_pt` is the width of one CSS pixel in millimetres, the unit the
#'   design system specifies strokes in.
#' - `GeomGLALine`, `GeomGLAPointHighlight`, `GeomGLATextHighlight`,
#'   `GeomGLAAnnotate`, `GeomGLASf` and `StatHighlight` are the new ggproto
#'   objects under the old names.
#'
#' `theme_gla()` and `gla_pal()` take their old arguments directly; see
#' their pages.
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes As in
#'   [ggplot2::layer()].
#' @param ... Passed on to the new function.
#' @param geom For `ggla_highlight()`, the geom that draws the point:
#'   `GeomGLAPointHighlight` (a marker) or `GeomGLATextHighlight` (a label).
#' @param filter_type Which point: `"end"`, `"start"`, `"max"`, `"min"` or
#'   `"xy"`.
#' @param x_filt,y_filt With `filter_type = "xy"`, the x and/or y value to
#'   keep.
#' @param x1,x2,y1,y2 For `ggla_labelline()`: `y1` alone is a horizontal
#'   line, `x1` alone a vertical one, all four a segment.
#' @param gla_theme `"default"` or `"inverse"`.
#' @param xmin,xmax,ymin,ymax The extent of a highlighted area.
#' @param to100 Draw a context bar to 100 behind each bar.
#' @param title Text for the centre of a donut.
#' @param base_size Font size for a donut's text.
#' @param inc_div Ignored; the diverging palette is always shown.
#' @param plot A ggplot.
#' @return A ggplot2 layer or list of layers, except `show_gla_pals()`, a
#'   ggplot, and `left_align_labs()`, its input.
#' @name gglaplot-compat
NULL

#' @rdname gglaplot-compat
#' @export
ggla_line <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  superseded("ggla_line()", "geom_gla_line()")
  geom_gla_line(mapping = mapping, data = data, stat = stat, position = position, ...,
                na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes)
}

#' @rdname gglaplot-compat
#' @export
ggla_highlight <- function(mapping = NULL, data = NULL, geom = GeomGLAPointHighlight,
                           stat = "identity", position = "identity", ...,
                           filter_type = "end", x_filt = NULL, y_filt = NULL,
                           na.rm = FALSE, show.legend = FALSE, inherit.aes = TRUE) {
  superseded("ggla_highlight()", "geom_gla_endpoint() or geom_gla_endlabel()")
  if (!filter_type %in% c("end", "start", "max", "min", "xy")) {
    stop("filter_type must be end, start, max, min or xy", call. = FALSE)
  }
  ggplot2::layer(
    data = data, mapping = mapping, stat = StatGlaEnd, geom = geom, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(where = filter_type, x_filt = x_filt, y_filt = y_filt, na.rm = na.rm, ...)
  )
}

#' @rdname gglaplot-compat
#' @export
ggla_labelline <- function(x1 = NULL, x2 = NULL, y1 = NULL, y2 = NULL, gla_theme = "default") {
  superseded("ggla_labelline()", "gla_reference_line()")
  theme <- compat_theme(gla_theme)
  if (is.null(x1) && is.null(x2) && is.null(y2)) {
    return(gla_reference_line(y = y1, theme = theme))
  }
  if (is.null(y1) && is.null(y2) && is.null(x2)) {
    return(gla_reference_line(x = x1, theme = theme))
  }
  ggplot2::annotate("segment", x = x1, xend = x2, y = y1, yend = y2,
                    colour = unname(gla_token("data-context", theme)),
                    linewidth = px(2), linetype = "42")
}

#' @rdname gglaplot-compat
#' @export
ggla_axisat0 <- function(gla_theme = "default") {
  superseded("ggla_axisat0()", "gla_zero_line()")
  compat_theme(gla_theme)
  list(ggplot2::expand_limits(y = 0), gla_zero_line("y"))
}

#' @rdname gglaplot-compat
#' @export
ggla_highlightarea <- function(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
                               gla_theme = "default") {
  superseded("ggla_highlightarea()", "annotate_gla_range()")
  annotate_gla_range(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                     theme = compat_theme(gla_theme))
}

#' @rdname gglaplot-compat
#' @export
ggla_horizbar <- function(..., stat = "identity", position = "stack", to100 = FALSE,
                          gla_theme = "default") {
  superseded("ggla_horizbar()", "geom_gla_col() with the category on y")
  theme <- compat_theme(gla_theme)
  th <- theme_gla(theme = theme, gridlines = "x")
  c(
    if (isTRUE(to100)) {
      list(ggplot2::geom_col(ggplot2::aes(y = 100), width = 0.6,
                             fill = unname(gla_token("data-empty", theme))))
    },
    list(
      geom_gla_bar(..., stat = stat, position = position),
      ggplot2::coord_flip(),
      # after the flip the theme's x is the value axis across the page
      ggplot2::theme(
        panel.grid.major.x = th$panel.grid.major.x, panel.grid.major.y = th$panel.grid.major.y,
        axis.line.x = th$axis.line.x, axis.line.y = th$axis.line.y,
        axis.text.y = ggplot2::element_text(hjust = 1)
      )
    )
  )
}

#' @rdname gglaplot-compat
#' @export
ggla_sf <- function(mapping = ggplot2::aes(), data = NULL, stat = "sf", position = "identity",
                    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  superseded("ggla_sf()", "geom_gla_sf()")
  geom_gla_sf(mapping = mapping, data = data, ..., na.rm = na.rm, show.legend = show.legend,
              inherit.aes = inherit.aes)
}

#' @rdname gglaplot-compat
#' @export
ggla_donut <- function(mapping = NULL, data = NULL, stat = "count", title = NULL,
                       gla_theme = "default", base_size = 14, ...) {
  superseded("ggla_donut()", "ggla_donut() (it has no new name)")
  theme <- compat_theme(gla_theme)
  mapping <- mapping %||% ggplot2::aes()
  bar_mapping <- utils::modifyList(ggplot2::aes(x = 2), mapping)
  bar_mapping$label <- NULL
  label_mapping <- utils::modifyList(ggplot2::aes(x = 3), mapping)
  c(
    list(
      geom_gla_bar(mapping = bar_mapping, data = data, stat = stat, position = "stack",
                   width = 0.9, ...),
      ggplot2::coord_polar(theta = "y", direction = -1),
      ggplot2::xlim(0, 3.25),
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
        legend.position = "none"
      )
    ),
    if (!is.null(title)) {
      list(ggplot2::annotate(
        "text", x = 0, y = 0, label = title, colour = unname(gla_token("text", theme)),
        family = gla_font("semibold"), size = px_pt(base_size) / ggplot2::.pt, lineheight = 1.2
      ))
    },
    if ("label" %in% names(label_mapping)) {
      list(geom_gla_text(mapping = label_mapping, data = data, stat = stat,
                         position = ggplot2::position_stack(vjust = 0.5),
                         size = px_pt(base_size) / ggplot2::.pt))
    }
  )
}

#' @rdname gglaplot-compat
#' @export
show_gla_pals <- function(gla_theme = "default", inc_div = FALSE) {
  superseded("show_gla_pals()", "gla_show_pal()")
  gla_show_pal(theme = compat_theme(gla_theme))
}

#' @rdname gglaplot-compat
#' @export
left_align_labs <- function(plot) {
  superseded("left_align_labs()", "theme_gla(), which aligns the titles to the plot edge")
  plot
}

#' @rdname gglaplot-compat
#' @format NULL
#' @usage NULL
#' @export
mm_to_pt <- 0.75 / ggplot2::.pt

#' @rdname gglaplot-compat
#' @format NULL
#' @usage NULL
#' @export
GeomGLALine <- GeomGlaLine

#' @rdname gglaplot-compat
#' @format NULL
#' @usage NULL
#' @export
GeomGLAPointHighlight <- GeomGlaMarker

#' @rdname gglaplot-compat
#' @format NULL
#' @usage NULL
#' @export
GeomGLATextHighlight <- GeomGlaText

#' @rdname gglaplot-compat
#' @format NULL
#' @usage NULL
#' @export
GeomGLAAnnotate <- ggplot2::ggproto(
  "GeomGLAAnnotate", GeomGlaText,
  default_aes = ggplot2::aes(
    colour = ggplot2::from_theme(ink), family = ggplot2::from_theme(family),
    size = ggplot2::from_theme(fontsize), angle = 0, hjust = 0, vjust = 0,
    alpha = NA, fontface = 1, lineheight = 1.2
  )
)

#' @rdname gglaplot-compat
#' @format NULL
#' @usage NULL
#' @export
GeomGLASf <- ggplot2::GeomSf

#' @rdname gglaplot-compat
#' @format NULL
#' @usage NULL
#' @export
StatHighlight <- StatGlaEnd
