#' The GLA ggplot2 theme
#'
#' The non-data ink of a chart, from the design system's `chart-*` colour
#' tokens and its chart typography roles. The plot sits on `chart-surface`,
#' gridlines are `chart-grid`, the axis line `chart-axis`, tick labels
#' `chart-label-muted` and the title `text`. The sizes are the roles'
#' pixel sizes (title 20, subtitle 16, ticks 14, caption 12) so a chart saved
#' at 96 dpi matches a chart drawn by the visualisation team's web
#' components pixel for pixel. These are the same tokens the dashboards
#' draw their charts with, so a static chart and a live one on the same
#' page read as one system.
#'
#' Titles, subtitle and caption are left-aligned to the plot edge. One axis
#' carries gridlines (the value axis, y on most charts) and the other the
#' axis line. Four gridlines is the design intent; pass
#' `scale_y_continuous(breaks = scales::breaks_extended(4))` or
#' [gla_breaks()] to get them. The y axis has no title by default, as on the
#' web charts, where the subtitle names the unit; `axis_titles` turns it on.
#'
#' Data defaults come with the theme too: with ggplot2 4 the geoms read
#' their default colour, line width and point size from the theme, and here
#' those are the design system's marks, a 2px `data-primary` line, a 1px
#' `chart-surface` edge on a bar, 14px annotation text in `chart-label`.
#' The `geom_gla_*()` layers draw on them; so do the plain ggplot2 geoms.
#'
#' @param base_size The tick size in CSS pixels. The other roles scale from
#'   it in the design system's proportions.
#' @param base_family Font family. The design system's typeface is Inter,
#'   bundled with the package; see [gla_font()] for when it is used.
#' @param theme `"light"` or `"dark"`. Dark reads the `-dark` tokens and is
#'   for a chart on an inverse surface, not a dark-mode device setting.
#' @param gridlines Which axis carries gridlines: `"y"`, `"x"`, `"both"` or
#'   `"none"`. Use `"x"` for horizontal bars.
#' @param legend Legend position: `"top"`, `"right"`, `"bottom"` or `"none"`.
#' @param axis_titles Which axis titles to show: `"x"` (the default, as on
#'   the web charts), `"y"`, `"both"` or `"none"`.
#' @return A ggplot2 theme.
#' @examples
#' library(ggplot2)
#' ggplot(gla_monthly[gla_monthly$series == "Variable A", ], aes(month, value)) +
#'   geom_gla_line() +
#'   labs(title = "Variable A has fallen since 2017", subtitle = "Monthly value, GBP") +
#'   theme_gla()
#' @export
theme_gla <- function(base_size = 14, base_family = gla_font(), theme = c("light", "dark"),
                      gridlines = c("y", "x", "both", "none"),
                      legend = c("top", "right", "bottom", "none"),
                      axis_titles = c("x", "y", "both", "none")) {
  theme <- match.arg(theme)
  gridlines <- match.arg(gridlines)
  legend <- match.arg(legend)
  axis_titles <- match.arg(axis_titles)
  ct <- chart_tokens(theme)
  roles <- gla_type()
  # roles in points, scaled from the tick role
  pt <- function(role) px_pt(base_size * roles[[role]]$size / roles$tick$size)
  # the family for a weight: a named Inter variant when it can be drawn,
  # else the base family with a face
  fam <- function(weight) {
    if (nzchar(base_family) && grepl("^Inter", base_family)) {
      f <- gla_font(weight)
      if (nzchar(f)) return(list(family = f, face = "plain"))
    }
    list(family = base_family, face = if (weight %in% c("semibold", "bold")) "bold" else "plain")
  }
  title <- fam("semibold")
  medium <- fam("medium")
  body <- fam("regular")
  text <- function(role, colour, ...) {
    ggplot2::element_text(colour = colour, size = pt(role), ...)
  }

  grid <- ggplot2::element_line(colour = ct$grid, linewidth = px(1))
  none <- ggplot2::element_blank()
  axis_line <- ggplot2::element_line(colour = ct$axis, linewidth = px(1))
  gap <- function(x) px_pt(x)

  ggplot2::theme_minimal(base_size = px_pt(base_size), base_family = body$family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = ct$surface, colour = NA),
      panel.background = ggplot2::element_rect(fill = ct$surface, colour = NA),
      text = ggplot2::element_text(colour = ct$text, family = body$family),
      plot.title = text("title", ct$text, family = title$family, face = title$face,
                        lineheight = 1.2, margin = ggplot2::margin(b = gap(4))),
      plot.subtitle = text("subtitle", ct$text_muted, lineheight = 1.3,
                           margin = ggplot2::margin(b = gap(12))),
      plot.caption = text("caption", ct$text_muted, hjust = 0, lineheight = 1.35,
                          margin = ggplot2::margin(t = gap(12))),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      axis.text = text("tick", ct$label_muted),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = gap(6))),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = gap(8)), hjust = 1),
      axis.title = text("axis_title", ct$text_muted, family = medium$family, face = medium$face),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = gap(8))),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = gap(8)), angle = 90),
      axis.ticks = none,
      axis.ticks.length = grid::unit(0, "pt"),
      panel.grid.minor = none,
      panel.grid.major.y = if (gridlines %in% c("y", "both")) grid else none,
      panel.grid.major.x = if (gridlines %in% c("x", "both")) grid else none,
      # the axis line goes on the axis that has no gridlines, where nothing
      # else draws the baseline
      axis.line.x = if (gridlines %in% c("y", "both")) none else axis_line,
      axis.line.y = if (gridlines %in% c("x", "both")) none else axis_line,
      legend.position = legend,
      legend.title = none,
      legend.text = text("label", ct$label),
      legend.justification = if (legend %in% c("top", "bottom")) "left" else "center",
      legend.location = "plot",
      legend.key.size = grid::unit(gap(16), "pt"),
      legend.key.spacing.x = grid::unit(gap(16), "pt"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(gap(12), "pt"),
      strip.text = text("axis_title", ct$text, family = medium$family, face = medium$face,
                        hjust = 0, margin = ggplot2::margin(b = gap(8))),
      panel.spacing = grid::unit(gap(24), "pt"),
      plot.margin = ggplot2::margin(gap(16), gap(16), gap(16), gap(16)),
      geom = ggplot2::element_geom(
        ink = ct$label, paper = ct$surface, accent = ct$primary,
        linewidth = px(2), borderwidth = px(1), linetype = 1, bordertype = 1,
        family = body$family, fontsize = px_pt(roles$label$size),
        pointsize = 1.5, pointshape = 19
      )
    ) +
    do.call(ggplot2::theme, c(
      list(),
      if (!axis_titles %in% c("x", "both")) list(axis.title.x = none),
      if (!axis_titles %in% c("y", "both")) list(axis.title.y = none)
    ))
}

#' The GLA theme for a map
#'
#' [theme_gla()] with the axes, gridlines and panel chrome removed, which
#' is all a choropleth or an outline map wants. The legend sits on the
#' right.
#'
#' @inheritParams theme_gla
#' @return A ggplot2 theme.
#' @examples
#' library(ggplot2)
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   ggplot(gla_boroughs) + geom_gla_sf() + theme_gla_map()
#' }
#' @export
theme_gla_map <- function(base_size = 14, base_family = gla_font(), theme = c("light", "dark"),
                          legend = c("right", "top", "bottom", "none")) {
  theme <- match.arg(theme)
  legend <- match.arg(legend)
  theme_gla(base_size, base_family, theme, gridlines = "none", legend = legend,
            axis_titles = "none") +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}

#' Four breaks on the value axis
#'
#' The web charts ask for four ticks on the value axis, which keeps the
#' gridlines to three, four or five. This is `scales::breaks_extended(4)`
#' under a name that says why.
#'
#' @param n Breaks to aim for.
#' @return A breaks function for a continuous scale.
#' @examples
#' library(ggplot2)
#' ggplot(gla_yearly, aes(year, average, colour = series)) +
#'   geom_gla_line() +
#'   scale_y_continuous(breaks = gla_breaks()) +
#'   theme_gla()
#' @export
gla_breaks <- function(n = 4) {
  scales::breaks_extended(n = n)
}
