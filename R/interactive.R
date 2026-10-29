#' An interactive chart from a ggplot, with the chrome plotly drops
#'
#' [plotly::ggplotly()] converts a chart drawn here, but leaves three things
#' behind: the eyebrow above the title, the `Source:` and `Note:` lines of
#' the footer, and the legend of a stepped scale, which it draws as
#' polygons with no key at all. `gla_plotly()` converts the chart and puts
#' them back: the eyebrow and title as the plot title, the footer as an
#' annotation under the plot with its labels in bold, and the stepped
#' scale's bins as a legend, highest bin first, labelled with the ranges
#' [guide_gla_bins()] shows. The surface, type and hover label come from
#' the tokens as [ggla_plotly_settings()] sets them.
#'
#' Interactive charts on a GLA site belong to the visualisation team's web
#' components; this is for a notebook, a Shiny app or a one-off.
#'
#' @param plot A ggplot.
#' @param theme `"light"` or `"dark"`. By default the chart's own theme
#'   decides: a chart drawn with `theme_gla(theme = "dark")` converts to a
#'   dark surface.
#' @param tooltip Passed to [plotly::ggplotly()].
#' @param ... Passed to [plotly::ggplotly()].
#' @return A plotly object.
#' @examples
#' if (requireNamespace("plotly", quietly = TRUE) && requireNamespace("sf", quietly = TRUE)) {
#'   library(ggplot2)
#'   b <- gla_boroughs
#'   b$change <- seq(-6, 4, length.out = nrow(b))
#'   p <- ggplot(b) +
#'     geom_gla_sf(aes(fill = change)) +
#'     scale_fill_gla_b("diverging", steps = 3, labels = label_gla_percent(scale = 1),
#'                      name = NULL) +
#'     gla_labs(eyebrow = "Synthetic data", title = "Change by borough",
#'              subtitle = "Percentage points", source = "Demo") +
#'     theme_gla_map()
#'   gla_plotly(p)
#' }
#' @export
gla_plotly <- function(plot, theme = NULL, tooltip = "all", ...) {
  rlang::check_installed("plotly", "for interactive charts")
  built <- ggplot2::ggplot_build(plot)
  plot_theme <- built$plot$theme
  if (is.null(theme)) {
    fill <- plot_theme$plot.background$fill
    theme <- if (identical(tolower(fill), chart_tokens("dark")$surface)) "dark" else gla_mode()
  }
  theme <- match_mode(theme)
  ct <- chart_tokens(theme)
  labels <- plot$labels
  out <- plotly::ggplotly(plotly_bake_defaults(plot, plot_theme), tooltip = tooltip, ...)
  out <- ggla_plotly_style(out, gla_theme = if (theme == "dark") "inverse" else "default")
  out <- ggla_plotly_config(out)
  out$x$layout <- plotly_layout(out$x$layout, plot, plot_theme, ct)
  out$x$data <- plotly_traces(out$x$data, out$x$layout)
  out <- plotly_edge_labels(out, built, ct)
  out <- plotly_chrome(out, labels, ct)
  out <- plotly_bin_legend(out, built, ct)
  out
}

plotly_family <- "Inter, Arial, sans-serif"

plotly_font <- function(colour, role, weight = 400) {
  list(color = colour, family = plotly_family, size = gla_type()[[role]]$size, weight = weight)
}

# ggplotly() builds each layer again without the plot's theme, so a mark
# whose colour, width or line pattern comes from the theme (every house
# mark with nothing mapped) gets ggplot2's old defaults. This resolves those
# defaults through the theme and sets them on a copy of each layer as fixed
# aesthetics, which the rebuild keeps. The dashed patterns are given as
# names plotly understands.
plotly_bake_defaults <- function(plot, theme) {
  baked <- c("colour", "fill", "linewidth", "linetype", "alpha", "size", "shape",
             "stroke", "hjust", "vjust", "family", "fontface")
  layers <- lapply(plot$layers, function(layer) {
    # a new name for the copy: ggproto keeps its parent by reference, so
    # rebinding `layer` would make the copy its own parent
    copy <- ggplot2::ggproto(NULL, layer)
    defaults <- tryCatch(layer$geom$use_defaults(data.frame(), theme = theme),
                         error = function(e) NULL)
    if (is.null(defaults) || nrow(defaults) != 1) return(copy)
    taken <- c(names(layer$mapping), names(layer$aes_params),
               if (isTRUE(layer$inherit.aes)) names(plot$mapping))
    for (aes in setdiff(intersect(names(defaults), baked), taken)) {
      value <- defaults[[aes]]
      if (length(value) != 1 || is.na(value)) next
      if (aes == "linetype") value <- plotly_linetype(value)
      copy$aes_params[[aes]] <- value
    }
    if (!is.null(copy$aes_params$linetype)) {
      copy$aes_params$linetype <- plotly_linetype(copy$aes_params$linetype)
    }
    copy
  })
  plot@layers <- layers
  plot
}

# the house dash patterns and the names ggplotly carries them under; the
# traces get the pattern back in pixels (plotly_traces)
plotly_dashes <- c(dashed = "22", dotted = "12", dotdash = "4212", twodash = "32",
                   longdash = "42")
plotly_dash_names <- c(dash = "dashed", dot = "dotted", dashdot = "dotdash",
                       longdashdot = "twodash", longdash = "longdash")

plotly_linetype <- function(x) {
  if (is.numeric(x) || !grepl("^[0-9a-fA-F]{2,8}$", x)) return(x)
  known <- names(plotly_dashes)[match(x, plotly_dashes)]
  if (!is.na(known)) return(known)
  on <- strtoi(substr(x, 1, 1), 16L)
  off <- strtoi(substr(x, 2, 2), 16L)
  if (on <= off) "dotted" else "dashed"
}

# the layout in the house style: every axis in the tokens and the type,
# keeping what the theme said about ticks, lines and gridlines; the legend
# where the theme put it; the strips and axis titles in the typeface, the
# strips at the left of their panel
plotly_layout <- function(layout, plot, theme, ct) {
  layout$plot_bgcolor <- ct$surface
  layout$paper_bgcolor <- ct$surface
  layout$font <- plotly_font(ct$text, "tick")
  layout$margin <- utils::modifyList(layout$margin %||% list(), list(r = 16, pad = 0))
  axes <- grep("^[xy]axis", names(layout), value = TRUE)
  for (key in axes) {
    axis <- layout[[key]]
    axis$ticks <- ""
    axis$tickfont <- plotly_font(ct$label_muted, "tick")
    axis$tickangle <- 0
    axis$linecolor <- ct$axis
    axis$linewidth <- 1
    axis$gridcolor <- ct$grid
    axis$gridwidth <- 1
    axis$zeroline <- FALSE
    axis$automargin <- TRUE
    axis$title <- utils::modifyList(
      if (is.list(axis$title)) axis$title else list(text = axis$title),
      list(font = plotly_font(ct$text_muted, "axis_title", 500))
    )
    layout[[key]] <- axis
  }
  x_scale <- plot$scales$get_scales("x")
  if (!is.null(x_scale) && identical(x_scale$position, "top")) {
    for (key in grep("^xaxis", axes, value = TRUE)) layout[[key]]$side <- "top"
  }
  layout$showlegend <- !identical(theme$legend.position, "none")
  layout$legend <- list(
    bgcolor = ct$surface, bordercolor = "transparent", borderwidth = 0,
    x = 0, y = 1, orientation = "h", xanchor = "left", yanchor = "bottom",
    font = plotly_font(ct$label, "label"), title = list(text = "")
  )
  domains <- lapply(grep("^xaxis", axes, value = TRUE), function(key) layout[[key]]$domain)
  keep <- list()
  for (a in layout$annotations) {
    if (isTRUE(a$legendTitle)) {
      if (inherits(theme$legend.title, "element_blank")) next
      a$font <- plotly_font(ct$label, "label", 500)
    } else if (identical(a$annotationType, "axis")) {
      a$font <- plotly_font(ct$text_muted, "axis_title", 500)
    } else if (identical(a$xref, "paper") && identical(a$yref, "paper")) {
      # a facet strip: set in the label role at the left edge of its panel
      a$font <- plotly_font(ct$text, "label", 500)
      for (d in domains) {
        if (length(d) == 2 && a$x >= d[1] && a$x <= d[2]) {
          a$x <- d[1]
          a$xanchor <- "left"
          break
        }
      }
    }
    keep <- c(keep, list(a))
  }
  layout$annotations <- keep
  layout
}

# the traces as plotly can draw them: a range drawn to the panel edge
# with an infinite coordinate takes the axis range instead, the dashes
# take the house pattern, and a mark that converted to nothing goes
plotly_traces <- function(traces, layout) {
  finite <- function(values, key) {
    if (is.null(values) || !is.numeric(values) || all(is.finite(values))) return(values)
    range <- layout[[key]]$range
    if (length(range) != 2) return(values)
    values[values == -Inf] <- range[1]
    values[values == Inf] <- range[2]
    values
  }
  axis_key <- function(ref, which) sub(paste0("^", which), paste0(which, "axis"), ref %||% which)
  traces <- lapply(traces, function(trace) {
    trace$x <- finite(trace$x, axis_key(trace$xaxis, "x"))
    trace$y <- finite(trace$y, axis_key(trace$yaxis, "y"))
    dash <- trace$line$dash
    name <- if (length(dash) == 1) plotly_dash_names[dash] else NA
    if (!is.na(name)) {
      width <- max(trace$line$width %||% 1, 1)
      digits <- strtoi(strsplit(plotly_dashes[[name]], "")[[1]], 16L)
      trace$line$dash <- paste0(round(digits * width), "px", collapse = ",")
    }
    trace
  })
  Filter(function(trace) !is.null(trace$type), traces)
}

# the labels a trace has no position for become annotations: an end label
# sits a few pixels off its point and is dodged apart from its neighbours
# as the geom does, and the labels on reference lines are anchored to the
# paper on one side and to the data on the other
plotly_edge_labels <- function(out, built, ct) {
  layers <- built$plot$layers
  for (i in seq_along(layers)) {
    geom <- layers[[i]]$geom
    kind <- if (inherits(geom, "GeomGlaEndlabel")) "end" else if (inherits(geom, "GeomGlaEdgelabel")) {
      "edge"
    } else if (inherits(geom, "GeomGlaToplabel")) "top" else next
    data <- built$data[[i]]
    if (!nrow(data)) next
    if (kind == "end" && isTRUE(layers[[i]]$geom_params$dodge %||% TRUE)) {
      for (panel in unique(data$PANEL)) {
        rows <- data$PANEL == panel
        range <- built$layout$panel_params[[as.integer(panel)]]$y.range
        if (is.null(range)) next
        data$y[rows] <- separate_labels(data$y[rows], spacing = diff(range) / 22,
                                        lower = range[1], upper = range[2])
      }
    }
    dx <- layers[[i]]$geom_params$dx %||% 8
    for (r in seq_len(nrow(data))) {
      suffix <- if (as.integer(data$PANEL[r]) > 1) as.integer(data$PANEL[r]) else ""
      alpha <- if (is.null(data$alpha) || is.na(data$alpha[r])) 1 else data$alpha[r]
      colour <- ggplot2::alpha(data$colour[r], alpha)
      a <- switch(kind,
        end = {
          right <- (data$hjust[r] %||% 0) >= 0.5
          list(text = data$label[r], x = as.numeric(data$x[r]), xref = paste0("x", suffix),
               xanchor = if (right) "right" else "left", xshift = dx,
               y = data$y[r], yref = paste0("y", suffix), yanchor = "middle")
        },
        edge = list(text = data$label[r], x = 1, xref = "paper", xanchor = "right", xshift = -4,
                    y = data$y[r], yref = paste0("y", suffix), yanchor = "bottom", yshift = 4),
        top = list(text = data$label[r], x = as.numeric(data$x[r]), xref = paste0("x", suffix),
                   xanchor = "left", xshift = 4, y = 1, yref = "paper", yanchor = "top",
                   yshift = -4)
      )
      a$showarrow <- FALSE
      a$font <- plotly_font(colour, "label")
      out$x$layout$annotations <- c(out$x$layout$annotations, list(a))
    }
  }
  out
}

# the eyebrow and title as one title block, the subtitle under it, and the
# footer as an annotation below the plot with its labels in bold
plotly_chrome <- function(out, labels, ct) {
  roles <- gla_type()
  px <- function(role) roles[[role]]$size
  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  span <- function(text, size, colour, weight = 400) {
    sprintf("<span style='font-size:%dpx;color:%s;font-weight:%d'>%s</span>",
            size, colour, weight, text)
  }
  title <- labels$title
  eyebrow <- attr(title, "eyebrow")
  title <- if (is.null(title) || inherits(title, "waiver")) "" else as.character(title)
  lines <- c(
    if (!is.null(eyebrow) && nzchar(eyebrow)) span(esc(eyebrow), px("eyebrow"), ct$text_muted, 500),
    if (nzchar(title)) span(esc(title), px("title"), ct$text, 600),
    if (!is.null(labels$subtitle) && !inherits(labels$subtitle, "waiver")) {
      span(esc(as.character(labels$subtitle)), px("subtitle"), ct$text_muted)
    }
  )
  caption <- labels$caption
  footer <- if (!is.null(caption) && !inherits(caption, "waiver")) {
    parts <- strsplit(as.character(caption), "\n", fixed = TRUE)[[1]]
    parts <- vapply(parts, function(line) {
      head <- regmatches(line, regexpr("^(Source|Note):", line))
      if (length(head)) {
        rest <- trimws(substring(line, nchar(head) + 1))
        paste0("<b>", head, "</b> ", esc(rest))
      } else {
        esc(line)
      }
    }, character(1))
    paste(parts, collapse = "<br>")
  }
  n_lines <- length(lines)
  top <- if (n_lines) 16 + sum(vapply(lines, function(x) 0, numeric(1))) +
    (if (!is.null(eyebrow)) px("eyebrow") * 1.4 else 0) + px("title") * 1.3 +
    (if (!is.null(labels$subtitle)) px("subtitle") * 1.4 else 0) + 12 else 16
  out$x$layout$title <- list(
    text = paste(lines, collapse = "<br>"), x = 0, xref = "container", xanchor = "left",
    y = 1, yref = "container", yanchor = "top", pad = list(t = 16, l = 16),
    font = plotly_font(ct$text, "title", 600)
  )
  out$x$layout$margin$t <- top
  if (!is.null(footer)) {
    out$x$layout$margin$b <- (out$x$layout$margin$b %||% 16) + 32 + 16 * length(parts)
  }
  if (!is.null(footer)) {
    out$x$layout$annotations <- c(out$x$layout$annotations, list(list(
      text = footer, x = 0, y = 0, xref = "paper", yref = "paper", xanchor = "left",
      yanchor = "top", yshift = -36, showarrow = FALSE, align = "left",
      font = plotly_font(ct$text_muted, "caption")
    )))
  }
  out
}

# a stepped colour scale's bins as legend entries: one hidden marker per
# bin in the bin's colour, labelled with its range, highest first
plotly_bin_legend <- function(out, built, ct) {
  scale <- built$plot$scales$get_scales("fill") %||% built$plot$scales$get_scales("colour")
  if (is.null(scale) || !inherits(scale, "ScaleBinned")) return(out)
  limits <- scale$get_limits()
  breaks <- scale$get_breaks()
  edges <- sort(unique(c(limits[1], breaks, limits[2])))
  if (length(edges) < 2) return(out)
  mids <- edges[-length(edges)] + diff(edges) / 2
  colours <- scale$map(mids)
  labels <- scale$get_labels(edges)
  ranges <- paste0(labels[-length(labels)], " to ", labels[-1])
  order <- rev(seq_along(ranges))
  for (i in order) {
    out <- plotly::add_trace(
      out, x = list(NULL), y = list(NULL), type = "scatter", mode = "markers",
      marker = list(color = colours[i], symbol = "square", size = 14),
      name = ranges[i], showlegend = TRUE, hoverinfo = "none", inherit = FALSE
    )
  }
  plotly::layout(
    out, showlegend = TRUE,
    legend = list(orientation = "v", x = 1, y = 0.5, xanchor = "left", yanchor = "middle",
                  traceorder = "normal", itemclick = FALSE, itemdoubleclick = FALSE,
                  bgcolor = ct$surface)
  )
}
