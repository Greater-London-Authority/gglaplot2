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
#' The hover label is written in the chart's own words. A point reads its
#' series and each axis's title and value, formatted by that axis's scale.
#' An area of a map drawn with [geom_gla_sf()] reads its name and its
#' value, formatted by the fill scale's `labels`, anywhere inside its
#' boundary. The name is the layer's `label` aesthetic or, without one, a
#' `name` column in the data. To write the label yourself, map a `text`
#' aesthetic and pass `tooltip = "text"`.
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
  baked <- plotly_area_text(plotly_bake_defaults(plot, plot_theme), built)
  out <- plotly::ggplotly(baked, tooltip = tooltip, ...)
  out <- ggla_plotly_style(out, gla_theme = if (theme == "dark") "inverse" else "default")
  out <- ggla_plotly_config(out)
  out$x$layout <- plotly_layout(out$x$layout, plot, plot_theme, ct)
  out$x$data <- plotly_traces(out$x$data, out$x$layout)
  out <- plotly_edge_labels(out, built, ct)
  out$x$data <- plotly_hover(out$x$data, built)
  out <- plotly_chrome(out, labels, ct)
  out <- plotly_bin_legend(out, built, ct)
  out
}

plotly_family <- "Inter, Arial, sans-serif"

# a plotly font for a chart role: its size and, unless given, its weight
plotly_font <- function(colour, role, weight = NULL) {
  r <- gla_type()[[role]]
  list(color = colour, family = plotly_family, size = r$size,
       weight = weight %||% css_weight(r$weight))
}

# the CSS number for a weight name
css_weight <- function(name) {
  c(regular = 400, medium = 500, semibold = 600, bold = 700)[[name]]
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
      list(font = plotly_font(ct$text_muted, "axis_title"))
    )
    layout[[key]] <- axis
  }
  x_scale <- plot$scales$get_scales("x")
  if (!is.null(x_scale) && identical(x_scale$position, "top")) {
    for (key in grep("^xaxis", axes, value = TRUE)) layout[[key]]$side <- "top"
  }
  given <- layout$legend
  position <- theme$legend.position
  if (!is.character(position)) position <- "top"
  layout$showlegend <- position != "none"
  # where the theme put it: along the top or the bottom from the left edge,
  # or down the right
  place <- switch(
    position,
    right = list(x = 1.02, y = 1, orientation = "v", xanchor = "left", yanchor = "top"),
    bottom = list(x = 0, y = -0.15, orientation = "h", xanchor = "left", yanchor = "top"),
    list(x = 0, y = 1, orientation = "h", xanchor = "left", yanchor = "bottom")
  )
  layout$legend <- c(
    list(bgcolor = ct$surface, bordercolor = "transparent", borderwidth = 0),
    place,
    list(font = plotly_font(ct$label, "label"), title = list(text = ""))
  )
  # the legend's title, where the theme has one; ggplotly() carries it on
  # the legend or, in older releases, as an annotation beside it
  titled <- !inherits(theme$legend.title, "element_blank")
  title <- if (is.list(given)) given$title$text
  if (titled && is.character(title) && nzchar(title)) {
    layout$legend$title <- list(text = title, font = plotly_font(ct$label, "label"))
  }
  domains <- lapply(grep("^xaxis", axes, value = TRUE), function(key) layout[[key]]$domain)
  keep <- list()
  for (a in layout$annotations) {
    if (isTRUE(a$legendTitle)) {
      # as an annotation it would stay where ggplotly() had its own legend
      if (titled) {
        layout$legend$title <- list(text = a$text, font = plotly_font(ct$label, "label"))
      }
      next
    } else if (identical(a$annotationType, "axis")) {
      a$font <- plotly_font(ct$text_muted, "axis_title")
    } else if (identical(a$xref, "paper") && identical(a$yref, "paper")) {
      # a facet strip: set in the label role at the left edge of its panel
      a$font <- plotly_font(ct$text, "label")
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

# the hover label of each area of a map: its name, then its value in the
# fill scale's own labels. ggplotly() joins areas into traces by their fill
# and hover text and so loses which row a trace came from; the label is
# worked out here from the built data and carried through the conversion as
# a text aesthetic between two invisible marks, which plotly_hover() reads
# back. A layer with a text aesthetic of its own is left alone.
plotly_mark <- "\u2063"

plotly_area_text <- function(plot, built) {
  for (i in seq_along(plot$layers)) {
    layer <- plot$layers[[i]]
    if (!inherits(layer$geom, "GeomSf") || "text" %in% names(layer$mapping)) next
    data <- layer$layer_data(plot$data)
    drawn <- built$data[[i]]
    if (!is.data.frame(data) || nrow(data) != nrow(drawn)) next
    mapping <- layer$mapping
    if (isTRUE(layer$inherit.aes)) {
      mapping <- utils::modifyList(as.list(plot$mapping), as.list(mapping))
    }
    text <- area_text(data, drawn$label, mapping, built$plot$scales)
    if (is.null(text)) next
    data$.gla_hover <- paste0(plotly_mark, text, plotly_mark)
    layer$data <- data
    layer$mapping$text <- ggplot2::aes(text = .data$.gla_hover)$text
  }
  plot
}

area_text <- function(data, name, mapping, scales) {
  name <- name %||% area_names(data)
  value <- NULL
  for (aes in c("fill", "colour")) {
    scale <- scales$get_scales(aes)
    values <- if (!is.null(scale) && !is.null(mapping[[aes]])) {
      tryCatch(rlang::eval_tidy(mapping[[aes]], data), error = function(e) NULL)
    }
    if (length(values) != nrow(data)) next
    value <- area_value_labels(scale, values)
    title <- scale$name
    if (is.character(title) && nzchar(title)) value <- paste0(title, ": ", value)
    value[is.na(values)] <- NA
    break
  }
  if (is.null(name) && is.null(value)) return(NULL)
  esc <- function(x) {
    gsub("<", "&lt;", gsub("&", "&amp;", as.character(x), fixed = TRUE), fixed = TRUE)
  }
  if (is.null(name)) return(ifelse(is.na(value), "No data", value))
  name <- paste0("<b>", esc(name), "</b>")
  if (is.null(value)) return(name)
  paste(name, ifelse(is.na(value), "No data", value), sep = "<br>")
}

# a column of names to call the areas by: one called name, or ending in it
# or in the NM of an ONS boundary file
area_names <- function(data) {
  cols <- grep("(^|_)name$|nm$", names(data), ignore.case = TRUE, value = TRUE)
  text <- vapply(cols, function(col) is.character(data[[col]]) || is.factor(data[[col]]),
                 logical(1))
  if (any(text)) as.character(data[[cols[text][1]]])
}

# a mapped value as the scale's labels write it. A house label function set
# to a fixed accuracy gives one more figure than it gives the legend, as the
# web charts' tips give one more than their axes; a scale with no labels of
# its own gives the number to four significant figures.
area_value_labels <- function(scale, values) {
  if (!is.numeric(values)) return(as.character(values))
  labels <- scale_labels(scale)
  refine <- attr(labels, "gla_refine")
  if (!is.null(refine)) return(refine$at(refine$accuracy / 10)(values))
  if (is.function(labels)) {
    out <- tryCatch(as.character(labels(signif(values, 4))), error = function(e) NULL)
    if (length(out) == length(values)) return(out)
  }
  vapply(signif(values, 4), function(v) format(v, big.mark = ",", trim = TRUE), character(1))
}

# the hover label in the chart's own words: the series, then each axis as
# its title and the value formatted by that axis's scale, so a chart whose
# axis reads in pounds or per cent hovers in pounds or per cent too. An
# area of a map takes the label plotly_area_text() made for it. Other marks
# with a shape rather than a point (a treemap's tiles, a map with a text
# aesthetic of its own) keep the label ggplotly gave them.
plotly_hover <- function(traces, built) {
  scales <- built$plot$scales
  labels <- built$plot$labels
  panel <- built$layout$panel_params[[1]]
  axis <- function(aes) {
    scale <- scales$get_scales(aes)
    if (is.null(scale)) return(NULL)
    title <- scale$name
    if (is.null(title) || inherits(title, "waiver")) title <- labels[[aes]]
    if (is.null(title) || inherits(title, "waiver")) title <- ""
    discrete <- isTRUE(scale$is_discrete())
    levels <- if (discrete) as.character(panel[[aes]]$get_labels())
    date <- inherits(scale, "ScaleContinuousDate") && inherits(scale$labels, "waiver")
    breaks <- panel[[aes]]$breaks
    breaks <- breaks[is.finite(breaks)]
    fmt <- function(values) {
      values <- as.numeric(unclass(values))
      if (discrete) {
        at <- round(values)
        ok <- !is.na(at) & at >= 1 & at <= length(levels)
        out <- rep(NA_character_, length(values))
        out[ok] <- levels[at[ok]]
        return(out)
      }
      if (date) return(trimws(format(as.Date(values, origin = "1970-01-01"), "%e %B %Y")))
      if (inherits(scale$labels, "waiver")) {
        # a plain number at the axis's own precision
        out <- label_gla_number()(c(values, breaks))
        return(out[seq_along(values)])
      }
      # the axis's labels at four significant figures, as the web charts' tips
      out <- tryCatch(as.character(scale$get_labels(signif(values, 4))), error = function(e) NULL)
      if (length(out) != length(values)) return(format(values, big.mark = ",", trim = TRUE))
      out
    }
    list(title = as.character(title), format = fmt)
  }
  x <- axis("x")
  y <- axis("y")
  line <- function(axis, values) {
    values <- unclass(values)
    if (is.null(axis) || !is.numeric(values)) return(NULL)
    text <- axis$format(values)
    if (all(is.na(text))) return(NULL)
    if (nzchar(axis$title)) paste0(axis$title, ": ", text) else text
  }
  marked <- paste0(plotly_mark, "(.*?)", plotly_mark)
  lapply(traces, function(trace) {
    if (is.character(trace$text) && any(grepl(plotly_mark, trace$text, fixed = TRUE))) {
      # an area of a map: the label made for it, shown anywhere inside it
      found <- regmatches(trace$text, regexpr(marked, trace$text, perl = TRUE))
      trace$text <- gsub(plotly_mark, "", found, fixed = TRUE)
      trace$hoverinfo <- "text"
      if (!is.null(trace$fill) && length(trace$text) == 1) trace$hoveron <- "fills"
      return(trace)
    }
    shape <- !is.null(trace$fill) || identical(trace$mode, "text") || !length(trace$x)
    if (shape || is.null(trace$text)) return(trace)
    series <- if (!is.null(trace$legendgroup) && !is.null(trace$name)) trace$name
    parts <- list(rep(series, length(trace$x)), line(x, trace$x), line(y, trace$y))
    parts <- Filter(Negate(is.null), parts)
    if (!length(parts)) return(trace)
    trace$text <- do.call(paste, c(parts, sep = "<br>"))
    trace$hoverinfo <- "text"
    trace
  })
}

# the labels a trace has no position for become annotations: an end label
# sits a few pixels off its point and is dodged apart from its neighbours
# as the geom does, and the labels on reference lines are anchored to the
# paper on one side and to the data on the other
plotly_edge_labels <- function(out, built, ct) {
  layers <- built$plot$layers
  for (i in seq_along(layers)) {
    geom <- layers[[i]]$geom
    kinds <- c(GeomGlaEndlabel = "end", GeomGlaEdgelabel = "edge", GeomGlaToplabel = "top")
    kind <- kinds[vapply(names(kinds), function(class) inherits(geom, class), logical(1))]
    if (!length(kind)) next
    kind <- kind[[1]]
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
  lh <- function(role) roles[[role]]$size * roles[[role]]$line_height
  weight <- function(role) css_weight(roles[[role]]$weight)
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
    if (!is.null(eyebrow) && nzchar(eyebrow)) {
      span(esc(eyebrow), px("eyebrow"), ct$text_muted, weight("eyebrow"))
    },
    if (nzchar(title)) span(esc(title), px("title"), ct$text, weight("title")),
    if (!is.null(labels$subtitle) && !inherits(labels$subtitle, "waiver")) {
      span(esc(as.character(labels$subtitle)), px("subtitle"), ct$text_muted, weight("subtitle"))
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
  # the title block's height on the roles' line heights, with the 4px step
  # under the eyebrow and the container's 8px above the chart
  top <- if (length(lines)) {
    16 + (if (!is.null(eyebrow)) lh("eyebrow") + 4 else 0) + lh("title") +
      (if (!is.null(labels$subtitle)) lh("subtitle") else 0) + 8
  } else {
    16
  }
  out$x$layout$title <- list(
    text = paste(lines, collapse = "<br>"), x = 0, xref = "container", xanchor = "left",
    y = 1, yref = "container", yanchor = "top", pad = list(t = 16, l = 16),
    font = plotly_font(ct$text, "title")
  )
  out$x$layout$margin$t <- top
  if (!is.null(footer)) {
    out$x$layout$margin$b <- (out$x$layout$margin$b %||% 16) + 32 + 16 * length(parts)
  }
  # a legend along the bottom sits between the plot and the footer
  below <- isTRUE(out$x$layout$showlegend) && isTRUE(out$x$layout$legend$y < 0)
  if (below) out$x$layout$margin$b <- (out$x$layout$margin$b %||% 16) + 32
  if (!is.null(footer)) {
    out$x$layout$annotations <- c(out$x$layout$annotations, list(list(
      text = footer, x = 0, y = 0, xref = "paper", yref = "paper", xanchor = "left",
      yanchor = "top", yshift = if (below) -76 else -36, showarrow = FALSE, align = "left",
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
  bins <- bin_ranges(scale)
  if (is.null(bins)) return(out)
  colours <- scale$map(bins$mid)
  ranges <- bins$label
  # set on the built object itself: a trace added with plotly::add_trace()
  # is built again when the widget prints and loses its name and its point,
  # and a trace with neither draws no legend entry
  axis <- out$x$data[[1]]
  keys <- lapply(rev(seq_along(ranges)), function(i) {
    list(
      x = I(NA_real_), y = I(NA_real_), type = "scatter", mode = "markers",
      marker = list(color = colours[i], symbol = "square", size = 14),
      name = ranges[i], showlegend = TRUE, hoverinfo = "none",
      xaxis = axis$xaxis %||% "x", yaxis = axis$yaxis %||% "y"
    )
  })
  out$x$data <- c(out$x$data, keys)
  out$x$layout$showlegend <- TRUE
  out$x$layout$legend <- utils::modifyList(out$x$layout$legend %||% list(), list(
    orientation = "v", x = 1, y = 0.5, xanchor = "left", yanchor = "middle",
    traceorder = "normal", itemclick = FALSE, itemdoubleclick = FALSE, bgcolor = ct$surface
  ))
  out
}
