#' A treemap
#'
#' Part-to-whole data as nested rectangles, each sized to its value, as the
#' web charts draw them: squarified so the tiles stay close to square,
#' largest first from the top left, a one pixel gap of the surface colour
#' between tiles, and the label inside the tile where it fits. Map the
#' tile's size to `area` and its name to `label`; map `fill` to a parent
#' category and the tiles are laid out group by group, each group's tiles
#' together, with the categorical palette telling the groups apart.
#'
#' Labels are drawn centred, the layout the web charts prefer, or at the
#' top left with `label_position = "topleft"`. A tile too small for its
#' label gets none: `min_label` is the share of the whole below which the
#' label is dropped. The axes carry nothing and [theme_gla_map()] is the
#' theme to use.
#'
#' @inheritParams ggplot2::layer
#' @param ... Other arguments passed on to [ggplot2::layer()].
#' @param label_position `"centre"` or `"topleft"`.
#' @param min_label The share of the total area below which a tile's label
#'   is left out.
#' @param aspect The width to height ratio the tiles are squarified for;
#'   the web charts draw treemaps at 16 by 6.
#' @param theme `"light"` or `"dark"`, for the gap between tiles. Defaults
#'   to the mode set by [gla_mode()].
#' @param na.rm Whether to drop missing values silently.
#' @return A list of two layers, the tiles and the labels.
#' @examples
#' library(ggplot2)
#' d <- data.frame(
#'   borough = c("Camden", "Hackney", "Islington", "Barnet", "Brent", "Ealing", "Harrow"),
#'   area = c("Inner", "Inner", "Inner", "Outer", "Outer", "Outer", "Outer"),
#'   value = c(42, 35, 28, 51, 44, 39, 22)
#' )
#' ggplot(d, aes(area = value, label = borough, fill = area)) +
#'   geom_gla_treemap() +
#'   scale_fill_gla() +
#'   gla_labs(title = "A treemap", subtitle = "Synthetic values") +
#'   theme_gla_map(legend = "top")
#' @export
geom_gla_treemap <- function(mapping = NULL, data = NULL, ...,
                             label_position = c("centre", "topleft"), min_label = 0.02,
                             aspect = 16 / 6, theme = gla_mode(), na.rm = FALSE,
                             show.legend = NA, inherit.aes = TRUE) {
  label_position <- match.arg(label_position)
  theme <- match_mode(theme)
  gap <- unname(gla_token("chart-surface", theme))
  roles <- gla_type()
  list(
    ggplot2::layer(
      data = data, mapping = mapping, stat = StatGlaTreemap, geom = GeomGlaTreemap,
      position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, aspect = aspect, colour = gap, ...)
    ),
    ggplot2::layer(
      data = data, mapping = mapping, stat = StatGlaTreemap, geom = GeomGlaTreemapLabel,
      position = "identity", show.legend = FALSE, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, aspect = aspect, label_position = label_position,
                    min_label = min_label, size = px(roles$label$size),
                    family = gla_font("medium"))
    ),
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, 1)),
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1)),
    ggplot2::labs(x = NULL, y = NULL)
  )
}

#' @rdname geom_gla_treemap
#' @format NULL
#' @usage NULL
#' @export
StatGlaTreemap <- ggplot2::ggproto(
  "StatGlaTreemap", ggplot2::Stat,
  required_aes = "area",
  default_aes = ggplot2::aes(fill = NULL, label = NULL),
  # tiles laid out in the unit square, groups first when fill is mapped,
  # then each group's tiles inside its rectangle
  compute_panel = function(data, scales, aspect = 16 / 6) {
    data <- data[!is.na(data$area) & data$area > 0, , drop = FALSE]
    if (nrow(data) == 0) return(data.frame())
    data$share <- data$area / sum(data$area)
    by_fill <- !is.null(data$fill) && length(unique(data$fill)) > 1
    if (!by_fill) {
      return(cbind(data, treemap_rects(data$share, c(0, 0, 1, 1), aspect)))
    }
    groups <- stats::aggregate(share ~ fill, data, sum)
    groups <- groups[order(-groups$share), , drop = FALSE]
    outer <- treemap_rects(groups$share, c(0, 0, 1, 1), aspect)
    pieces <- lapply(seq_len(nrow(groups)), function(i) {
      rows <- data[data$fill == groups$fill[i], , drop = FALSE]
      box <- c(outer$xmin[i], outer$ymin[i], outer$xmax[i], outer$ymax[i])
      inner <- treemap_rects(rows$share / sum(rows$share), box,
                             aspect * (box[3] - box[1]) / (box[4] - box[2]))
      cbind(rows, inner)
    })
    do.call(rbind, pieces)
  }
)

# the squarified treemap layout as d3 draws it (treemapSquarify with the
# golden ratio): values largest first, each row laid along the shorter
# side of what is left and closed when the next tile would make the row's
# worst aspect ratio worse, judged against the drawn shape of the chart
treemap_rects <- function(shares, box, aspect = 1) {
  ord <- order(-shares)
  values <- shares[ord]
  phi <- (1 + sqrt(5)) / 2
  # the box in drawn units: x stretched by the aspect so a square here is a
  # square on the chart; y runs downward as d3's does and is flipped at the end
  x0 <- box[1] * aspect
  x1 <- box[3] * aspect
  y0 <- 0
  y1 <- box[4] - box[2]
  value <- sum(values)
  n <- length(values)
  out <- matrix(NA_real_, n, 4)
  i0 <- 1
  while (i0 <= n) {
    dx <- x1 - x0
    dy <- y1 - y0
    i1 <- i0
    sum_value <- values[i1]
    min_value <- max_value <- sum_value
    alpha <- max(dy / dx, dx / dy) / (value * phi)
    beta <- sum_value^2 * alpha
    min_ratio <- max(max_value / beta, beta / min_value)
    while (i1 < n) {
      node <- values[i1 + 1]
      sum_try <- sum_value + node
      beta <- sum_try^2 * alpha
      new_ratio <- max(max(max_value, node) / beta, beta / min(min_value, node))
      if (new_ratio > min_ratio) break
      sum_value <- sum_try
      min_value <- min(min_value, node)
      max_value <- max(max_value, node)
      min_ratio <- new_ratio
      i1 <- i1 + 1
    }
    idx <- i0:i1
    if (dx < dy) {
      # a strip across the top, tiles side by side
      strip_y1 <- y0 + dy * sum_value / value
      pos <- x0
      for (k in idx) {
        width <- dx * values[k] / sum_value
        out[k, ] <- c(pos, y0, pos + width, strip_y1)
        pos <- pos + width
      }
      y0 <- strip_y1
    } else {
      # a strip down the left, tiles stacked
      strip_x1 <- x0 + dx * sum_value / value
      pos <- y0
      for (k in idx) {
        height <- dy * values[k] / sum_value
        out[k, ] <- c(x0, pos, strip_x1, pos + height)
        pos <- pos + height
      }
      x0 <- strip_x1
    }
    value <- value - sum_value
    i0 <- i1 + 1
  }
  # held to the box, as the last tile's edge can land a hair outside it
  # through floating point and the axis scale would censor it
  rects <- data.frame(
    xmin = pmax(out[, 1] / aspect, box[1]), xmax = pmin(out[, 3] / aspect, box[3]),
    ymin = pmax(box[4] - out[, 4], box[2]), ymax = pmin(box[4] - out[, 2], box[4])
  )
  rects[order(ord), , drop = FALSE]
}

#' @rdname geom_gla_treemap
#' @format NULL
#' @usage NULL
#' @export
GeomGlaTreemap <- ggplot2::ggproto(
  "GeomGlaTreemap", ggplot2::GeomRect,
  default_aes = ggplot2::aes(
    colour = ggplot2::from_theme(paper), fill = ggplot2::from_theme(accent),
    linewidth = ggplot2::from_theme(borderwidth), linetype = 1, alpha = NA
  ),
  draw_panel = function(self, data, panel_params, coord, lineend = "butt", linejoin = "mitre") {
    ggplot2::ggproto_parent(ggplot2::GeomRect, self)$draw_panel(
      data, panel_params, coord, lineend = lineend, linejoin = linejoin
    )
  }
)

#' @rdname geom_gla_treemap
#' @format NULL
#' @usage NULL
#' @export
GeomGlaTreemapLabel <- ggplot2::ggproto(
  "GeomGlaTreemapLabel", ggplot2::GeomText,
  required_aes = c("xmin", "xmax", "ymin", "ymax"),
  default_aes = ggplot2::aes(
    colour = ggplot2::from_theme(paper), size = ggplot2::from_theme(fontsize),
    angle = 0, hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),
  draw_panel = function(data, panel_params, coord, label_position = "centre",
                        min_label = 0.02, na.rm = FALSE) {
    if (is.null(data$label)) return(ggplot2::zeroGrob())
    data <- data[!is.na(data$label) & data$share >= min_label, , drop = FALSE]
    if (nrow(data) == 0) return(ggplot2::zeroGrob())
    inset <- 0.012
    if (label_position == "topleft") {
      data$x <- data$xmin + inset
      data$y <- data$ymax - inset
      data$hjust <- 0
      data$vjust <- 1
    } else {
      data$x <- (data$xmin + data$xmax) / 2
      data$y <- (data$ymin + data$ymax) / 2
    }
    ggplot2::GeomText$draw_panel(data, panel_params, coord, na.rm = na.rm)
  }
)
