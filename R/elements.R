#' Theme elements for the chart chrome
#'
#' Two text elements that draw what the web charts' title group and footer
#' draw. `element_gla_title()` is [ggplot2::element_text()] for the plot
#' title with an eyebrow above it, the short label set by
#' `gla_labs(eyebrow = )`. `element_gla_caption()` is the footer: a line
#' that starts `Source:` or `Note:` has that label set in bold, as the web
#' footer does, and the lines sit `gap` apart. [theme_gla()] sets both, so
#' they are only needed for a theme built by hand. A plain
#' [ggplot2::element_text()] added to the theme afterwards keeps the eyebrow
#' and the labels and changes what it sets.
#'
#' @inheritParams ggplot2::element_text
#' @param eyebrow A list with `size` (points), `colour`, `family` and `face`
#'   for the eyebrow line. Anything missing is taken from the title.
#' @param label A list with `family` and `face` for the `Source:` and
#'   `Note:` labels.
#' @param gap Space between the footer's lines, in points.
#' @return A theme element.
#' @examples
#' library(ggplot2)
#' ggplot(gla_yearly, aes(year, average, colour = series)) +
#'   geom_gla_line() +
#'   scale_colour_gla() +
#'   gla_labs(eyebrow = "Demo data", title = "A title with an eyebrow",
#'            source = "LDN Viz Tools", note = "For demonstration") +
#'   theme_gla()
#' @name element_gla
NULL

#' @rdname element_gla
#' @export
element_gla_title <- S7::new_class(
  "element_gla_title", parent = ggplot2::element_text,
  properties = list(eyebrow = S7::class_list)
)

#' @rdname element_gla
#' @export
element_gla_caption <- S7::new_class(
  "element_gla_caption", parent = ggplot2::element_text,
  properties = list(label = S7::class_list, gap = S7::class_numeric)
)

# the element as the plain text element it extends
as_text_element <- function(element) {
  S7::convert(element, ggplot2::element_text)
}

# the S7 classes carry their package-qualified name only, so the drawing
# methods are registered under it on load rather than through NAMESPACE
draw_gla_title <- function(element, label = "", ..., margin_x = FALSE, margin_y = TRUE) {
  eyebrow <- attr(label, "eyebrow")
  text <- as.character(label)
  base <- as_text_element(element)
  if (is.null(eyebrow) || !nzchar(eyebrow)) {
    return(ggplot2::element_grob(base, text, ..., margin_x = margin_x, margin_y = margin_y))
  }
  style <- element@eyebrow
  above <- base
  above@size <- style$size %||% base@size
  above@colour <- style$colour %||% base@colour
  above@family <- style$family %||% base@family
  above@face <- style$face %||% "plain"
  margin <- base@margin
  # the title's top margin moves to the eyebrow; the eyebrow's own gap to
  # the title sits below it
  above@margin <- margin
  above@margin[3] <- grid::unit(style$gap %||% 2, "pt")
  base@margin[1] <- grid::unit(0, "pt")
  grobs <- list(ggplot2::element_grob(above, eyebrow, ..., margin_x = margin_x,
                                      margin_y = margin_y))
  if (length(text) && any(nzchar(text))) {
    grobs <- c(grobs, list(ggplot2::element_grob(base, text, ..., margin_x = margin_x,
                                                 margin_y = margin_y)))
  }
  stack_grobs(grobs)
}

draw_gla_caption <- function(element, label = "", ..., margin_x = FALSE, margin_y = TRUE) {
  base <- as_text_element(element)
  text <- paste(as.character(label), collapse = "\n")
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  labelled <- grepl("^(Source|Note):", lines)
  if (!any(labelled)) {
    return(ggplot2::element_grob(base, label, ..., margin_x = margin_x, margin_y = margin_y))
  }
  outer <- base@margin
  inner <- base
  inner@margin <- ggplot2::margin(0, 0, 0, 0)
  bold <- inner
  bold@family <- element@label$family %||% inner@family
  bold@face <- element@label$face %||% "bold"
  gap <- grid::unit(if (length(element@gap)) element@gap[1] else 4, "pt")
  # side margins on, at zero, so the grob reports its width to the layout
  line <- function(el, txt) {
    ggplot2::element_grob(el, txt, ..., margin_x = TRUE, margin_y = TRUE)
  }
  grobs <- list(spacer(outer[1]))
  for (i in seq_along(lines)) {
    if (i > 1) grobs <- c(grobs, list(spacer(gap)))
    g <- if (labelled[i]) {
      head <- sub("^((Source|Note):).*$", "\\1", lines[i])
      rest <- trimws(substring(lines[i], nchar(head) + 1), "left")
      beside_grobs(line(bold, head), line(inner, rest), gap)
    } else {
      line(inner, lines[i])
    }
    grobs <- c(grobs, list(g))
  }
  grobs <- c(grobs, list(spacer(outer[3])))
  stack_grobs(grobs)
}

#' @importFrom ggplot2 merge_element
NULL

# a plain text element added over one of these keeps the eyebrow or the
# labels and changes what it sets; ggplot2 refuses to merge the classes
S7::method(merge_element, list(ggplot2::element_text, element_gla_title)) <-
  function(new, old, ...) {
    over_element(new, old)
  }

S7::method(merge_element, list(ggplot2::element_text, element_gla_caption)) <-
  function(new, old, ...) {
    over_element(new, old)
  }

over_element <- function(new, old) {
  set <- names(which(lengths(S7::props(new)) > 0))
  set <- setdiff(set, c("inherit.blank", "debug"))
  S7::props(old)[set] <- S7::props(new, set)
  old
}

# a grob inside a viewport, wrapped rather than edited so a grob that
# carries its own layout viewport keeps it
in_viewport <- function(grob, vp) {
  grid::gTree(children = grid::gList(grob), vp = vp, cl = "gla_in_viewport")
}

#' @exportS3Method grid::heightDetails
heightDetails.gla_in_viewport <- function(x) grid::grobHeight(x$children[[1]])

#' @exportS3Method grid::widthDetails
widthDetails.gla_in_viewport <- function(x) grid::grobWidth(x$children[[1]])

# an empty grob of a height, for the margins and gaps in a stack
spacer <- function(height) {
  grid::rectGrob(height = height, gp = grid::gpar(col = NA, fill = NA))
}

# grobs one above the other, each at its own height, measured by grid as
# a whole so the plot layout gives the stack the room it needs
stack_grobs <- function(grobs) {
  heights <- lapply(grobs, grid::grobHeight)
  top <- grid::unit(1, "npc")
  for (i in seq_along(grobs)) {
    vp <- grid::viewport(y = top, just = "top", height = heights[[i]])
    grobs[[i]] <- in_viewport(grobs[[i]], vp)
    top <- top - heights[[i]]
  }
  grid::gTree(children = do.call(grid::gList, grobs), cl = "gla_stack")
}

#' @exportS3Method grid::heightDetails
heightDetails.gla_stack <- function(x) {
  Reduce(`+`, lapply(x$children, grid::grobHeight))
}

#' @exportS3Method grid::widthDetails
widthDetails.gla_stack <- function(x) {
  do.call(max, lapply(x$children, grid::grobWidth))
}

# a grob and another to its right, a gap apart, from the left edge
beside_grobs <- function(left, right, gap) {
  width <- grid::grobWidth(left)
  left <- in_viewport(left, grid::viewport(x = grid::unit(0, "npc"), just = "left",
                                           width = width))
  right <- in_viewport(right, grid::viewport(x = width + gap, just = "left",
                                             width = grid::unit(1, "npc") - width - gap))
  grid::gTree(children = grid::gList(left, right), gap = gap, cl = "gla_beside")
}

#' @exportS3Method grid::heightDetails
heightDetails.gla_beside <- function(x) {
  do.call(max, lapply(x$children, grid::grobHeight))
}

#' @exportS3Method grid::widthDetails
widthDetails.gla_beside <- function(x) {
  Reduce(`+`, lapply(x$children, grid::grobWidth)) + x$gap
}
