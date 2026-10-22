# CSS pixels to the millimetres ggplot2 measures lines in. The design system
# specifies strokes in pixels (a 2px line, a 1px gridline); at 96 dpi a pixel
# is 0.75pt.
px <- function(x) x * 0.75 / ggplot2::.pt

# CSS pixels to points, for font sizes
px_pt <- function(x) x * 0.75

`%||%` <- function(x, y) if (is.null(x)) y else x
