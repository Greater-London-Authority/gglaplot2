# The theme elements are S7 objects and the package reads their properties
# with `@`. Base R dispatches `@` to S7 from 4.3.0; before that S7's own
# `@` has to be imported, as ggplot2 does, or a plot stops with "trying to
# get slot ... that is not an S4 object".

#' @keywords internal
#' @importFrom rlang .data
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
"_PACKAGE"
