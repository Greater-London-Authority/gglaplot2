#' Number, percentage, pound and date labels in the house shapes
#'
#' Label functions for a scale's `labels` argument, in the shapes the web
#' charts' axes and tooltips use: a comma every three digits, a point for
#' the decimal, `£` with nothing between it and the number, and a date
#' written out as `5 March 2024`. Each is a thin wrapper over the scales
#' package with those defaults set, and takes that function's other
#' arguments through `...`.
#'
#' `label_gla_number()` leaves the decimals to scales when `accuracy` is
#' `NULL`, which picks the fewest that keep the breaks apart. With
#' `short = TRUE` a thousand and up is cut to `k`, `m` and `bn`, the way a
#' UK reader abbreviates, to one decimal where it says something: `45k`,
#' `350m`, `1.2bn`.
#'
#' `label_gla_percent()` expects fractions, as the web charts do, so `0.05`
#' is `5%`; pass `scale = 1` for data already in percentage points. The web
#' axes round to whole percentages and their tooltips to one decimal, which
#' is `accuracy = 1` and `accuracy = 0.1`.
#'
#' `label_gla_pounds()` is `label_gla_number()` with `£` in front, so
#' `label_gla_pounds(short = TRUE)` writes `£1.2bn`.
#'
#' `label_gla_date()` writes the day, month and year in full, as the web
#' tooltips do. An axis usually wants less: `"%Y"` or `"%b %Y"`.
#'
#' @param accuracy The number to round to: `1` for whole numbers, `0.1` for
#'   one decimal. `NULL` lets scales choose from the breaks.
#' @param scale What to multiply the value by before formatting.
#' @param prefix,suffix Text either side of the number.
#' @param short Cut a thousand and up to `k`, `m` and `bn`.
#' @param format A [strftime()] format for the date.
#' @param ... Passed to [scales::label_number()] or
#'   [scales::label_percent()].
#' @return A function that takes a vector and returns character labels.
#' @examples
#' label_gla_number()(c(1234, 1234567))
#' label_gla_number(short = TRUE)(c(45000, 3.5e6, 1.2e9))
#' label_gla_percent()(c(0.05, 0.123))
#' label_gla_percent(accuracy = 0.1, scale = 1)(12.34)
#' label_gla_pounds()(c(50, 1234))
#' label_gla_pounds(short = TRUE)(c(45000, 1.2e9))
#' label_gla_date()(as.Date("2024-03-05"))
#'
#' library(ggplot2)
#' ggplot(gla_yearly, aes(year, average, colour = series)) +
#'   geom_gla_line() +
#'   scale_y_continuous(breaks = gla_breaks(), labels = label_gla_pounds(short = TRUE)) +
#'   theme_gla()
#' @name label_gla
NULL

# a thousand and up as k, m and bn, as a UK reader abbreviates
gla_short_scale <- function() c(0, k = 1e3, m = 1e6, bn = 1e9)

#' @rdname label_gla
#' @export
label_gla_number <- function(accuracy = NULL, scale = 1, prefix = "", suffix = "",
                             short = FALSE, ...) {
  args <- list(accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
               big.mark = ",", decimal.mark = ".", trim = TRUE, ...)
  if (short) {
    args$scale_cut <- gla_short_scale()
    # one decimal where it says something, so 1.2bn and 45k rather than 1bn
    # and 45.0k; scales would pick one accuracy for every value at once
    if (is.null(accuracy)) {
      args$accuracy <- 0.1
      args$drop0trailing <- TRUE
    }
  }
  rlang::exec(scales::label_number, !!!args)
}

#' @rdname label_gla
#' @export
label_gla_percent <- function(accuracy = 1, scale = 100, ...) {
  scales::label_percent(accuracy = accuracy, scale = scale, big.mark = ",", decimal.mark = ".",
                        trim = TRUE, ...)
}

#' @rdname label_gla
#' @export
label_gla_pounds <- function(accuracy = NULL, scale = 1, suffix = "", short = FALSE, ...) {
  label_gla_number(accuracy = accuracy, scale = scale, prefix = "\u00a3", suffix = suffix,
                   short = short, ...)
}

#' @rdname label_gla
#' @export
label_gla_date <- function(format = "%e %B %Y") {
  function(x) {
    out <- format(x, format)
    out[!is.na(x)] <- trimws(out[!is.na(x)])
    out
  }
}
