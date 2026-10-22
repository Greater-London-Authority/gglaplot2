#' Show the palettes
#'
#' A swatch chart of the palettes in [gla_pal()], the way a palette page
#' in a style guide shows them, so a reader can see the colours before
#' choosing one.
#'
#' @param type Palette types to show. Default all.
#' @param theme `"light"` or `"dark"`.
#' @return A ggplot.
#' @examples
#' gla_show_pal()
#' gla_show_pal("sequential", theme = "dark")
#' @export
gla_show_pal <- function(type = c("categorical", "sequential", "diverging", "sex", "change"),
                         theme = c("light", "dark")) {
  type <- match.arg(type, several.ok = TRUE)
  theme <- match.arg(theme)
  rows <- lapply(type, function(ty) {
    pal <- gla_pal(ty, theme = theme)
    data.frame(palette = ty, position = seq_along(pal), colour = unname(pal),
               name = if (is.null(names(pal))) "" else names(pal), stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, rows)
  d$palette <- factor(d$palette, levels = rev(type))
  ggplot2::ggplot(d, ggplot2::aes(.data$position, .data$palette)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$colour), width = 0.92, height = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = .data$name), vjust = 2.6,
                       size = px_pt(12) / ggplot2::.pt, family = gla_font(),
                       colour = unname(gla_token("chart-label-muted", theme))) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0.6)) +
    theme_gla(theme = theme, gridlines = "none", legend = "none", axis_titles = "none") +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(hjust = 1, size = px_pt(14)),
                   axis.line = ggplot2::element_blank())
}

#' Show a group of tokens in both modes
#'
#' The light and dark values of the tokens matching a pattern, side by
#' side. The default shows the `data-*` group a chart draws with.
#'
#' @param pattern A regular expression matched against token names.
#' @return A ggplot.
#' @examples
#' gla_show_tokens("^data-")
#' gla_show_tokens("^chart-")
#' @export
gla_show_tokens <- function(pattern = "^data-") {
  tk <- gglaplot2::gla_tokens
  tk <- tk[grepl(pattern, tk$token), ]
  if (nrow(tk) == 0) stop("No tokens match ", pattern, call. = FALSE)
  d <- rbind(
    data.frame(token = tk$token, mode = "light", colour = tk$light, stringsAsFactors = FALSE),
    data.frame(token = tk$token, mode = "dark", colour = tk$dark, stringsAsFactors = FALSE)
  )
  d$token <- factor(d$token, levels = rev(tk$token))
  d$mode <- factor(d$mode, levels = c("light", "dark"))
  d$label <- toupper(d$colour)
  d$text <- ifelse(contrast_dark(d$colour), gla_token("chart-label-muted"),
                   gla_token("chart-surface"))
  ggplot2::ggplot(d, ggplot2::aes(.data$mode, .data$token)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$colour), width = 0.94, height = 0.86) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label, colour = .data$text),
                       size = px_pt(12) / ggplot2::.pt, family = gla_font()) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_discrete(position = "top", expand = ggplot2::expansion(add = 0.5)) +
    theme_gla(gridlines = "none", legend = "none", axis_titles = "none") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = 1, size = px_pt(13)),
                   axis.line = ggplot2::element_blank())
}

# TRUE where a colour is light enough to carry dark text
contrast_dark <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  lum <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
  lum > 0.55
}
