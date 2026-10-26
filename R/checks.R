# The palette checks the visualisation work holds a categorical palette to,
# computed rather than eyeballed: the lightness band, the chroma floor,
# separation under simulated colour vision deficiency, separation under
# normal vision, and contrast against the surface. A ramp has its own set.
# The maths follows the design system's validator so the two agree.

#' Check a palette the way the design system does
#'
#' Five checks on a categorical palette, each measurable from the colours
#' alone. Lightness: every colour's OKLCH lightness sits in the mode's
#' band (0.43 to 0.77 on light, 0.48 to 0.67 on dark). Chroma: every
#' colour's chroma is at least 0.10, below which a hue reads as grey.
#' Colour vision deficiency: the worst adjacent pair, under simulated
#' protanopia and deuteranopia, is at least 8 OKLab units apart (6 to 8 is a
#' warning, legal only with direct labels, gaps or texture). Normal vision:
#' the worst adjacent pair is at least 15 apart unsimulated. Contrast: each
#' colour reaches 3:1 against the surface, or is flagged as needing labels
#' or a table. A fail on the first four is a fail; the contrast check warns.
#'
#' `gla_check_ramp()` is the set for an ordered ramp, which the categorical
#' checks would fail by design: lightness monotone along the ramp, each step
#' at least 0.06 lighter or darker than its neighbour, the palest step at
#' 2:1 against the surface, and one hue (a spread of 40 degrees or less).
#'
#' The tests run these on the package's palettes, so a token release that
#' moves a colour out of bounds fails the suite.
#'
#' @param colours Hex colours, in palette order.
#' @param theme `"light"` or `"dark"`, which sets the lightness band and the
#'   default surface.
#' @param surface The surface the colours sit on. Defaults to the mode's
#'   `chart-surface` token.
#' @param pairs `"adjacent"` compares neighbours, as for bars, lines and
#'   stacks; `"all"` compares every pair, for a scatter, a map or small
#'   multiples where any two colours can meet.
#' @return A data frame with one row per check: `check`, `result` (`"pass"`,
#'   `"warn"` or `"fail"`) and `detail`, with an attribute `ok` that is
#'   `TRUE` when nothing failed.
#' @examples
#' gla_check_palette(gla_pal(n = 3))
#' gla_check_palette(gla_pal(), pairs = "all")
#' gla_check_ramp(gla_pal("sequential", 5))
#' @name gla_check_palette
NULL

#' @rdname gla_check_palette
#' @export
gla_check_palette <- function(colours, theme = c("light", "dark"), surface = NULL,
                              pairs = c("adjacent", "all")) {
  theme <- match.arg(theme)
  pairs <- match.arg(pairs)
  colours <- check_hex(colours)
  surface <- check_hex(surface %||% unname(gla_token("chart-surface", theme)))
  band <- if (theme == "light") c(0.43, 0.77) else c(0.48, 0.67)
  n <- length(colours)
  lch <- t(vapply(colours, oklch, numeric(2)))

  off <- which(lch[, 1] < band[1] | lch[, 1] > band[2])
  low_c <- which(lch[, 2] < 0.10)
  idx <- if (pairs == "all") {
    utils::combn(n, 2, simplify = FALSE)
  } else {
    lapply(seq_len(max(n - 1, 0)), function(i) c(i, i + 1))
  }
  pair_label <- function(p) paste0(colours[p[1]], " and ", colours[p[2]])
  listed <- function(i, values, digits) {
    paste(sprintf(paste0("%s (%.", digits, "f)"), colours[i], values), collapse = ", ")
  }

  cvd <- lapply(idx, function(p) {
    c(protan = delta_e(colours[p[1]], colours[p[2]], "protan"),
      deutan = delta_e(colours[p[1]], colours[p[2]], "deutan"),
      tritan = delta_e(colours[p[1]], colours[p[2]], "tritan"))
  })
  pd <- vapply(cvd, function(x) min(x[["protan"]], x[["deutan"]]), 0)
  worst_pd <- if (length(pd)) min(pd) else 99
  which_pd <- if (length(pd)) which.min(pd) else NA
  tritan <- if (length(cvd)) min(vapply(cvd, `[[`, 0, "tritan")) else 99
  normal <- vapply(idx, function(p) delta_e(colours[p[1]], colours[p[2]]), 0)
  worst_n <- if (length(normal)) min(normal) else 99
  contrast <- vapply(colours, wcag_contrast, 0, surface)
  low_contrast <- which(contrast < 3)

  state <- function(ok) if (ok) "pass" else "fail"
  lightness <- if (length(off)) {
    sprintf("outside %.2f to %.2f: %s", band[1], band[2], listed(off, lch[off, 1], 3))
  } else {
    sprintf("all %d inside %.2f to %.2f", n, band[1], band[2])
  }
  chroma <- if (length(low_c)) {
    sprintf("below 0.10, reads grey: %s", listed(low_c, lch[low_c, 2], 3))
  } else {
    sprintf("all %d at or above 0.10", n)
  }
  cvd_state <- if (worst_pd >= 8) "pass" else if (worst_pd >= 6) "warn" else "fail"
  cvd_detail <- if (is.na(which_pd)) {
    "one colour"
  } else {
    kind <- names(which.min(cvd[[which_pd]][c("protan", "deutan")]))
    sprintf("worst %s pair %s, %.1f (%s); tritan %.1f", pairs, pair_label(idx[[which_pd]]),
            worst_pd, kind, tritan)
  }
  normal_detail <- if (length(normal)) {
    sprintf("worst %s pair %s, %.1f", pairs, pair_label(idx[[which.min(normal)]]), worst_n)
  } else {
    "one colour"
  }
  contrast_detail <- if (length(low_contrast)) {
    sprintf("below 3:1 on %s, needs labels or a table: %s", surface,
            listed(low_contrast, contrast[low_contrast], 2))
  } else {
    sprintf("all %d at or above 3:1 on %s", n, surface)
  }
  rows <- list(
    c("Lightness band", state(!length(off)), lightness),
    c("Chroma floor", state(!length(low_c)), chroma),
    c("CVD separation", cvd_state, cvd_detail),
    c("Normal-vision floor", state(worst_n >= 15), normal_detail),
    c("Contrast on surface", if (length(low_contrast)) "warn" else "pass", contrast_detail)
  )
  check_report(rows)
}

#' @rdname gla_check_palette
#' @export
gla_check_ramp <- function(colours, theme = c("light", "dark"), surface = NULL) {
  theme <- match.arg(theme)
  colours <- check_hex(colours)
  surface <- check_hex(surface %||% unname(gla_token("chart-surface", theme)))
  l <- vapply(colours, function(x) oklch(x)[1], 0)
  ord <- order(l)
  mono <- all(ord == seq_along(l)) || all(ord == rev(seq_along(l)))
  gaps <- abs(diff(l))
  thin <- which(gaps < 0.06)
  palest <- if (theme == "light") colours[which.max(l)] else colours[which.min(l)]
  pale_contrast <- wcag_contrast(palest, surface)
  hues <- vapply(colours, okhue, 0)
  spread <- if (length(hues)) max(hues) - min(hues) else 0
  if (spread > 180) spread <- 360 - spread
  state <- function(ok) if (ok) "pass" else "fail"
  mono_detail <- if (mono) {
    "steps run light to dark"
  } else {
    sprintf("out of order: %s", paste(sprintf("%.3f", l), collapse = ", "))
  }
  gap_detail <- if (length(thin)) {
    steps <- sprintf("%s to %s (%.3f)", colours[thin], colours[thin + 1], gaps[thin])
    sprintf("steps closer than 0.06: %s", paste(steps, collapse = ", "))
  } else {
    "every step at least 0.06 apart"
  }
  rows <- list(
    c("Lightness monotone", state(mono), mono_detail),
    c("Step gap", state(!length(thin)), gap_detail),
    c("Palest step contrast", state(pale_contrast >= 2),
      sprintf("%s at %.2f:1 on %s", palest, pale_contrast, surface)),
    c("Single hue", state(spread <= 40), sprintf("hue spread %.0f degrees", spread))
  )
  check_report(rows)
}

check_report <- function(rows) {
  out <- data.frame(
    check = vapply(rows, `[[`, "", 1), result = vapply(rows, `[[`, "", 2),
    detail = vapply(rows, `[[`, "", 3), stringsAsFactors = FALSE
  )
  attr(out, "ok") <- !any(out$result == "fail")
  out
}

check_hex <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- ifelse(startsWith(x, "#"), x, paste0("#", x))
  bad <- x[!grepl("^#[0-9a-f]{6}$", x)]
  if (length(bad)) stop("Not six-digit hex colours: ", paste(bad, collapse = ", "), call. = FALSE)
  unname(x)
}

# ── colour maths, as the design system's validator does it ────────────────

hex_linear <- function(hex) {
  s <- grDevices::col2rgb(hex)[, 1] / 255
  ifelse(s <= 0.04045, s / 12.92, ((s + 0.055) / 1.055)^2.4)
}

oklab_from_linear <- function(rgb) {
  l <- (0.4122214708 * rgb[1] + 0.5363325363 * rgb[2] + 0.0514459929 * rgb[3])^(1 / 3)
  m <- (0.2119034982 * rgb[1] + 0.6806995451 * rgb[2] + 0.1073969566 * rgb[3])^(1 / 3)
  s <- (0.0883024619 * rgb[1] + 0.2817188376 * rgb[2] + 0.6299787005 * rgb[3])^(1 / 3)
  c(
    0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s,
    1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s,
    0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s
  )
}

oklch <- function(hex) {
  lab <- oklab_from_linear(hex_linear(hex))
  c(lab[1], sqrt(lab[2]^2 + lab[3]^2))
}

okhue <- function(hex) {
  lab <- oklab_from_linear(hex_linear(hex))
  (atan2(lab[3], lab[2]) * 180 / pi) %% 360
}

# Machado, Oliveira and Fernandes (2009) at severity 1.0, on linear RGB
machado <- list(
  protan = matrix(c(0.152286, 1.052583, -0.204868, 0.114503, 0.786281, 0.099216,
                    -0.003882, -0.048116, 1.051998), 3, byrow = TRUE),
  deutan = matrix(c(0.367322, 0.860646, -0.227968, 0.280085, 0.672501, 0.047413,
                    -0.011820, 0.042940, 0.968881), 3, byrow = TRUE),
  tritan = matrix(c(1.255528, -0.076749, -0.178779, -0.078411, 0.930809, 0.147602,
                    0.004733, 0.691367, 0.303900), 3, byrow = TRUE)
)

simulate_cvd <- function(rgb, kind) {
  pmin(pmax(as.numeric(machado[[kind]] %*% rgb), 0), 1)
}

# Euclidean distance in OKLab, times 100; `kind` NULL for normal vision
delta_e <- function(a, b, kind = NULL) {
  la <- hex_linear(a)
  lb <- hex_linear(b)
  if (!is.null(kind)) {
    la <- simulate_cvd(la, kind)
    lb <- simulate_cvd(lb, kind)
  }
  100 * sqrt(sum((oklab_from_linear(la) - oklab_from_linear(lb))^2))
}

wcag_contrast <- function(a, b) {
  lum <- function(hex) sum(c(0.2126, 0.7152, 0.0722) * hex_linear(hex))
  l <- sort(c(lum(a), lum(b)), decreasing = TRUE)
  (l[1] + 0.05) / (l[2] + 0.05)
}
