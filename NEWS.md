# gglaplot2 0.1.0

The first full build, on the visualisation team's ldn-viz-tools conventions.

* `theme_gla()` now sets its type from the design system's chart roles
  (title 20px semibold, subtitle 16, ticks 14, caption 12) and, with
  ggplot2 4, the geom defaults too: a 2px `data-primary` line, a 1px
  `chart-surface` edge on bars, `chart-label` annotation text. The y axis
  title is off by default as on the web charts; `axis_titles` turns it on.
  `theme_gla_map()` for maps; `gla_breaks()` for four gridlines.
* Inter, the design system's typeface, is bundled and registered with
  systemfonts on load, so ragg, svglite, knitr and pkgdown draw it on any
  machine. `gla_font()` says when it is used.
* The marks the web charts draw: `geom_gla_line()`, `geom_gla_dot()`,
  `geom_gla_marker()`, `geom_gla_col()`, `geom_gla_bar()`, `geom_gla_area()`,
  `geom_gla_endpoint()` and `geom_gla_endlabel()` (end-of-line labels that
  push apart when they overlap), `gla_zero_line()`, `gla_reference_line()`,
  `annotate_gla_range()`, `geom_gla_sf()`. They read their colour from the
  theme, so `theme_gla(theme = "dark")` recolours them.
* `scale_colour_gla()` defaults to `type = "auto"`: London against a
  comparator is primary against context grey, Domestic/International and
  Inner/Outer are primary against secondary, anything else the categorical
  palette in order. `gla_series_colours()` is the rule on its own.
* `gla_labs()` and `gla_caption()` carry the title, subtitle, byline,
  source and note the web chart container shows. `gla_describe()` writes
  a text alternative from the plot and `gla_alt()` sets it.
* `gla_save()` saves at the web charts' 880 by 440, doubled.
* `gla_show_pal()` and `gla_show_tokens()` draw swatches.
* Demo data from the Storybook (`gla_monthly`, `gla_yearly`) and the London
  boroughs (`gla_boroughs`).
* A pkgdown site structured like the Storybook: Foundations (tokens,
  typography, palettes) and a page per chart family, styled from the
  tokens. Built on pull requests and published from main.

# gglaplot2 0.0.0.9000

* First cut. The GLA design tokens as `gla_tokens` and `gla_brand`, generated
  from the visualisation team's vendored package; `gla_token()`; `gla_pal()`
  with categorical, sequential, diverging, sex and change palettes;
  `scale_colour_gla()`, `scale_fill_gla()` and their continuous variants;
  `theme_gla()` built on the `chart-*` tokens, with light and dark modes.
