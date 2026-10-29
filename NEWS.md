# gglaplot2 0.2.0.9000

* Tokens refreshed to `@ldn-viz/themes` 9.1.0: 0 added, 0 removed, 3 changed.

* `gla_plotly()` now converts every chart family the articles draw as the
  static chart draws it. `ggplotly()` builds each layer without the plot's
  theme, so a mark with nothing mapped took ggplot2's old defaults: the
  colours, widths and dash patterns now come from the theme. The theme's
  legend and axis settings are kept, the facet strips are set in the
  typeface at the left of their panel, a shaded range drawn to the panel
  edge is drawn there rather than dropped, the end labels and the labels on
  reference lines are placed as annotations (the end labels dodged apart as
  the geom does), a forecast line is dashed from its projected point, and
  a chart on the dark theme converts to a dark surface.
* `geom_gla_treemap()` draws the web charts' treemap: tiles squarified
  largest first from the top left with a surface-colour gap, laid out
  group by group when `fill` is mapped, labels centred or top left and
  dropped from tiles too small for them. Drawn by the package, no new
  dependency.
* `gla_plotly()` makes an interactive chart from a ggplot and puts back
  what `ggplotly()` drops: the eyebrow and title, the footer with its
  labels in bold, and a stepped scale's bins as a legend labelled with
  their ranges, so an interactive choropleth has a key.
* The chart chrome as the web charts draw it: `gla_labs(eyebrow = )` puts
  a short label above the title in the eyebrow role, and the footer sets
  its `Source:` and `Note:` labels in bold. Both are theme elements,
  `element_gla_title()` and `element_gla_caption()`, drawn by the package
  with no new dependency; an `element_text()` added over them keeps them.
  `gla_type()` gains the `eyebrow` role, 14px medium.
* `scale_*_gla_b("sequential", steps = 5)` cuts five bins of one rounded
  width from a round number at or below the data, the contract the
  diverging scale already had.
* `geom_gla_paired()` draws the paired dot plot: a marker per date for
  each category and an arrow between them in `data-context` that stops
  short of the markers. `facet_gla_rows()` is the vertically faceted
  chart, one full panel per level in a column with the axes on every
  panel. Each has an article.
* `gla_download_button()` and `gla_download()` put a save-as-SVG (or PNG)
  button in a Shiny app, writing through `gla_save()` under a name slugged
  from the chart's title. An SVG from `gla_save()` now declares Inter as a
  web font from the Fontsource CDN, so it reads in the typeface on a
  machine without it; `web_fonts = FALSE` leaves that out.
* `ggplotly()` now takes the maps: `geom_gla_sf()` kept its coordinate
  datum off, which left plotly nothing to build axes from, so every map
  failed to convert. `theme_gla_map()` now blanks the axis text and lines
  in full. The paired dot plot converts too, without its arrowheads.

# gglaplot2 0.2.0

Stepped scales and a range legend for maps, the house label functions,
dark mode in one switch, forecasts, grouped bars, and the release and
token jobs.

* The stepped scales' legend is now a swatch per bin labelled with its
  range, `-6% to -4%`, highest bin at the top, through `guide_gla_bins()`.
  The ends are always labelled, and the scale's `labels` function shapes
  both ends of each range, so `labels = label_gla_percent(scale = 1)` puts
  the sign on. `guide = "coloursteps"` brings ggplot2's bar back.
* `scale_*_gla_b("diverging", steps = 3)` fixes three bins each side of
  the midpoint, or `steps = c(2, 4)` for a different count each side. Each
  side takes its own rounded step width, and with `limits` the steps
  divide them exactly, so the count holds as the data changes where
  `breaks` would need resetting.
* Releases are tagged by the promotion: a pull request that sets a release
  version in `DESCRIPTION` and heads `NEWS.md` with it tags `main` as
  `v<version>` when it lands, makes the GitHub release from that section
  of the notes and opens the pull request that starts the next development
  version. `v0.1.0` marks the first full build. The gglaplot names stay
  for the whole 0.x series; the last 0.x release warns on every call and
  1.0 removes them.
* Labels in the house shapes: `label_gla_number()`, `label_gla_percent()`,
  `label_gla_pounds()` and `label_gla_date()`, for a scale's `labels`
  argument. A comma every three digits, `£` against the number, whole
  percentages from fractions, dates written out as `5 March 2024`, and
  `short = TRUE` to cut a thousand and up to `k`, `m` and `bn`. Thin
  wrappers over scales, which now needs 1.3.0 or later.
* The ramp scales, `scale_*_gla_c()` and `scale_*_gla_b()`, draw their
  guide vertical on the right whatever the theme's legend position. Under
  `theme_gla()` a ramp sat horizontal above the chart, in the place meant
  for series keys. `legend = "none"` still hides it and a `guide` passed to
  the scale wins.
* Diverging scales count out from zero. `scale_fill_gla_b()` and
  `scale_colour_gla_b()` are stepped scales for maps. The diverging one
  makes zero a break, gives each side as many steps as it has bins, and
  cuts both sides from one ramp, so a change running from -80 to +5 shows
  four reds and one light blue rather than a dark blue stretched over the
  rises; set `limits` to hold an outlier to the end step. The sequential
  one cuts the blue ramp into steps. The continuous
  `scale_*_gla_c("diverging")` now puts its neutral at `midpoint` (zero by
  default) instead of at the middle of the data. `gla_pal("diverging", n)`
  builds the steps from the 400 to 800 levels of the red and blue ramps,
  with `n = c(down, up)` for an uneven pair.
* One switch for dark mode: `gla_mode("dark")` sets it for the session and
  `with_gla_mode()` for a block of code. Every function that takes a
  `theme` argument now defaults to the mode, so a dark chart no longer
  needs `theme = "dark"` on the theme, the scales, the reference line and
  the range separately. An explicit `theme` still wins for that call.
* Forecasts: `geom_gla_line(projected_from = )` draws each line solid to
  the point and dashed from it, and `annotate_gla_projection()` adds the
  web charts' wash, rule and "Projected" label over the period.
* Line styles: `scale_linetype_gla()` and `gla_linetypes()`, the web
  charts' solid-then-dashed set for a second dimension colour cannot
  carry, such as a measure against its average.
* The design system's palette checks, computed in R: `gla_check_palette()`
  (lightness band, chroma floor, separation under simulated colour vision
  deficiency and under normal vision, contrast on the surface) and
  `gla_check_ramp()` for an ordered ramp. The tests hold the package's
  palettes to them with the current reports as the baseline, so a token
  release that moves a colour out of bounds fails the suite.
* Grouped bars: `geom_gla_col(position = "dodge")` and `geom_gla_bar()` now
  lay the bars out as the web charts' grouped bar charts do, through
  `position_gla_dodge()`: a wide group, close bars, a clear gap before the
  next category, and a missing bar leaving the others at the same width.
  Works with the category on `y`. The bar charts article shows both.

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
* The categorical palette is the web charts' `getColorScale` order:
  primary, secondary, tertiary, then turquoise, purple, red, green and
  yellow. `gla_max_series()` is the design ceiling, three.
* Everything gglaplot exported runs here under its old name: `ggla_line()`,
  `ggla_highlight()`, `ggla_labelline()`, `ggla_axisat0()`,
  `ggla_highlightarea()`, `ggla_horizbar()`, `ggla_sf()`, `ggla_donut()`,
  `show_gla_pals()`, `left_align_labs()`, the `GeomGLA*` objects,
  `mm_to_pt`, the plotly helpers, and `theme_gla()` and `gla_pal()` with
  their old arguments. The colour lists `gla_default`, `gla_inverse` and
  `gla_colours` are generated from the tokens; the example datasets are
  the originals. Each old name says once a session what it is now; the
  Migrating from gglaplot article lists them.
* `geom_gla_text()`, annotation text in the house style, and
  `gla_reference_line(x = )` for a vertical rule.
* `plotly::ggplotly()` converts the `geom_gla_*()` marks.
* The tokens are vendored from the `@ldn-viz/themes` npm release, recorded
  in `inst/tokens/VERSION` and read by `gla_token_version()`.
  `data-raw/refresh-tokens.R` fetches a release, rebuilds everything from
  it and prints what changed; it and the tests fail with the token's name
  if a release drops or renames one the package reads.

# gglaplot2 0.0.0.9000

* First cut. The GLA design tokens as `gla_tokens` and `gla_brand`, generated
  from the visualisation team's vendored package; `gla_token()`; `gla_pal()`
  with categorical, sequential, diverging, sex and change palettes;
  `scale_colour_gla()`, `scale_fill_gla()` and their continuous variants;
  `theme_gla()` built on the `chart-*` tokens, with light and dark modes.
