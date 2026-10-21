
# gglaplot2 <img src="man/figures/logo.svg" align="right" height="34" alt="" />

Charts in the Greater London Authority’s house style, for ggplot2. The
successor to
[gglaplot](https://github.com/Greater-London-Authority/gglaplot).

The design tokens, the typeface and the chart conventions are the City
Intelligence Unit visualisation team’s, taken from their
[ldn-viz-tools](https://github.com/Greater-London-Authority/ldn-viz-tools)
package unchanged: the same tokens the Rhino dashboard template vendors,
so a static chart made here, a live chart on a dashboard and a chart on
one of the team’s sites read as one system. This package is the way
those decisions reach anyone plotting in R.

## Install

The repository is private, so install with a GitHub token that can read
it:

``` r
# install.packages("pak")
pak::pak("Greater-London-Authority/gglaplot2")
```

## Use

``` r
library(ggplot2)
library(gglaplot2)

ggplot(gla_monthly, aes(month, value, colour = series)) +
  geom_gla_line() +
  geom_gla_endpoint() +
  geom_gla_endlabel(aes(label = series)) +
  scale_x_date(expand = expansion(mult = c(0.02, 0.18))) +
  scale_y_continuous(breaks = gla_breaks(), labels = scales::label_comma()) +
  scale_colour_gla() +
  gla_labs(
    title = "All three variables have converged since 2020",
    subtitle = "London monthly estimated variable value (GBP), January 2015 to March 2024",
    source = "LDN Viz Tools demo data", byline = "GLA City Intelligence", x = NULL
  ) +
  theme_gla(legend = "none")
```

<img src="man/figures/README-example-1.png" alt="" width="100%" />

- `theme_gla()`: the non-data ink from the `chart-*` tokens and the type
  from the design system’s chart roles; with ggplot2 4 the geom defaults
  too. `theme = "dark"` for an inverse surface, `theme_gla_map()` for a
  map.
- `geom_gla_line()`, `geom_gla_dot()`, `geom_gla_marker()`,
  `geom_gla_col()`, `geom_gla_area()`, `geom_gla_endlabel()`,
  `gla_zero_line()`, `gla_reference_line()`, `annotate_gla_range()`,
  `geom_gla_sf()`: the marks the web charts draw, reading their colour
  from the theme.
- `scale_colour_gla()`, `scale_fill_gla()` and the continuous variants:
  the palettes as scales. The default reads the series names the way the
  web charts do, so London against a comparator is primary against
  context grey.
- `gla_labs()`: title, subtitle, byline, source and note.
  `gla_describe()` writes a text alternative from the plot.
- `gla_save()`: the web charts’ 880 by 440, doubled.
- `gla_token()`, `gla_pal()`, `gla_tokens`, `gla_brand`: the tokens
  themselves.

The site at <https://greater-london-authority.github.io/gglaplot2/> has
a page per chart family with the code, and the foundations behind them.

## Typeface

Inter is bundled and registered with systemfonts when the package loads,
so ragg, svglite, knitr and pkgdown draw it on any machine. The base
graphics devices only know installed fonts and fall back to the device’s
sans; see `?gla_font`.

## Refreshing the tokens

When the visualisation team release a new token package, drop the new
`_colors.scss` and `_primitives.scss` into `inst/tokens/` and run:

``` r
source("data-raw/tokens.R")
devtools::document()
```

Nothing in `R/` carries a colour by hand, and a test checks the data
against the vendored file, so a refresh that was not regenerated fails
the suite. Do the Rhino dashboard template from the same release.

## Contributing

Development happens on feature branches off `dev`, merged by pull
request. Every pull request runs the linter, a check that `man/` and
`NAMESPACE` are up to date, `R CMD check` with the tests, and a build of
the site. Every push to `dev` runs the same and a green run
fast-forwards `main`, so `main` is always a version that passed; a push
to `main` publishes the site. Releases are tagged on `main` and recorded
in `NEWS.md`.

Do not push to `main` directly. If it ever gets ahead of `dev` the
promotion job refuses to force it and says so; merge `main` into `dev`
and push again.

Before opening a pull request:

``` r
lintr::lint_package()
devtools::document()
devtools::test()
devtools::check()
```

The visual snapshots under `tests/testthat/_snaps/` are drawn with the
bundled Inter through svglite, so they match across machines; after a
change that moves pixels on purpose, run `testthat::snapshot_review()`.

## What is not here

gglaplot’s plotly helpers and its donut chart have not been brought
across. Interactive charts belong to the web packages; the rest of
gglaplot’s geoms are here under new names, against the tokens rather
than the old hand-kept colour tables.
