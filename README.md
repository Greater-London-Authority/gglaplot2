# gglaplot2

The GLA design tokens as R data, with a ggplot2 theme, palettes and scales
built on them. The successor to
[gglaplot](https://github.com/Greater-London-Authority/gglaplot).

The tokens under `inst/tokens/` are the City Intelligence Unit visualisation
team's package, vendored unchanged. The same files are vendored by the Rhino
dashboard template, so a static chart made here and a live chart on a
dashboard draw from one set of colours. This package is the way those tokens
reach anyone plotting in R.

## Install

The repository is private, so install with a GitHub token that can read it:

```r
# install.packages("pak")
pak::pak("Greater-London-Authority/gglaplot2")
```

## Use

```r
library(ggplot2)
library(gglaplot2)

ggplot(economics, aes(date, unemploy / 1000)) +
  geom_line(colour = gla_token("data-primary"), linewidth = 0.8) +
  labs(title = "Unemployment", subtitle = "Thousands, monthly", caption = "Source: FRED") +
  theme_gla()

ggplot(mpg, aes(displ, hwy, colour = drv)) +
  geom_point() +
  scale_colour_gla() +
  theme_gla()
```

- `theme_gla()`: the non-data ink from the `chart-*` tokens. One gridline
  axis by default, titles left-aligned to the plot edge, `theme = "dark"` for
  an inverse surface.
- `gla_pal()`: `categorical`, `sequential`, `diverging`, `sex` and `change`
  palettes. The categorical palette is safe to three series; design to three
  and fold categories rather than reach for a fourth colour.
- `scale_colour_gla()`, `scale_fill_gla()`, `scale_colour_gla_c()`,
  `scale_fill_gla_c()`: the palettes as scales. `type = "sex"` maps by level
  name so women and men never swap colours.
- `gla_token()`: any token by name, in either mode. `gla_tokens` is the
  whole table and `gla_brand` the twelve brand statics.

## Refreshing the tokens

When the visualisation team release a new token package, drop the new
`_colors.scss` and `_primitives.scss` into `inst/tokens/` and run:

```r
source("data-raw/tokens.R")
devtools::document()
```

That regenerates `gla_tokens` and `gla_brand`. Nothing in `R/` carries a
colour by hand, and a test checks the data against the vendored file, so a
refresh that was not regenerated fails the suite.

## Contributing

Development happens on feature branches off `dev`, merged by pull request.
Every pull request runs the linter and `R CMD check` with the tests. Every
push to `dev` runs the same, and a green run fast-forwards `main`, so `main`
is always a version that passed. Releases are tagged on `main` and recorded
in `NEWS.md`.

Do not push to `main` directly. If it ever gets ahead of `dev` the promotion
job refuses to force it and says so; merge `main` into `dev` and push again.

Before opening a pull request:

```r
lintr::lint_package()
devtools::document()
devtools::check()
```

## What is not here yet

gglaplot carried geoms (`ggla_line`, `ggla_highlight`, `ggla_donut`,
`ggla_horizbar`, `ggla_sf`), plotly helpers and example datasets. None of
those have been brought across. The package starts with the tokens, the
theme, the palettes and the scales, which is what every chart needs; the
rest comes in as someone needs it, against the tokens rather than the old
hand-kept colour tables.
