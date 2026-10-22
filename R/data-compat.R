# gglaplot's datasets and colour lists, kept so its examples and the
# scripts written from them still run. The colour lists are generated from
# the tokens by data-raw/compat.R; the datasets are the originals.

#' gglaplot's theme colours
#'
#' The named colour lists gglaplot's functions read (`background`,
#' `body text`, `light grid` and so on), each now the token that plays that
#' part: `background` is `chart-surface`, `light grid` is `chart-grid`,
#' `context data` is `data-context`. `gla_default` holds the light values
#' and `gla_inverse` the dark. New code reads [gla_token()].
#'
#' @format A named list of eleven hex colours.
#' @name gla_default
"gla_default"

#' @rdname gla_default
#' @format NULL
"gla_inverse"

#' gglaplot's colour names
#'
#' The thirty names gglaplot's palettes were built from (`blue_core`,
#' `purple_light`, `magenta_dark`, `pink_brand`), each now the token nearest
#' it: the core set is the `data-categorical-*` tokens, the light and dark
#' sets are the 500 and 800 steps of the palette ramps, the brand set the
#' brand statics. New code reads [gla_token()] and [gla_brand].
#'
#' @format A named list of thirty hex colours.
"gla_colours"

#' Average Attainment 8 scores, 2016/17
#'
#' Average score per pupil in each element, London and England. One of
#' gglaplot's example datasets.
#'
#' @format A data frame with 8 rows: `Region`, `Subject` and `Score`.
#' @source <https://data.london.gov.uk/>
"AvAtt"

#' EU referendum results by London borough
#'
#' The 23 June 2016 vote for each local authority in London, with the
#' borough boundary as an sf geometry column. One of gglaplot's example
#' datasets. Contains Ordnance Survey data, Crown copyright and database
#' right 2015.
#'
#' @format A data frame with 33 rows: `Area_Code`, `Area`, `Leave`,
#'   `Remain`, `PropLeave`, `CatLeave` and `geometry`.
#' @source <https://data.london.gov.uk/dataset/eu-referendum-results>
"EURef"

#' Early years achievement by ethnicity, 2017
#'
#' Percentage achieving at least the expected standard in the Early Years
#' Foundation Stage, London and England. One of gglaplot's example
#' datasets.
#'
#' @format A data frame with 12 rows: `Ethnicity`, `Region` and `eyfsp`.
#' @source <https://data.london.gov.uk/>
"EYFSP"

#' Gender pay gap, London and the UK
#'
#' The median total gender pay gap by year. One of gglaplot's example
#' datasets.
#'
#' @format A data frame with 42 rows: `Year` (Date), `location` and `GPG`.
#' @source <https://data.london.gov.uk/>
"LDNUK"

#' Internet use in London
#'
#' Adults who had and had not used the internet in the last three months.
#' One of gglaplot's example datasets.
#'
#' @format A data frame with 2 rows: `locat`, `usage` and `numUsers`.
#' @source <https://www.ons.gov.uk/>
"Internet"
