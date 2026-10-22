# gglaplot's example datasets, carried over as they were so its examples
# still run. EURef's geometry column is re-made so its coordinate reference
# system is in the form current sf reads without a warning.
#
#   Rscript data-raw/gglaplot-data.R <path to a gglaplot checkout>

src <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(src)) stop("Give the path to a gglaplot checkout")
env <- new.env()
for (d in c("AvAtt", "EURef", "EYFSP", "Internet", "LDNUK")) {
  load(file.path(src, "data", paste0(d, ".rda")), envir = env)
}
# the old object carries a proj4 string with no EPSG code; the coordinates
# are British National Grid eastings and northings
geometry <- sf::st_geometry(env$EURef$geometry)
attr(geometry, "crs") <- NULL
stopifnot(all(sf::st_coordinates(geometry)[, 1] > 1000))
env$EURef$geometry <- sf::st_sfc(geometry, crs = 27700)
AvAtt <- env$AvAtt
EURef <- env$EURef
EYFSP <- env$EYFSP
Internet <- env$Internet
LDNUK <- env$LDNUK
usethis::use_data(AvAtt, EURef, EYFSP, Internet, LDNUK, overwrite = TRUE)
cat("Wrote gglaplot's five example datasets\n")
