# Builds gla_boroughs, the London borough boundaries the map examples draw.
# The source is the ONS Open Geography local authority boundary file
# (December 2022, generalised), cut to the 33 London boroughs and simplified
# for a small-scale map, kept here as data-raw/london_boroughs.geojson.
# Contains OS data (c) Crown copyright and database right 2022, Open
# Government Licence v3.
#
#   Rscript data-raw/boroughs.R

gla_boroughs <- sf::st_read(file.path("data-raw", "london_boroughs.geojson"), quiet = TRUE)
gla_boroughs <- gla_boroughs[order(gla_boroughs$name), ]
names(gla_boroughs)[names(gla_boroughs) == "gss_code"] <- "code"
gla_boroughs <- gla_boroughs[, c("code", "name", "geometry")]
rownames(gla_boroughs) <- NULL
stopifnot(nrow(gla_boroughs) == 33, all(startsWith(gla_boroughs$code, "E09")))
usethis::use_data(gla_boroughs, overwrite = TRUE)
cat(sprintf("Wrote gla_boroughs: %d boroughs, %s\n", nrow(gla_boroughs), format(sf::st_crs(gla_boroughs)$input)))
