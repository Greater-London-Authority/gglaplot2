# Builds the demo datasets from the visualisation team's Storybook data, so
# the examples here are the same series the ldn-viz-tools charts are
# demonstrated with. Fetched from the public repository and parsed as text;
# nothing is typed by hand.
#
#   Rscript data-raw/demo-data.R

base <- "https://raw.githubusercontent.com/Greater-London-Authority/ldn-viz-tools/main/packages/charts/src/data/"

parse_rows <- function(url, fields) {
  txt <- paste(readLines(url, warn = FALSE), collapse = "\n")
  re <- paste0(
    fields[1], ": '([^']+)',\\s*", fields[2], ": '([^']+)',\\s*", fields[3], ": ([0-9.]+)"
  )
  m <- regmatches(txt, gregexpr(re, txt, perl = TRUE))[[1]]
  parts <- regmatches(m, regexec(re, m, perl = TRUE))
  data.frame(
    a = vapply(parts, `[[`, character(1), 2),
    b = vapply(parts, `[[`, character(1), 3),
    c = as.numeric(vapply(parts, `[[`, character(1), 4)),
    stringsAsFactors = FALSE
  )
}

monthly <- parse_rows(paste0(base, "monthlyData.ts"), c("Month", "Variable", "Value"))
gla_monthly <- data.frame(
  month = as.Date(monthly$a),
  series = monthly$b,
  value = monthly$c,
  stringsAsFactors = FALSE
)

yearly <- parse_rows(paste0(base, "yearlyData.ts"), c("Year", "Variable", "Average"))
gla_yearly <- data.frame(
  year = as.integer(yearly$a),
  series = yearly$b,
  average = yearly$c,
  stringsAsFactors = FALSE
)

stopifnot(nrow(gla_monthly) == 333, nrow(gla_yearly) == 30)
usethis::use_data(gla_monthly, gla_yearly, overwrite = TRUE)
cat(sprintf("Wrote gla_monthly (%d rows) and gla_yearly (%d rows)\n", nrow(gla_monthly), nrow(gla_yearly)))
