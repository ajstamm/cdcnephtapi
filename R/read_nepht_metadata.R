# need to add temporal table
read_nepht_metadata <- function(token = NULL, path = "data") {
  l <- list()
  # content area
  l$con <- read_nepht_contentareas(token = token)
  write.csv(l$con, paste(path, "content_pkg.csv", sep = "/"),
            row.names = FALSE)
  # indicator
  l$ind <- read_nepht_indicators(l$con, token = token)
  write.csv(l$ind, paste(path, "indicator_pkg.csv", sep = "/"),
            row.names = FALSE)
  # measure
  l$mes <- read_nepht_measures(l$con, token = token)
  write.csv(l$mes, paste(path, "measure_pkg.csv", sep = "/"),
            row.names = FALSE)
  # geograhic levels
  l$geo <- read_nepht_geographiclevels(l$mes, token = token)
  write.csv(l$geo, paste(path, "geolevels_pkg.csv", sep = "/"),
            row.names = FALSE)
  # geography
  # removed child and rollup details to reduce size; not used?
  gsf <- read_nepht_geographies(l$geo, token = token)
  write.csv(l$gsf, paste(path, "geography_pkg.csv", sep = "/"),
            row.names = FALSE)
  # measure stratification
  l$msf <- read_nepht_measurestratification(l$geo, token = token) # n=4428
  write.csv(l$msf, paste(path, "measurestrat_pkg.csv", sep = "/"),
            row.names = FALSE)
  l$str <- read_nepht_stratification(l$geo, token = token)
  write.csv(l$str, paste(path, "stratlevel_pkg.csv", sep = "/"),
            row.names = FALSE)
  # temporal
  l$tem <- read_nepht_temporal(l$geo, token = token)
  write.csv(l$tem, paste(path, "temporal_pkg.csv", sep = "/"),
            row.names = FALSE)

  return(l)
}
