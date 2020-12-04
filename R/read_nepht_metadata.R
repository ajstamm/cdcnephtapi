# need to add temporal table
read_nepht_metadata <- function(token = NULL, path = "data") {
  l <- list()
  # content area
  l$con <- read_nepht_contentareas()
  write.csv(con, paste(path, "content_pkg.csv", sep = "/"), 
            row.names = FALSE)
  # indicator
  l$ind <- read_nepht_indicators(con)
  write.csv(ind, paste(path, "indicator_pkg.csv", sep = "/"), 
            row.names = FALSE)
  # measure
  l$mes <- read_nepht_measures(con)
  write.csv(mes, paste(path, "measure_pkg.csv", sep = "/"), 
            row.names = FALSE)
  # geograhic levels
  l$geo <- read_nepht_geographiclevels(mes)
  write.csv(geo, paste(path, "geolevels_pkg.csv", sep = "/"), 
            row.names = FALSE)
  # geography
  # removed child and rollup details to reduce size; not used?
  gsf <- read_nepht_geographies(geo)
  write.csv(gsf, paste(path, "geography_pkg.csv", sep = "/"), 
            row.names = FALSE)
  # measure stratification
  l$msf <- read_nepht_measurestratification(geo) # n=4428
  write.csv(msf, paste(path, "measurestrat_pkg.csv", sep = "/"), 
            row.names = FALSE)
  l$str <- read_nepht_stratification(geo)
  write.csv(str, paste(path, "stratlevel_pkg.csv", sep = "/"), 
            row.names = FALSE)
  return(l)
}