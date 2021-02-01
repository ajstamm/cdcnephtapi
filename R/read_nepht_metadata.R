#' Read NEPHT API metadata
#'
#' This function reads in the definitions directly from the API.
#'
#' @param token Your API token. If you don't have one, set this to NULL.
#'
#' Since this function pings the API thousands of times, I recommend using
#' the embedded definitions files where possible and running this function
#' only after major content updates to the API (new years or indicators).
#'
#' This function will be run before each package update to ensure definitions
#' are current as of the package release date.
#'
#' This function does not retain all information from the tables to save on
#' package size. It does retain all information relevant to the other package
#' functions except some geographic stratification information relevant to the
#' Geography definitions, which were removed to reduce package size and were
#' not found to be necessary in testing. Should you need the additional
#' Geography information run read_nepht_geography() and replace the file
#' "geography_pkg.csv" in the package install folder.
#'

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
