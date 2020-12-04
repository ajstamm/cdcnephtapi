# read geography table; follows geographic levels table
read_nepht_geographies <- function(geo) {
  base <- "https://ephtracking.cdc.gov:443/apigateway/api/v1"
  geo <- geo[order(geo$mes_id, geo$geo_id), ]
  l <- list()
  x <- 1
  for (i in 1:nrow(geo)) {
    url <- paste(base, "geography", geo$mes_id[i], 
                 geo$geo_id[i], "0", sep = "/") # 0 = non-rollup
    id <- paste(geo$mes_id[i], geo$geo_id[i], sep = ".")
    t <- read_nepht_api_data(url, id = id)
    if (!is.character(t)) {
      t$mes_id <- geo$mes_id[i]
      t$geo_id <- geo$geo_id[i]
      t$rollup <- 0
      l[[x]] <- t
      x <- x + 1
      rm(t)
    }
  }
  for (i in 1:nrow(geo)) {
    url <- paste(base, "geography", geo$mes_id[i], 
                 geo$geo_id[i], "1", sep = "/") # 1 = rollup
    id <- paste(geo$mes_id[i], geo$geo_id[i], sep = ".")
    t <- read_api_data(url, id = id)
    if(!is.character(t)) {
      t$mes_id <- geo$mes_id[i]
      t$geo_id <- geo$geo_id[i]
      t$rollup <- 1
      l[[x]] <- t
      x <- x + 1
      rm(t)
    }
  }
  d <- dplyr::bind_rows(l)
  d$cat_id <- d$parentGeographicId
  d$lvl_id <- d$childGeographicId
  d$cat_name <- trimws(d$parentName)
  d$lvl_name <- trimws(d$childName)
  d <- d[, c("mes_id", "geo_id", "cat_id", "cat_name")]
  d <- dplyr::group_by(d, mes_id, geo_id, cat_id)
  d <- dplyr::slice(d, 1)
  return(d)
}
