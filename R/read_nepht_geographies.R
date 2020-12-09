# read geography table; follows geographic levels table
read_nepht_geographies <- function(d, token = NULL) {
  base <- "https://ephtracking.cdc.gov:443/apigateway/api/v1"
  d <- d[order(d$mes_id, d$geo_id), ]
  l <- list()
  x <- 1
  for (i in 1:nrow(d)) {
    url <- paste(base, "geography", d$mes_id[i],
                 d$geo_id[i], "0", sep = "/") # 0 = non-rollup
    if (!is.null(token)) {
      url <- paste0(url, "?apiToken=", token)
    }
    id <- paste(d$mes_id[i], d$geo_id[i], sep = ".")
    t <- read_nepht_api(url, id = id)
    if (!is.character(t)) {
      t$mes_id <- d$mes_id[i]
      t$geo_id <- d$geo_id[i]
      t$rollup <- 0
      l[[x]] <- t
      x <- x + 1
      rm(t)
    }
  }
  for (i in 1:nrow(d)) {
    url <- paste(base, "geography", d$mes_id[i],
                 d$geo_id[i], "1", sep = "/") # 1 = rollup
    if (!is.null(token)) {
      url <- paste0(url, "?apiToken=", token)
    }
    id <- paste(d$mes_id[i], d$geo_id[i], sep = ".")
    t <- read_api_data(url, id = id)
    if(!is.character(t)) {
      t$mes_id <- d$mes_id[i]
      t$geo_id <- d$geo_id[i]
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
