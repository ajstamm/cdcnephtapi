# read temporal table; follows geographic levels table
read_nepht_temporal <- function(d) {
  base <- "https://ephtracking.cdc.gov:443/apigateway/api/v1"
  d <- d[order(d$mes_id, d$geo_id), ]
  d <- d[!is.na(d$mes_id), ]
  l <- list()
  x <- 1
  for (i in 1:nrow(d)) {
    url <- paste(base, "temporal", d$mes_id[i], d$geo_id[i], 
                 "ALL", "ALL", sep = "/")
    # 
    id <- paste(d$mes_id[i], d$geo_id[i], sep = ".")
    t <- read_nepht_api(url, id = id)
    if (!is.character(t)) {
      t$mes_id <- d$mes_id[i]
      t$geo_id <- d$geo_id[i]
      l[[x]] <- t
      x <- x + 1
      rm(t)
    }
  }
  d <- dplyr::bind_rows(l) 
  d$cat_id <- d$parentTemporalId
  d$lvl_id <- d$childTemporalId
  d$cat_name <- trimws(d$parentTemporalType)
  d$lvl_name <- trimws(d$childTemporalType)
  d$cat_val <- d$parentTemporal
  d$lvl_val <- d$childTemporal
  d <- d[, c("mes_id", "geo_id", "cat_id", "cat_name", "cat_val", 
             "lvl_id", "lvl_name", "lvl_val")]
  return(d)
}
