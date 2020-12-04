# read measure stratification table; follows geographic levels table
read_nepht_measurestratification <- function(geo) {
  base <- "https://ephtracking.cdc.gov:443/apigateway/api/v1"
  geo <- geo[order(geo$mes_id, geo$geo_id), ]
  l <- list()
  x <- 1
  for (i in 1:nrow(geo)) {
    url <- paste(base, "measurestratification", geo$mes_id[i], 
                 geo$geo_id[i], geo$smooth[i], sep = "/")
    id <- paste(geo$mes_id[i], geo$geo_id[i], sep = ".")
    t <- read_api_data(url, id = id)
    if(!is.character(t)) {
      t$cat_id <- t$stratificationTypeId
      t$cat_name <- trimws(t$displayName)
      t$cat_lbl <- t$columnName
      t <- t[, c("cat_id", "cat_name", "cat_lbl", 
                 "stratificationItem")]
      t$mes_id <- geo$mes_id[i]
      t$geo_id <- geo$geo_id[i]
      t$smooth <- geo$smooth[i]
      l[[x]] <- t
      x <- x + 1
      rm(t)
    }
  }
  d <- dplyr::bind_rows(l) 
  e <- tidyr::unnest(d, stratificationItem)
  f <- d[!d$mes_id %in% e$mes_id, 
         c("mes_id", "geo_id", "cat_id", "cat_name", "cat_lbl", "smooth")]
  d <- bind_rows(e, f)
  d <- unique(d)
  d$lvl_id <- d$localId
  d$lvl_name <- trimws(d$name)
  d <- d[, c("mes_id", "geo_id", "cat_id", "cat_name", "cat_lbl", "lvl_id", 
             "lvl_name", "smooth")]
  return(d)
}
