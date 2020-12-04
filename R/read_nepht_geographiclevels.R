# read geographic levels table; follows measure table
read_nepht_geographiclevels <- function(mes) {
  base <- "https://ephtracking.cdc.gov:443/apigateway/api/v1"
  mes <- mes[order(mes$mes_id), ]
  l <- list()
  x <- 1
  for (i in 1:nrow(mes)) {
    url <- paste(base, "geographiclevels", mes$mes_id[i], sep = "/")
    t <- read_api_data(url)
    if(!is.character(t)) {
      t <- unique(t)
      t$mes_id <- mes$mes_id[i]
      l[[x]] <- t
      x <- x + 1
      rm(t)
    }
  }
  d <- dplyr::bind_rows(l) 
  d$geo_id <- d$geographicTypeId
  d$smooth <- as.numeric(d$smoothingLevelId==3)
  d$name <- trimws(d$geographicType)
  d <- d[!is.na(d$name) | !is.na(d$mes_id), 
         c("mes_id", "geo_id", "name", "smooth")]
  return(d)
}
