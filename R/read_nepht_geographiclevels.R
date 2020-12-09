# read geographic levels table; follows measure table
read_nepht_geographiclevels <- function(d, token = NULL) {
  base <- "https://ephtracking.cdc.gov:443/apigateway/api/v1"
  d <- d[order(d$mes_id), ]
  l <- list()
  x <- 1
  for (i in 1:nrow(d)) {
    url <- paste(base, "geographiclevels", d$mes_id[i], sep = "/")
    if (!is.null(token)) {
      url <- paste0(url, "?apiToken=", token)
    }
    t <- read_nepht_api(url)
    if(!is.character(t)) {
      t <- unique(t)
      t$mes_id <- d$mes_id[i]
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
