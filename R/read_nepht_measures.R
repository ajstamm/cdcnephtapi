# read measure table; follows indicator table
read_nepht_measures <- function(ind) {
  base <- "https://ephtracking.cdc.gov:443/apigateway/api/v1"
  l <- list()
  for (i in 1:nrow(ind)) {
    url <- paste(base, "measures", ind$ind_id[i], sep = "/")
    t <- read_api_data(url) 
    t <- unique(t)
    t$ind_id <- ind$ind_id[i]
    l[[i]] <- t
  }
  d <- dplyr::bind_rows(l) 
  d$mes_id <- d$id
  d$name <- trimws(d$name)
  d <- d[!is.na(d$name), c("ind_id", "mes_id", "name")]
  return(d)
}
