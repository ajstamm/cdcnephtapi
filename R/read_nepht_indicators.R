# read indicator table; follows cntent table
read_nepht_indicators <- function(con) {
  base <- "https://ephtracking.cdc.gov:443/apigateway/api/v1"
  l <- list()
  for (i in 1:nrow(con)) {
    url <- paste(base, "indicators", con$con_id[i], sep = "/")
    t <- read_api_data(url)
    t$con_id <- con$con_id[i]
    l[[i]] <- t
  }
  d <- dplyr::bind_rows(l)
  d$ind_id <- d$id
  d$name <- trimws(d$name)
  d <- d[, c("con_id", "ind_id", "name")]
  return(d)
}
