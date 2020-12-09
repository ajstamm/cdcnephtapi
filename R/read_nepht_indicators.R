# read indicator table; follows cntent table
read_nepht_indicators <- function(d, token = NULL) {
  base <- "https://ephtracking.cdc.gov:443/apigateway/api/v1"
  l <- list()
  for (i in 1:nrow(d)) {
    url <- paste(base, "indicators", d$con_id[i], sep = "/")
    if (!is.null(token)) {
      url <- paste0(url, "?apiToken=", token)
    }
    t <- read_nepht_api(url)
    t$con_id <- d$con_id[i]
    l[[i]] <- t
  }
  d <- dplyr::bind_rows(l)
  d$ind_id <- d$id
  d$name <- trimws(d$name)
  d <- d[, c("con_id", "ind_id", "name")]
  return(d)
}
