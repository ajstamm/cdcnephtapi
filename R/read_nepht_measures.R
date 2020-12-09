# read measure table; follows indicator table
read_nepht_measures <- function(d, token = NULL) {
  base <- "https://ephtracking.cdc.gov:443/apigateway/api/v1"
  l <- list()
  for (i in 1:nrow(d)) {
    url <- paste(base, "measures", d$ind_id[i], sep = "/")
    if (!is.null(token)) {
      url <- paste0(url, "?apiToken=", token)
    }
    t <- read_nepht_api(url)
    t <- unique(t)
    t$ind_id <- d$ind_id[i]
    l[[i]] <- t
  }
  d <- dplyr::bind_rows(l)
  d$mes_id <- d$id
  d$name <- trimws(d$name)
  d <- d[!is.na(d$name), c("ind_id", "mes_id", "name")]
  return(d)
}
