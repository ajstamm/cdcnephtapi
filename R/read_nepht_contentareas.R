# read content table
read_nepht_contentareas <- function(token = NULL) {
  base <- "https://ephtracking.cdc.gov:443/apigateway/api/v1"
  url <- paste(base, "contentareas/json", sep = "/")
  if (!is.null(token)) {
    url <- paste0(url, "?apiToken=", token)
  }
  d <- read_nepht_api(url)
  d$name <- trimws(d$name)
  d$con_id <- d$id
  d <- d[, c("con_id", "name")]
  return(d)
}
