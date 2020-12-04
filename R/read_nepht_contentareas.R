# read content table
read_nepht_contentareas <- function(token = NULL) {
  base <- "https://ephtracking.cdc.gov:443/apigateway/api/v1"
  url <- paste(base, "contentareas/json", sep = "/")
  d <- read_nepht_api_data(url)
  d$name <- trimws(d$name)
  d$con_id <- d$id
  d <- d[, c("con_id", "name")]
  return(d)
}
