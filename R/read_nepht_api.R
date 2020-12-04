# read the url and isolates the data
# returns the only data frame in the list
read_nepht_api <- function(url, id = 0) {
  res = httr::GET(url)
  js <- jsonlite::fromJSON(httr::content(res, as="text",
                                         encoding = "UTF-8"), flatten = T)
  if (length(js) > 0) {
    if (class(js) == "list") {
      x <- c()
      for (i in 1:length(js)) {
        x <- c(x, class(js[[i]]))
      }
      y <- 1:length(js)
      index <- y[x=="data.frame"]
      if (length(index) > 0) {
        d <- js[[index]]
        return(d)
      }
    }
  }
  print(paste(id, ": No table produced."))
}
