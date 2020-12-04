# read stratification table; follows geographic levels table
# to identify missings in measure stratification
read_nepht_stratification <- function(geo) {
  base <- "https://ephtracking.cdc.gov:443/apigateway/api/v1"
  geo <- geo[order(geo$mes_id, geo$geo_id), ]
  l <- list()
  x <- 1
  for (i in 1:nrow(geo)) {
    url <- paste(base, "stratificationlevel", geo$mes_id[i], 
                 geo$geo_id[i], geo$smooth[i], sep = "/")
    id <- paste(geo$mes_id[i], geo$geo_id[i], sep = ".")
    t <- read_api_data(url, id = id)
    if (!is.character(t)) {
      t$cat_id <- t$id
      t$cat_name <- trimws(t$name)
      t <- t[, c("cat_id", "cat_name", "stratificationType")]
      t$mes_id <- geo$mes_id[i]
      t$geo_id <- geo$geo_id[i]
      t$smooth <- geo$smooth[i]
      l[[x]] <- t
      x <- x + 1
      rm(t)
    }
    # print(paste("i =", i, "; mes_id =", geo$mes_id[i], 
    #       "; geo_id =", geo$geo_id[i]))
  }
  d <- dplyr::bind_rows(l) 
  e <- tidyr::unnest(d, stratificationType)
  f <- d[!d$cat_id %in% e$cat_id, 
         c("mes_id", "geo_id", "cat_id", "cat_name", "smooth")]
  d <- bind_rows(e, f)
  d$cat_lbl <- d$columnName
  d$lvl_id <- d$id
  d$lvl_name <- trimws(d$name)
  lbl <-  c("mes_id", "geo_id", "cat_id", "cat_name", "cat_lbl", 
            "lvl_name", "lvl_id", "smooth")
  d <- d[, lbl]
  d <- unique(d)
  return(d)
}
