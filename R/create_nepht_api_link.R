#' Create API link
#'
#' This function builds the API link based on user selections.
#'
#' @param measure    ID for the API measure
#' @param strat      ID for the API stratification
#' @param geolevel   ID for the selected geographic level, or "ALL"
#' @param geography  ID for the selected geography, or "ALL"
#' @param years      Selected years
#' @param smoothed   Whether data should be smoothed (1) or not (0)
#' @param mstrat     If selected, sub-stratification to download
#' @param mstrat_lvl If selected, sub-stratification levels to download
#' @param token      Your API token. If you don't have one, set this to NULL.
#'
# notes:
# geographic levels and geographies can equal ALL
# years are separated by commas
# for some reason, I can't get token and stratifications to work together

create_nepht_api_link <- function(measure, strat, geolevel = "ALL",
                            geography = "ALL", years = 2000,
                            smoothed = 0, mstrat = NULL,
                            mstrat_lvl = NULL, token = NULL) {
  base <- "https://ephtracking.cdc.gov:443/apigateway/api/v1/getCoreHolder"
  url <- paste(base, measure, strat, geolevel, geography,
               paste(years, collapse = ","), smoothed, 0, sep = "/")
  if (!is.null(mstrat)) {
    lvls <- paste(mstrat_lvl, collapse= ",")
    url <- paste0(url, "?", mstrat, "=", lvls)
  } else if (!is.null(token)) {
    url <- paste0(url, "?", token)
  }
  return(url)
}
