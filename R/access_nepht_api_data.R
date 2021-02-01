#' Access NEPHT API data
#' 
#' This function reads in your API token (if relevant), takes you through 
#' a series of dialogs to built your API filepath, and accesses the API 
#' to download your data as a data frame.
#' 
#' @param token Your API token. If you don't have one, set this to NULL.
#' 

access_nepht_api_data <- function(token = NULL) {
  # define api settings
  s <- identify_nepht_api_settings()
  # build api path
  api <- 
    create_nepht_api_link(
      measure = s$measure$id, 
      strat = s$stratification$id, 
      geolevel = s$geolevel$id, 
      geography = s$geography$id, 
      years = s$years$id, 
      smoothed = s$geolevel$is_smooth, 
      mstrat = s$stratification$type,
      mstrat_lvl = s$stratification$m_id, 
      token = token)
  # read api data
  d <- read_nepht_api(url = api) 
  return(d)
}
