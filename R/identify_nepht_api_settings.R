#' Identify NEPHT API settings
#'
#' This function takes you through a series of dialogs to select your
#' desired settings based on definitions read in from the API. To avoid
#' overwhelming the API, these definitions are embedded in the package and
#' are current as of the date of package compilation.
#'
#' @param path The filepath to read the definitions files, which will
#'             default to the package install when the package is complete.
#'
#' If you wish to update the definitions files, run read_nepht_metadata()
#' and use the same path setting for both that function and this one.
#'

# rewrite this to allow you to include your own filepath(s) for
# updated definitions.

# order:
# content area
# indicator
# measure
# geographic levels
# temporal, geography, and smoothing
# measure levels, stratification

# I would love to figure out how to make a window
# that updates when you press a button

# when building package, fix all data links
# include insructions for structure of definitions folder
# or instructions to run read_nepht_metadata()



identify_nepht_api_settings <- function(path = "data") {
  #---- content area ----
  # need id, name
  title <- "Content Area Selection"
  instr <- "Please select a content area."
  data <- read.csv(paste(path, "content_pkg.csv", sep = "/"),
                   stringsAsFactors = FALSE)

  lbl <- draw_nepht_api_list_window(title = title, instruction = instr,
                                    mylist = data$name)$myvar
  id <- data$con_id[data$name == lbl]
  content <- list(id = id, lbl = lbl)

  #---- indicator ----
  # need id, name, con_id
  title <- "Indicator Selection"
  instr <- paste0("Please select an indicator for \n",
                  "content: ", content$lbl, ".")
  data <- read.csv(paste(path, "indicator_pkg.csv", sep = "/"),
                   stringsAsFactors = FALSE)

  l <- data[data$con_id == content$id, ]

  lbl <- draw_nepht_api_list_window(title = title, instruction = instr,
                                    mylist = l$name)$myvar
  id <- l$ind_id[l$name == lbl]
  indicator <- list(id = id, lbl = lbl)

  #---- measure ----
  # need id, name, ind_id
  title <- "Measure Selection"
  instr <- paste0("Please select a measure for \n",
                  "content area: ", content$lbl, " \n",
                  "indicator: ", indicator$lbl, ".")
  data <- read.csv(paste(path, "measure_pkg.csv", sep = "/"),
                   stringsAsFactors = FALSE)

  l <- data[data$ind_id == indicator$id, ]

  lbl<- draw_nepht_api_list_window(title = title, instruction = instr,
                                   mylist = l$name)$myvar
  id <- l$mes_id[l$name == lbl]
  measure <- list(id = id, lbl = lbl)

  #---- geographic level ----
  # need id, name, mes_id, smooth
  title <- "Geographic Level Selection"
  instr <- paste0("Please select a geographic level for \n",
                  "content area: ", content$lbl, " \n",
                  "indicator: ", indicator$lbl, " \n",
                  "measure: ", measure$lbl, ".")
  data <- read.csv(paste(path, "geolevels_pkg.csv", sep = "/"),
                   stringsAsFactors = FALSE)

  l <- data[which(data$mes_id == measure$id), ]

  lbl <- draw_nepht_api_list_window(title = title, instruction = instr,
                                    mylist = l$name, all = TRUE)$myvar
  if (lbl == "All options") {
    id <- "ALL"
    is_smooth <- 0 # could be 1 sometimes; add new dialog?
  } else {
    id <- l$geo_id[l$name %in% lbl]
    is_smooth <- l$smooth[l$name == lbl]
  }

  geolevel <- list(id = id, lbl = lbl, is_smooth = is_smooth)

  #---- geography ----
  # need id, name, mes_id, geo_id
  title <- "Geography Selection"
  instr <- paste0("Please select a geography for \n",
                  "content area: ", content$lbl, "\n",
                  "indicator: ", indicator$lbl, "\n",
                  "measure: ", measure$lbl, "\n",
                  "geographic level: ", geolevel$lbl)
  data <- read.csv(paste(path, "geography_pkg.csv", sep = "/"),
                   stringsAsFactors = FALSE)

  if(geolevel$id == "All") {
    geo_id <- 2
  } else {
    geo_id <- geolevel$id
  }
  l <- data[data$mes_id == measure$id & data$geo_id == geo_id, ]

  lbl <- draw_nepht_api_list_window(title = title, instruction = instr,
                                    mylist = l$cat_name, all = TRUE)$myvar
  if (lbl == "All options") {
    id <- "ALL"
  } else {
    id <- l$cat_id[l$cat_name %in% lbl]
  }

  geography <- list(id = id, lbl = lbl)

  #---- stratification ----
  # need cat_id, lbl, lvl_id, lvl_name, geo_id, mes_id, smooth, cat_name
  # allow multiple selections in second step
  title <- "Stratification Selection"
  instr <- paste0("Please select a stratification level for \n",
                  "content area: ", content$lbl, "\n",
                  "indicator: ", indicator$lbl, "\n",
                  "measure: ", measure$lbl, "\n",
                  "geographic level: ", geolevel$lbl)
  data <- read.csv(paste(path, "stratlevel_pkg.csv", sep = "/"),
                   stringsAsFactors = FALSE)

  if(geolevel$id == "All") {
    geo_id <- 2
  } else {
    geo_id <- geolevel$id
  }
  l <- data[data$mes_id == measure$id & data$geo_id == geo_id &
              data$smooth %in% geolevel$is_smooth, ]

  lbl <- draw_nepht_api_list_window(title = title, instruction = instr,
                                      mylist = l$cat_name)$myvar

  id <- unique(l$cat_id[l$cat_name %in% lbl])
  tag <- unique(l$cat_lbl[l$cat_name %in% lbl])

  if (!is.na(tag)) {
    l <- l[l$cat_lbl == tag, ]

    title <- "Measure Stratification Selection"
    instr <- paste0("Please select at least one level for \n",
                    "content area: ", content$lbl, "\n",
                    "indicator: ", indicator$lbl, "\n",
                    "measure: ", measure$lbl, "\n",
                    "geographic level: ", geolevel$lbl, " level \n",
                    "stratification: ", lbl)
    data <- read.csv(paste(path, "measurestrat_pkg.csv", sep = "/"),
                     stringsAsFactors = FALSE)

    m_lbl <- draw_nepht_api_list_window(title = title, instruction = instr,
                                        mylist = l$lvl_name,
                                        opts = "extended")$myvar
    m_id <- l$lvl_id[l$lvl_name %in% m_lbl]
  } else {
    m_id <- m_lbl <- tag <- NULL
  }

  strat <- list(lbl = lbl, id = id, type = tag,
                m_id = m_id, m_lbl = m_lbl)

  #---- years ----
  # need mes_id, geo_id, cat_val
  # note measure ID and geography ID
  # or allow multiple selections
  title <- "Time Period Selection"
  instr <- paste0("Please select at least one year for \n",
                  "content area: ", content$lbl, "\n",
                  "indicator: ", indicator$lbl, "\n",
                  "measure: ", measure$lbl, "\n",
                  "geographic level: ", geolevel$lbl, " level.")
  data <- read.csv(paste(path, "temporal_pkg.csv", sep = "/"),
                   stringsAsFactors = FALSE)

  if(geolevel$id == "All") {
    geo_id <- 2
  } else {
    geo_id <- geolevel$id
  }
  l <- data[data$mes_id == measure$id & data$geo_id == geo_id, ]
  # multi-year; appears to be labeled with latest year in API,
  # but all years listed in temporal table
  # may vary by state; code based on AZ for now
  if (content$lbl == "Birth Defects") {
    l <- l[l$cat_val > min(l$cat_val) + 4, ]
    opts <- "single"
  } else {
    opts <- "extended"
  }

  id <- draw_nepht_api_list_window(title = title, instruction = instr,
                                   mylist = unique(l$cat_val),
                                   opts = opts)$myvar
  years <- list(id = id[order(-id)])

  #---- final list ----
  if (geography$id == "ALL") geolevel$id <- "ALL"
  settings <- list(content = content,      # lbl, id
                   indicator = indicator,  # lbl, id
                   measure = measure,      # lbl, id
                   geolevel = geolevel,    # lbl, id, is_smooth
                   geography = geography,  # lbl, id
                   stratification = strat, # lbl, id, m_lbl, m_id, type
                   years = years)          # id
  return(settings)
}
