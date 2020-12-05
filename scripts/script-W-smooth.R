library(dplyr)
library(lubridate)

# smoothed p_cases and CI:
source("smooth_column-v2.R")
source("smooth_column_past.R")

smooth_param <- 15
active_window <- 18 #https://patient.info/news-and-features/coronavirus-how-quickly-do-covid-19-symptoms-develop-and-how-long-do-they-last
path_W <- "../data/estimates-W/"
path_W_dunbar <- "../data/estimates-W-dunbar/"
# estimates_path <- "./estimates-W/"

start_date <- "2020-03-01"
# end_date <- "2020-11-02"
end_date <- today()

do_smoothing <- function(country = "ES",
                         estimates_path){
  
  cat("Smoothing ", country, " with path ", estimates_path, "\n")
  dt <- read.csv(paste0(estimates_path, "PlotData/", country, "-estimate.csv"), as.is = T)
  dt$date <- as.Date(dt$date)
  dt <- dt[dt$date >= ymd(start_date) & dt$date <= ymd(end_date),]
  
  dt_past <- dt
  if (nrow(dt) >= smooth_param) {
    # --- Smoothing
    
    # dt[["p_cases"]][is.na(dt[["p_cases"]])] <- 0
    # if (sum(dt$p_cases != 0) > smooth_param) {
    #   dt <- smooth_column_cum(dt, "p_cases", smooth_param)
    # }
    # dt[["p_cases_recent"]][is.na(dt[["p_cases_recent"]])] <- 0
    # if (sum(dt$p_cases_recent != 0) > smooth_param) {
    #   dt <- smooth_column(dt, "p_cases_recent", smooth_param)
    # }
    # dt <- smooth_column_cum(dt, "p_cases", smooth_param)
    # dt <- smooth_column(dt, "p_cases_recent", smooth_param)
    
    cat("Smoothing p_cases...\n")
    try(
    dt <- smooth_column(dt, "p_cases", basis_dim = smooth_param, link_in ="log", monotone = T)
    , silent = F)
    cat("Smoothing p_cases...\n")
    
    cat("Smoothing p_cases_fatalities...\n")
    try(
    dt <- smooth_column(dt, "p_cases_fatalities", basis_dim = smooth_param, link_in ="log", monotone = T)
    , silent = F)

    cat("Smoothing p_cases_recent...\n")
    try(
    dt <- smooth_column(dt, "p_cases_recent", basis_dim = smooth_param, link_in ="log", monotone = F)
    , silent = F)
    
    cat("Smoothing p_cases_stillsick...\n")
    try(
    dt <- smooth_column(dt, "p_cases_stillsick", basis_dim = smooth_param, link_in ="log", monotone = F)
    , silent = F)
    

    # --- Saving new data
    
    dir.create(paste0(estimates_path, "PlotData/"), showWarnings = F)
    cat(paste0("::- script-W: Writing the smoothed region based estimate summary for ", country, "..\n"))
    write.csv(dt, paste0(estimates_path, "PlotData/", country, "-estimate-smooth.csv"),
              row.names = FALSE)
  }
}

interest <- c("US", "ES", "BR", "DE", "EC", "PT", "FR", "IT", "GB", "CY", "CL", "UA")
dd <- sapply(interest, do_smoothing, estimates_path = path_W)
# dd <- sapply(interest, do_smoothing, estimates_path = path_W_dunbar)





# cat("Smoothing ES\n")
# do_smoothing("ES", path_W)
# do_smoothing("ES", path_W_dunbar)
# cat("Smoothing BR\n")
# do_smoothing("BR", path_W)
# do_smoothing("BR", path_W_dunbar)
# cat("Smoothing US\n")
# do_smoothing("US", path_W)
# do_smoothing("US", path_W_dunbar)
# do_smoothing("PT", path_W)
# do_smoothing("PT", path_W_dunbar)

