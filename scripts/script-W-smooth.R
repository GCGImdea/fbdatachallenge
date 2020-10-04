library(dplyr)

# smoothed p_cases and CI:
source("smooth_column.R")
smooth_param <- 25
active_window <- 18 #https://patient.info/news-and-features/coronavirus-how-quickly-do-covid-19-symptoms-develop-and-how-long-do-they-last

#######

estimates_path <- "../data/estimates-W/"
# estimates_path <- "./estimates-W/"

do_smoothing <- function(country = "ES"){
dt <- read.csv(paste0(estimates_path, "PlotData/", country, "-estimate.csv"), as.is = T)

# --- Smoothing

dt[["p_cases"]][is.na(dt[["p_cases"]])] <- 0
if (sum(dt$p_cases != 0) > smooth_param) {
  dt <- smooth_column(dt, "p_cases", smooth_param)
}
dt[["p_cases_recent"]][is.na(dt[["p_cases_recent"]])] <- 0
if (sum(dt$p_cases_recent != 0) > smooth_param) {
  dt <- smooth_column(dt, "p_cases_recent", smooth_param)
}

# --- Computing the daily differences of cases

dt$p_daily <- c(0 , diff(dt$p_cases))
dt[["p_cases_smooth"]][is.na(dt[["p_cases_smooth"]])] <- 0
dt$p_daily_smooth <- c(0 , diff(dt$p_cases_smooth))

#total active cases
dt$p_active <- cumsum(c(dt$p_daily[1:active_window],
                        diff(dt$p_daily, lag = active_window)))
dt$p_active_smooth <- cumsum(c(dt$p_daily_smooth[1:active_window],
                        diff(dt$p_daily_smooth, lag = active_window)))
# --- Saving new data

dir.create(paste0(estimates_path, "PlotData/"), showWarnings = F)
cat(paste0("::- script-W: Writing the smoothed region based estimate summary for ", country, "..\n"))
write.csv(dt, paste0(estimates_path, "PlotData/", country, "-estimate.csv"))
}

cat("Smoothing ES\n")
do_smoothing("ES")
cat("Smoothing BR\n")
do_smoothing("BR")
cat("Smoothing US\n")
do_smoothing("US")
