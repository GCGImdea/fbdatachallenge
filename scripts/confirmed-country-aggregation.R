library(dplyr)

estimates_path <- "../data/estimates-confirmed/"

agg_ccfr <- function(country_code = c("ES", "BR")){
  country_code <- match.arg(country_code)
  filenames <- list.files(paste0(estimates_path, country_code),
                          pattern="*.csv", full.names=TRUE)
  ldf <- lapply(filenames, read.csv, as.is = T)
  ldf <- do.call(rbind, ldf)
  #ldf$date <- as.Date(ldf$date, format = "%Y-%m-%d")
  
  country_est <- ldf %>%
    group_by(date) %>%
    summarise(countrycode = country_code,
              population = sum(unique(population), na.rm = T),
              cases = sum(cases, na.rm = T),
              deaths = sum(deaths, na.rm = T),
              hospital = sum(hospital, na.rm = T),
              icu = sum(icu, na.rm = T),
              cases_prev_week = sum(cases_prev_week, na.rm = T),
              deaths_prev_week = sum(deaths_prev_week, na.rm = T),
              hospital_prev_week = sum(hospital_prev_week, na.rm = T),
              icu_prev_week = sum(icu_prev_week, na.rm = T)
              )
  
  dir.create(paste0(estimates_path, "PlotData"), showWarnings = F)
  write.csv(country_est, paste0(estimates_path, "PlotData/", country_code, "-estimate.csv"),
            row.names = FALSE)
}

agg_ccfr("ES")
#agg_ccfr("BR")