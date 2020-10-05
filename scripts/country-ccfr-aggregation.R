library(dplyr)
agg_ccfr <- function(country_code = c("ES", "BR")){
  country_code <- match.arg(country_code)
  filenames <- list.files(paste0("../data/estimates-ccfr-based/", country_code),
                          pattern="*.csv", full.names=TRUE)
  ldf <- lapply(filenames, read.csv, as.is = T)
  ldf <- do.call(rbind, ldf)
  #ldf$date <- as.Date(ldf$date, format = "%Y-%m-%d")
  
  country_level_estimates <- ldf %>%
    group_by(date) %>%
    summarise(total_cases = sum(cases, na.rm = T),
              total_deaths = sum(deaths, na.rm = T),
              total_cases_daily = sum(cases_daily, na.rm = T),
              total_cases_active = sum(cases_active, na.rm = T),
              total_cases_active_undetected = sum(cases_active_undected, na.rm = T))
  
  country_level_estimates$total_cum_cases = cumsum(country_level_estimates$total_cases)
  country_level_estimates$total_cum_deaths = cumsum(country_level_estimates$total_deaths)
  write.csv(country_level_estimates, 
            paste0("../data/estimates-ccfr-based/",country_code,
            "-country-ccfr-aggregate-estimate.vsv"))
}

agg_ccfr("ES")
agg_ccfr("BR")