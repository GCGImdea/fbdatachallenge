library(dplyr)
filenames <- list.files("../data/estimates-ccfr-based/BR", pattern="*.csv", full.names=TRUE)
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

