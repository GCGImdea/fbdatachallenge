library(dplyr)
agg_ccfr <- function(country_code = c("ES", "BR")){
  country_code <- match.arg(country_code)
  filenames <- list.files(paste0("../data/estimates-ccfr-based/", country_code),
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
              cum_cases = sum(cum_cases, na.rm = T),
              cum_deaths = sum(cum_deaths, na.rm = T),
              deaths_prev_week = sum(deaths_prev_week, na.rm = T),

              cases_infected = sum(cases_infected, na.rm = T),
              cases_infected_low = sum(cases_infected_low, na.rm = T),
              cases_infected_high = sum(cases_infected_high, na.rm = T),
              cases_daily = sum(cases_daily, na.rm = T),
              cases_contagious = sum(cases_contagious, na.rm = T),
              cases_active = sum(cases_active, na.rm = T))
  
  country_est$p_cases_infected <- country_est$cases_infected / country_est$population
  country_est$p_cases_infected_low <- country_est$cases_infected_low / country_est$population
  country_est$p_cases_infected_high <- country_est$cases_infected_high / country_est$population
  country_est$p_cases_daily <- country_est$cases_daily / country_est$population
  country_est$p_cases_contagious <- country_est$cases_contagious / country_est$population
  country_est$p_cases_active <- country_est$cases_active / country_est$population
  
  write.csv(country_est, 
            paste0("../data/estimates-ccfr-based/PlotData/",country_code,
            "-estimate.csv"))
}

agg_ccfr("ES")
agg_ccfr("BR")