library(dplyr)
active_window <- 18
cfr_baseline <- 1.38

###################################################################
calculate_ci <- function(p_est, level, pop_size) {
  z <- qnorm(level+(1-level)/2)
  se <- sqrt(p_est*(1-p_est))/sqrt(pop_size)
  return(list(low=p_est-z*se, upp=p_est+z*se, error=z*se))
}

hosp_to_death_trunc <- function(x, mu_hdt, sigma_hdt){
  dlnorm(x, mu_hdt, sigma_hdt)
}

scale_cfr <- function(data_1_in, delay_fun, mu_hdt, sigma_hdt){
  case_incidence <- data_1_in$cases
  case_incidence[is.na(case_incidence)] <- 0
  death_incidence <- data_1_in$deaths
  cumulative_known_t <- 0 # cumulative cases with known outcome at time tt
  # Sum over cases up to time tt
  for(ii in 1:length(case_incidence)){
    known_i <- 0 # number of cases with known outcome at time ii
    for(jj in 0:(ii - 1)){
      known_jj <- case_incidence[ii - jj] * delay_fun(jj, mu_hdt = mu_hdt, 
                                                      sigma_hdt = sigma_hdt)
      known_i <- known_i + known_jj
    }
    cumulative_known_t <- cumulative_known_t + known_i # Tally cumulative known
  }
  # naive CFR value
  b_tt <- sum(death_incidence)/sum(case_incidence) 
  # corrected CFR estimator
  p_tt <- sum(death_incidence)/cumulative_known_t
  if (sum(death_incidence, na.rm = T) > cumulative_known_t){
    ccfrr <- data.frame(nCFR = 0, cCFR = 0, total_deaths = 0, 
                        cum_known_t = 0, total_cases = sum(case_incidence))
  } else{
    ccfrr <- data.frame(nCFR = b_tt, cCFR = p_tt, total_deaths = sum(death_incidence), 
                        cum_known_t = round(cumulative_known_t), total_cases = sum(case_incidence))
  }
  return(ccfrr)
}

#############################################################################



plot_estimates <- function(region_code = "BRAC",
                           z_mean_hdt = 13,
                           z_sd_hdt = 12.7,
                           z_median_hdt = 9.1,
                           c_cfr_baseline = 1.38,
                           c_cfr_estimate_range = c(1.23, 1.53),
                           dts = data, 
                           ac_window){
  #cat("::- script-ccfr-based: Computing ccfr-based estimates for", country_geoid, "::\n")
  mu_hdt = log(z_median_hdt)
  sigma_hdt = sqrt(2*(log(z_mean_hdt) - mu_hdt))
  
  # extract data for region
  dt <- dts %>%
    filter(regioncode == region_code)
  #dt$deaths[is.na(dt$deaths)] <- 0
  
  #dt <- as.data.frame(data[rev(1:nrow(data)),])
  # ####### fix NAs in cases and deaths #######
  # dt$cases[is.na(dt$cases)] <- 0
  # dt$deaths[is.na(dt$deaths)] <- 0
  # ##########################################
  
  dt$cum_cases <- cumsum(dt$cases)
  dt$cum_deaths <- cumsum(dt$deaths)
  
  dt$date <- as.Date(dt$date, format = "%Y-%m-%d")
  #dt$fecha <- gsub("-", "/", dt$date)
  
  # dt <- dt %>% 
  #   select(date, cases, deaths, cum_cases, cum_deaths) %>% 
  #   rename(population = popData2019) %>% 
  #   select(date, cases, deaths, cum_cases, cum_deaths, population)
  
  
  ndt <- nrow(dt)
  est_ccfr <- rep(NA, ndt)
  est_ccfr_low <- rep(NA, ndt)
  est_ccfr_high <- rep(NA, ndt)
  ccfr_factor <- rep(NA, ndt)
  p_ccfr <- rep(NA, ndt)
  p_ccfr_low <- rep(NA, ndt)
  p_ccfr_high <- rep(NA, ndt)
  
  #cat("::- script-ccfr-based: Computing ccfr-based estimates for", country_geoid, "::\n")
  
  for (i in ndt : 1) {
    data2t <- dt[1:i, c("cases", "deaths")]
    ccfr <- scale_cfr(data2t, delay_fun = hosp_to_death_trunc,
                      mu_hdt = mu_hdt, sigma_hdt = sigma_hdt)
    fraction_reported <- c_cfr_baseline / (ccfr$cCFR*100)
    sigma_fraction_reported <- (1/ccfr$total_deaths)-(1/ccfr$cum_known_t)+ (1/1023) - (1/74130)
    fraction_reported_high <- fraction_reported * exp(1.96*sigma_fraction_reported)
    fraction_reported_low <- fraction_reported * exp(-(1.96*sigma_fraction_reported))
    est_ccfr_low[i] <- dt$cum_cases[i]*(1/fraction_reported_high)#switch low and high here coz of inverse.
    est_ccfr_high[i] <- dt$cum_cases[i]*(1/fraction_reported_low)
    est_ccfr[i] <- dt$cum_cases[i]*(1/fraction_reported)
    ccfr_factor[i] <- (1/fraction_reported)
    p_ccfr[i] <- est_ccfr[i]/dt$population[1]
    p_ccfr_low[i] <- est_ccfr_low[i]/dt$population[1]
    p_ccfr_high[i] <- est_ccfr_high[i]/dt$population[1]
  }
  
  dt$est_cases <- est_ccfr
  dt$est_cases_low <- est_ccfr_low
  dt$est_cases_high <- est_ccfr_high
  dt$p_cases <- p_ccfr
  dt$p_cases_low <- p_ccfr_low
  dt$p_cases_high <- p_ccfr_high
  
  # clean ccfr factor
  ccfr_factor[is.na(ccfr_factor)|(ccfr_factor<1)] <- 1
  # daily ccfr estimate
  dt$cases_daily <- ccfr_factor*dt$cases
  
  #total active cases
  dt$cases_active <- cumsum(c(dt$cases_daily[1:ac_window],
                              diff(dt$cases_daily, lag = ac_window)))
  #undetected active cases
  undetected_daily_estimate <-  dt$cases_daily - dt$cases
  dt$cases_active_undected <- cumsum(c(undetected_daily_estimate[1:ac_window],
                                       diff(undetected_daily_estimate, lag = ac_window)))
  
  dt$p_cases_daily <- ccfr_factor*dt$cases_daily/dt$population
  dt$p_cases_active <- dt$cases_active/dt$population
  dt$p_cases_active_undetected <- dt$cases_active_undected/dt$population
  
  
  dt_w <- dt %>% 
    select("date", "regioncode", "cases", "deaths", "cum_cases", "cum_deaths", "cases_daily", "cases_active", "cases_active_undected", 
           "p_cases", "p_cases_low", "p_cases_high", "p_cases_daily", "p_cases_active", "p_cases_active_undetected", 
           "population")
  
  dir.create("../data/estimates-ccfr-based/BR/", showWarnings = F)
  cat("::- script-ccfr-based: Writing data for", dt_w$regioncode[1], "::\n")
  write.csv(dt_w, paste0("../data/estimates-ccfr-based/BR/",
                         dt_w$regioncode[1], "-estimate.csv"))
  
} 

#############################


generate_estimates <- function(active_window_cases,
                               cfr_baseline){
  gunzip_link <- "https://data.brasil.io/dataset/covid19/caso_full.csv.gz"
  #download.file(gunzip_link, destfile = "../data/brazil_io_regional/caso_full.csv.gz")
  cases_full <- read.csv("../data/brazil_io_regional/caso_full.csv", as.is = T) %>% 
    filter(place_type == "state") %>% 
    select(date, state, new_confirmed, new_deaths, estimated_population_2019) %>% 
    rename(cases = new_confirmed, 
           deaths = new_deaths, 
           population = estimated_population_2019) %>% 
    mutate(regioncode = paste0("BR", state))
  
  
  regsdata <- read.csv("../data/common_data/regions-tree-population.csv", as.is = T) %>% 
    filter(countrycode == "BR") %>% 
    group_by(regioncode) %>% 
    summarise(population = sum(population))
  
  # data <- left_join(cases_full, regsdata, by = "regioncode")
  
  go <- sapply(sort(unique(cases_full$regioncode)),
               plot_estimates, dts =  cases_full, ac_window = active_window_cases, 
               c_cfr_baseline = cfr_baseline)
  
}
generate_estimates(active_window, cfr_baseline)