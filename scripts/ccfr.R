library(dplyr)

dtcases <- read.csv("../data/datadista_regional/ccaa_covid19_datos_isciii_nueva_serie.csv", as.is = T)
dtcases <- dtcases %>% select(fecha, cod_ine, ccaa, num_casos) %>% rename(cases = num_casos)

dtdeaths <- read.csv("../data/datadista_regional/ccaa_covid19_fallecidos_por_fecha_defuncion_nueva_serie_long.csv",
                     as.is = T)
dtdeaths <- dtdeaths %>% rename(ccaa = CCAA, deaths = total)

data <- full_join(dtcases, dtdeaths, by = c("fecha", "cod_ine", "ccaa"))

plot_estimates <- function(region_ine = 1,
                           z_mean_hdt = 13,
                           z_sd_hdt = 12.7,
                           z_median_hdt = 9.1,
                           c_cfr_baseline = 1.38,
                           c_cfr_estimate_range = c(1.23, 1.53),
                           dts = data, 
                           ac_window){
  cat("::- script-ccfr-based: Computing ccfr-based estimates for", country_geoid, "::\n")
  mu_hdt = log(z_median_hdt)
  sigma_hdt = sqrt(2*(log(z_mean_hdt) - mu_hdt))
  
  # extract data for region
  dt <- dts %>%
    filter(cod_ine == region_ine)
  dt$deaths[is.na(dt$deaths)] <- 0
  
  #dt <- as.data.frame(data[rev(1:nrow(data)),])
  # ####### fix NAs in cases and deaths #######
  # dt$cases[is.na(dt$cases)] <- 0
  # dt$deaths[is.na(dt$deaths)] <- 0
  # ##########################################
  
  dt$cum_cases <- cumsum(dt$cases)
  dt$cum_deaths <- cumsum(dt$deaths)
  
  dt$date <- as.Date(dt$fecha, format = "%Y-%m-%d")
  dt$fecha <- gsub("-", "/", dt$date)
  
  dt <- data %>% 
    select(date, cases, deaths, cum_cases, cum_deaths) %>% 
    rename(population = popData2019) %>% 
    select(date, cases, deaths, cum_cases, cum_deaths, population)
  
  
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
    select("date", "cases", "deaths", "cum_cases", "cum_deaths", "cases_daily", "cases_active", "cases_active_undected", 
           "p_cases", "p_cases_low", "p_cases_high", "p_cases_daily", "p_cases_active", "p_cases_active_undetected", 
           "population")
  
  dir.create("../data/estimates-ccfr-based/PlotData/", showWarnings = F)
  cat("::- script-ccfr-based: Writing data for", country_geoid, "::\n")
  write.csv(dt_w, paste0("../data/estimates-ccfr-based/PlotData/", country_geoid, "-estimate.csv"))
  
} 


generate_estimates <- function(active_window_cases = 12){
  url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
               Sys.Date(), ".xlsx", sep = "")
  GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
  cat("::- script-ccfr: Checking the ECDC data for the day ::\n")
  #try( data_ecdc <- read_excel(tf), silent = T) # ECDC daily excel seems unvailable for now
  try( data_ecdc <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                             na.strings = "", fileEncoding = "UTF-8-BOM"), silent = T)
  
  if(!exists("data_ecdc")){
    cat("::- script-ccfr: Seems the ECDC data for the day is not available yet ::\n")
    cat("::- script-ccfr: Trying to get data for the previous day ::\n")
    url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
                 Sys.Date()-1, ".xlsx", sep = "")
    GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
    try( data_ecdc <- read_excel(tf), silent = T)
    if(!exists("data_ecdc")){
      stop("::- script-confirmed: Unfortunately, the ECDC data for yesterday is not availabe neither ::\n")
    }else{
      cat("::- script-ccfr: Using ECDC data for previous day ::\n")
      data_ecdc$countryterritoryCode[data_ecdc$geoId == "CZ"] <- "CZE" # add "CZ" manually
      data_country_code <- read_excel("../data/common-data/wikipedia-iso-country-codes.xlsx")
      names(data_country_code) <- c("English.short.name.lower.case", "Alpha.2.code",
                                    "Alpha.3.code", "Numeric.code", "ISO.3166.2")
      
      data_ecdc <- inner_join(data_ecdc, data_country_code, by = c("countryterritoryCode" = "Alpha.3.code"))
      
      all_geo_ids <- unique(data_ecdc$Alpha.2.code)
      sapply(all_geo_ids, plot_estimates, dts = data_ecdc)
    }
  } else{
    cat("::- script-ccfr: ECDC data for the day available! ::\n")
    data_ecdc$countryterritoryCode[data_ecdc$geoId == "CZ"] <- "CZE" # add "CZ" manually
    data_country_code <- read_excel("../data/common-data/wikipedia-iso-country-codes.xlsx")
    names(data_country_code) <- c("English.short.name.lower.case", "Alpha.2.code",
                                  "Alpha.3.code", "Numeric.code", "ISO.3166.2")
    data_ecdc <- inner_join(data_ecdc, data_country_code, by = c("countryterritoryCode" = "Alpha.3.code"))
    all_geo_ids <- unique(data_ecdc$Alpha.2.code) 
    go <- sapply(all_geo_ids, plot_estimates, dts =  data_ecdc, ac_window = active_window_cases)
  }
  
}
generate_estimates()

#plot_estimates(country_geoid = "ES", dts =  data_ecdc, ac_window = active_window_cases)
