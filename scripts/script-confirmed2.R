# load library
library(tidyverse)
library(readxl)
library(httr)

contagious_window <- 12
active_window <- 18

plot_estimates <- function(country_geoid = "AF", dts, 
                           contagious_window,
                           active_window){
  cat("::- script-confirmed: Working on", country_geoid, "::\n")
  data <- dts %>% 
    select(dateRep:popData2019, "Alpha.2.code" )
  data$geoId <- data$Alpha.2.code 
  data <- data %>% select(dateRep:popData2019)
  data <- data[data$geoId == country_geoid,]
  
  dt <- as.data.frame(data[rev(1:nrow(data)),])
  ####### fix NAs in cases and deaths #######
  dt$cases[is.na(dt$cases)] <- 0
  dt$deaths[is.na(dt$deaths)] <- 0
  ##########################################
  dt$cases_infected <- cumsum(dt$cases)
  dt$cum_deaths <- cumsum(dt$deaths)
  
  dt$date <- gsub("-", "/", as.Date(dt$dateRep, format = "%d/%m/%Y"))
  
  if (nrow(dt) >= contagious_window){
    dt$cases_contagious <- cumsum(c(dt$cases[1:contagious_window], diff(dt$cases, lag = contagious_window))) # Carlo active cases
  }
  else {
    dt$cases_contagious <- NA
  }
  
  #symptomatic
  if (nrow(dt) >= active_window){
    dt$cases_active <- cumsum(c(dt$cases[1:active_window], diff(dt$cases, lag = active_window)))
  }
  else {
    dt$cases_active <- NA
  }
  
  #Cumulative previous week
  if (nrow(dt) >= 7){
    dt$cases_prev_week <- cumsum(c(dt$cases[1:7], diff(dt$cases, lag = 7)))
    dt$deaths_prev_week <- cumsum(c(dt$deaths[1:7], diff(dt$deaths, lag = 7)))
    # dt$hospital_prev_week <- cumsum(c(dt$hospital[1:7], diff(dt$hospital, lag = 7)))
    # dt$icu_prev_week <- cumsum(c(dt$icu[1:7], diff(dt$icu, lag = 7)))
  }
  else {
    dt$cases_prev_week <- dt$deaths_prev_week <- NA
    # dt$hospital_prev_week <- dt$icu_prev_week <- NA
  }
  
  # - Cases_infected: Population that is or has been infected of COVID-19.
  # - Cases_daily: Population infected (detected or reported) that day (to the available knowledge). In general we will not be able to say whether they have cases_actives or not.
  # - Cases_contagious: Those infected that can transmit the virus on a given day (assumes a case is contagious 12 days after infected)
  # - Cases_active: Those infected whose case is still active on a given day (assumes a case is active 18 days after infected)
  
  dt <- dt %>% 
    select(date, geoId, popData2019, cases, deaths, cases_prev_week, deaths_prev_week, cases_infected, cum_deaths, 
           cases_contagious, cases_active) %>% 
    rename(countrycode = geoId, population = popData2019) 
  # mutate(p_cases_infected = cases_infected/population,
  #        p_cases_daily = abs(cases_daily/population),
  #        p_cases_contagious = abs(cases_contagious/population),
  #        p_cases_active = abs(cases_active/population)) %>% 
  # %>% 
  #   select(date, cases, deaths, cases_infected, cum_deaths, cases_contagious, cases_infect, cases_active, p_cases, p_cases_daily, p_cases_contagious, p_infect, p_cases_active, population)

  dt$p_cases_infected <- abs(dt$cases_infected/dt$population)
  dt$p_cases <- abs(dt$cases/dt$population)
  dt$p_cases_contagious <- abs(dt$cases_contagious/dt$population)
  dt$p_cases_active <- abs(dt$cases_active/dt$population)
    
  dir.create("../data/estimates-confirmed/PlotData/", showWarnings = F)
  # cat("::- script-confirmed: Writing data for", country_geoid, "::\n")
  write.csv(dt, paste0("../data/estimates-confirmed/PlotData/", country_geoid, "-estimate.csv"))
}

generate_estimates <- function(contagious_window,
                               active_window){
    url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
                 Sys.Date(), ".xlsx", sep = "")
    GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
    cat("::- script-confirmed: Checking the ECDC data for the day ::\n")
    #try( data_ecdc <- read_excel(tf), silent = T) # ECDC daily excel seems unvailable for now
    try( data_ecdc <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                               na.strings = "", fileEncoding = "UTF-8-BOM"), silent = T)
    
    if(!exists("data_ecdc")){
      cat("::- script-confirmed: Seems the ECDC data for the day is not available yet ::\n")
      cat("::- script-confirmed: Trying to get data for the previous day ::\n")
      url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",
                   Sys.Date()-1, ".xlsx", sep = "")
      GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
      try( data_ecdc <- read_excel(tf), silent = T)
      if(!exists("data_ecdc")){
        stop("::- script-confirmed: Unfortunately, the ECDC data for yesterday is not availabe neither ::\n")
      }else{
        cat("::- script-confirmed: Using ECDC data for previous day ::\n")
        data_ecdc$countryterritoryCode[data_ecdc$geoId == "CZ"] <- "CZE" # add "CZ" manually
        data_country_code <- read_excel("../data/common-data/wikipedia-iso-country-codes.xlsx")
        names(data_country_code) <- c("English.short.name.lower.case", "Alpha.2.code",
                                      "Alpha.3.code", "Numeric.code", "ISO.3166.2")
        
        data_ecdc <- inner_join(data_ecdc, data_country_code, by = c("countryterritoryCode" = "Alpha.3.code"))
        
        all_geo_ids <- unique(data_ecdc$Alpha.2.code)
        sapply(all_geo_ids, plot_estimates, dts = data_ecdc,
               contagious_window = contagious_window, 
               active_window = active_window)
      }
    } else{
      cat("::- script-confirmed: ECDC data for the day available! ::\n")
      data_ecdc$countryterritoryCode[data_ecdc$geoId == "CZ"] <- "CZE" # add "CZ" manually
      data_country_code <- read_excel("../data/common-data/wikipedia-iso-country-codes.xlsx")
      names(data_country_code) <- c("English.short.name.lower.case", "Alpha.2.code",
                                    "Alpha.3.code", "Numeric.code", "ISO.3166.2")
      data_ecdc <- inner_join(data_ecdc, data_country_code, by = c("countryterritoryCode" = "Alpha.3.code"))
      all_geo_ids <- unique(data_ecdc$Alpha.2.code) 
      go <- sapply(all_geo_ids, plot_estimates, dts =  data_ecdc, 
                   contagious_window = contagious_window, 
                   active_window = active_window)
    }
  
}
generate_estimates(contagious_window = contagious_window,
                   active_window = active_window)
