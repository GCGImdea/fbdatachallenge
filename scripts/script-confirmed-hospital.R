# load library
library(tidyverse)
library(readxl)
library(httr)

estimates_path <- "../data/estimates-confirmed/PlotData/"
country_table <- "../data/common-data/country_population_umd.csv"
country_names <- c( "Austria", "Belgium", "Bulgaria", "Croatia", 
                    # "Cyprus", 
                    "Czechia", "Denmark", "Estonia",
                    "Finland", "France", 
                    # "Germany", "Greece", 
                    "Hungary", "Iceland", "Ireland", "Italy",
                    "Latvia", "Lithuania", "Luxembourg", "Malta", 
                    # "Netherlands", 
                    "Norway", "Poland", "Portugal",
                    # "Romania", 
                    "Slovakia", "Slovenia", "Spain", 
                    # "Sweden", 
                    "United Kingdom")

plot_estimates <- function(country_name = "Portugal", dts, ctbl){
  cat("::- script-confirmed: Working on", country_name, "::\n")

  dts <- dts[dts$country == country_name,]
  dts$date <- as.Date(dts$date)
  min_date <- dts$date[1]
  
  
  cat("kkk",  nrow(dts), min_date, "\n")
  
  df_hosp <- dts[dts$indicator == "Daily hospital occupancy",]
  df_icu <- dts[dts$indicator == "Daily ICU occupancy",]
  
  ctbl <- ctbl[ctbl$country == country_name, ]

  cat("kkk\n")
  
  if ((nrow(ctbl)>0) && (nrow(df_hosp) + nrow(df_icu) > 0)) {
    
    cat("kkk",  min_date, "\n")
    
    country_code <- ctbl$iso_alpha2[1]
    df_hosp$countrycode <- country_code
    df_hosp <- df_hosp %>% select(date, countrycode, country, value) %>% rename(countryname = country, hosp_occupancy = value)
    # Complete all dates
    df_hosp <- df_hosp %>%
      mutate(date = as.Date(date)) %>%
      complete(date = seq.Date(min_date, Sys.Date(), by="day")) %>%
      fill(countrycode, countryname)
    
    
    df_icu <- df_icu %>% select(date, value) %>% rename(icu_occupancy = value)
    df_icu <- df_icu %>%
      mutate(date = as.Date(date)) %>%
      complete(date = seq.Date(min_date, Sys.Date(), by="day"))
    
    dts <- merge(df_hosp, df_icu, by = "date")
    
    dir.create(estimates_path, showWarnings = F)
    # cat("::- script-confirmed: Writing data for", country_geoid, "::\n")
    write.csv(dts, paste0(estimates_path, country_code, "-hospital-icu.csv"),
              row.names = FALSE)
    
  }
  else {
    cat("Country not found or no data \n")
  }
}



df <- read.csv("https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/csv/data.csv")
ctbl <- read.csv(country_table)

# country_names <- unique(df$country)

sapply(country_names, plot_estimates, dts = df, ctbl = ctbl)
