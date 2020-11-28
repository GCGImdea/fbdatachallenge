# load library
library(tidyverse)
library(readxl)
library(httr)
library(zoo)

data_url <- "https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/csv/data.csv"
estimates_path <- "../data/estimates-confirmed/hospital/"
country_table <- "../data/common-data/country_population_ecdc.csv"
# country_names_occupancy <- c( "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
#                     "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
#                     "Hungary", "Iceland", "Ireland", "Italy",
#                     "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
#                     "Norway", "Poland", "Portugal", "Romania",
#                     "Slovakia", "Slovenia", "Spain", "Sweden",
#                     "United Kingdom")

get_occupancy <- function(country_name = "Portugal", prefix, dts, ctbl){

  cat("::- script-occupancy: Working on", country_name, "::\n")

  ctbl <- ctbl[ctbl$country_territory == country_name, ]
  country_code <- ctbl$geo_id[1]

  dts <- dts[dts$country == country_name,]
  # dts$date <- as.Date(dts$date)
  # min_date <- dts$date[1]
  
  cat("Data",  country_code, nrow(dts), "\n")
  
  prefix$countrycode <- country_code
  prefix$countryname <- country_name
  
  df_hosp <- dts[dts$indicator == "Daily hospital occupancy",]
  
  if (nrow(df_hosp)>0) {
    df_hosp <- df_hosp %>% select(date, value) %>% rename(hosp_occupancy = value)
    # Complete all dates
    # df_hosp <- df_hosp %>%
    #   mutate(date = as.Date(date)) %>%
    #   complete(date = seq.Date(min_date, Sys.Date(), by="day")) %>%
    #   fill(countrycode, countryname)
    
    prefix$date <- as.Date(prefix$date)
    df_hosp$date <- as.Date(df_hosp$date)
     
    prefix <- merge(prefix, df_hosp, by = "date", all.x=T)
    prefix <- unique(prefix)
  }
  
  
  df_icu <- dts[dts$indicator == "Daily ICU occupancy",]
  
  if (nrow(df_icu)>0) {
    df_icu <- df_icu %>% select(date, value) %>% rename(icu_occupancy = value)
    # Complete all dates
    # df_hosp <- df_hosp %>%
    #   mutate(date = as.Date(date)) %>%
    #   complete(date = seq.Date(min_date, Sys.Date(), by="day")) %>%
    #   fill(countrycode, countryname)
    
    prefix$date <- as.Date(prefix$date)
    df_icu$date <- as.Date(df_icu$date)
    
    prefix <- merge(prefix, df_icu, by = "date", all.x=T)
    prefix <- unique(prefix)
  }
  
  
  df_hosp <- dts[dts$indicator == "Weekly new hospital admissions per 100k",]
  
  if (nrow(df_hosp)>0) {
    df_hosp <- df_hosp %>% select(year_week, value) %>% rename(hosp_weekly_admission = value)
    # Complete all dates
    # df_hosp <- df_hosp %>%
    #   mutate(date = as.Date(date)) %>%
    #   complete(date = seq.Date(min_date, Sys.Date(), by="day")) %>%
    #   fill(countrycode, countryname)
    
    # prefix$date <- as.Date(prefix$date)
    # df_hosp$date <- as.Date(df_hosp$date)
    
    prefix <- merge(prefix, df_hosp, by = "year_week")
    prefix <- unique(prefix)
    prefix <- prefix[!is.na(prefix$date),]
  }
  
  df_icu <- dts[dts$indicator == "Weekly new ICU admissions per 100k",]
  
  if (nrow(df_icu)>0) {
    df_icu <- df_icu %>% select(year_week, value) %>% rename(icu_weekly_admission = value)
    # Complete all dates
    # df_hosp <- df_hosp %>%
    #   mutate(date = as.Date(date)) %>%
    #   complete(date = seq.Date(min_date, Sys.Date(), by="day")) %>%
    #   fill(countrycode, countryname)
    
    # prefix$date <- as.Date(prefix$date)
    # df_hosp$date <- as.Date(df_hosp$date)
    
    prefix <- merge(prefix, df_icu, by = "year_week")
    prefix <- unique(prefix)
    prefix <- prefix[!is.na(prefix$date),]
  }
  
  prefix$date <- as.Date(prefix$date)
  prefix <- prefix[order(prefix$date),]  
  
    dir.create(estimates_path, showWarnings = F)
    # cat("::- script-confirmed: Writing data for", country_geoid, "::\n")
    write.csv(prefix, paste0(estimates_path, country_code, "-hospital-icu.csv"),
              row.names = FALSE)

}

dts <- read.csv(data_url)
ctbl <- read.csv(country_table)

# France has all the dates and weeks
prefix <- dts[dts$country == "France",c("date", "year_week")]

# Complete all dates
prefix$date <- as.Date(prefix$date)
prefix <- prefix %>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(prefix$date[1], Sys.Date(), by="day")) %>%
  fill(year_week)

country_names_occupancy <- unique(dts$country)

kk <- sapply(country_names_occupancy, get_occupancy, prefix=prefix, dts = dts, ctbl = ctbl)
