## script needs file for country and country population.
library(tidyverse)
library(readxl)
library(httr)

# Downlod the data from UMD repository to data/UMD_updated/Full_Survey_Data/
try(source("script-import-UMD-onlyfullcountry.R"), silent = F)
try(source("script-import-UMD-onlyfullregion.R"), silent = F)

# Generate past_smooth columns for UMD data, in data/estimates-umd-unbatched/PlotData/
# NOTE: Takes several hours (past_smoothing is slow)
try(source("UMD_country_past_smoothing_fromOriAll.R"), silent = F)


# Download the data about confirmed cases, deaths, hospital, icu, etc. and accumulate for weeks
try(source("script-confirmed2.R"), silent = T) # Downloads all country cases and deaths from ECDC
try(source("script-confirmed_ES_datadista.R"), silent = T) # Downloads regional Spanish cases, deaths, hospital and icu from Datadista github
try(source("script-confirmed-country-aggregation.R"), silent = T) # Aggrgates regional data for the country, when available
try(source("script-confirmed-hospital.R"), silent = T) # Downloads hospital and ICU occupancy from ECDC

# Compute estimates from the CoronaSurveys responses
try(source("script-W-v2.R"), silent = T)
#try(source("script-W-dunbar.R"), silent = T)
try(source("script-W-smooth.R"), silent = T)  # Uses smooth_column-v2.R
try(source("script-W-past-smooth.R"), silent = T)  # Uses smooth_column-v2.R
try(source("script-W-plots.R"), silent = T)

# Compute CCFR estimates
try(source("script-ccfr-based3.R"), silent = T) # Generates CCFR estimates for all countries from ECDC data
try(source("script-ccfr-ES-based.R"), silent = T) # Generates CCFR regional ES estimates from Datadista
try(source("script-ccfr-BR-based.R"), silent = T) # Generates CCFR regional BR estimates 
try(source("script-ccfr-country-aggregation-v2.R"), silent = T) # Aggregates regional estimates, when available

# Merge data into giant CSV files per country
try(source("script-stitch-data-v2.R"), silent = T)

#Plotting combining multiple estimates
try(source("script-ggplot-cumulative-v2.R"), silent = T)
#try(source("script-plot-active.R"), silent = T)
try(source("script-ggplot-active.R"), silent = T)
try(source("script-ggplot-active-region.R"), silent = T)





