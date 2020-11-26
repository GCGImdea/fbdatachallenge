## Libraries
library(dplyr)
library(stringr)
library(zoo) # to use rollmean
library(foreign)
library(MASS)
library(ggplot2)
library(grid) # annotate a ggplot

load_and_combine <-
  function(code) {
    all_df <- data.frame()
    
    ## Load and clean UMD regressors ----
    loaded_umd_df <- read.csv(paste0("../data/estimates-umd-unbatched/PlotData/", code ,"_UMD_country_nobatch_past_smooth.csv"))
    loaded_umd_df < loaded_umd_df %>%
      mutate(date = as.Date(date)) %>%
      dplyr::select(starts_with("pct_")
      
    
#    cat(colnames(loaded_umd_df))
    
    ## Load and clean CCFR regressors
    loaded_ccfr_df <- read.csv(paste0("../data/estimates-ccfr-based/PlotData/",code,"-estimate.csv"))
    loaded_ccfr_df <- loaded_ccfr_df %>% 
      mutate(date = as.Date(date)) %>% 
      dplyr::select(date,cases,cases_daily,cases_contagious,cases_active)
    
#    cat(colnames(loaded_ccfr_df))
    
    ## Load NSUM and clean regressors
    
    
    return(all_df)
  }

# Files to consider, could be other source of names
file_in_path <- "../data/estimates-umd-unbatched/PlotData/"
file_in_pattern <- ".*_UMD_country_nobatch_past_smooth.csv"
files <- dir(file_in_path, pattern = file_in_pattern)

# ISO codes to process
iso_codes <- c()
for (file in files) {
  iso_codes <- c(iso_codes,substr(file, 1, 2))
}

# Debug: Optionaly reduce to some countries to run faster
iso_codes <- c("PT","ES")

# Load and process given countries
for (code in iso_codes)
{
  cat("doing ", code, ": ")
  all_df <- load_and_combine(code)
}




