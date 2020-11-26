## Libraries
library(dplyr)
library(stringr)
library(zoo) # to use rollmean
library(foreign)
library(MASS)
library(ggplot2)
library(grid) # annotate a ggplot

load_and_combine <-
  function(code, nsum=FALSE) {
    
    ## Load and clean official data targets
    loaded_confirmed_df <-read.csv(paste0("../data/estimates-confirmed/PlotData/",code,"-estimate.csv"))
    
    df_confirmed <- loaded_confirmed_df %>%
      mutate(date = as.Date(date)) %>%
      dplyr::select(date,deaths,cases)
    
    pop <- loaded_confirmed_df$population[1]
    cat("[loaded confirmed]")
    
#    cat(colnames(loaded_confirmed_df))
    
    ## Load and clean UMD regressors ----
    loaded_umd_df <- read.csv(paste0("../data/estimates-umd-unbatched/PlotData/", code ,"_UMD_country_nobatch_past_smooth.csv"))
    df_umd <- loaded_umd_df %>% dplyr::select(starts_with("pct"))
    df_umd <- df_umd * pop / 100
    df_umd$date <- as.Date(loaded_umd_df$date)
    
    cat("[loaded UMD]")
#    cat(colnames(loaded_umd_df))
    
    ## Load and clean CCFR regressors
    loaded_ccfr_df <- read.csv(paste0("../data/estimates-ccfr-based/PlotData/", code ,"-estimate.csv"))
    df_ccfr <- loaded_ccfr_df %>% 
      mutate(date = as.Date(date)) %>% 
      dplyr::select(date,cases_daily,cases_contagious,cases_active)
    
    cat("[loaded CCFR]")    
#    cat(colnames(loaded_ccfr_df))
    
    ## Load NSUM and clean regressors, not all countries have this
    if (nsum)
    {
      loaded_nsum_df <- read.csv(paste0("../data/estimates-W/PlotData/",code,"-estimate.csv"))
      df_nsum <- loaded_nsum_df %>% dplyr::select(p_cases,p_cases_recent,p_cases_fatalities,p_cases_stillsick)
      df_nsum <- df_nsum * pop
      df_nsum$date <- as.Date(loaded_nsum_df$date)
 
      cat("[loaded NSUM]")
    }
    
    ## Stitch together data frames ...
    
    all_df <- df_umd %>% full_join(df_ccfr, by = "date")
    
    if (nsum)
    {
      all_df <- all_df %>% full_join(df_nsum, by = "date")
    }
    
    all_df <- all_df %>% full_join(df_confirmed, by = "date")
    
    cat(colnames(all_df))
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
  cat("; Doing ", code, ": ")
  all_df <- load_and_combine(code,TRUE)
}




