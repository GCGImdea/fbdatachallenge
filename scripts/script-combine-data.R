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
    
#    cat(colnames(all_df))
    return(all_df)
  }

signal_to_match <- "deaths"

signals_to_try <- c(
  "pct_fever",
  "pct_cough",
  "pct_difficulty_breathing",
  "pct_fatigue",
  "pct_stuffy_runny_nose",
  "pct_aches_muscle_pain",
  "pct_sore_throat",
  "pct_chest_pain",
  "pct_nausea",
  "pct_anosmia_ageusia",
  "pct_eye_pain",
  "pct_headache",
  "pct_cmnty_sick",
  "pct_ever_tested",
  "pct_tested_recently",
  "pct_worked_outside_home",
  "pct_grocery_outside_home",
  "pct_ate_outside_home",
  "pct_spent_time_with_non_hh",
  "pct_attended_public_event",
  "pct_used_public_transit",
  "pct_direct_contact_with_non_hh",
  "pct_wear_mask_all_time",
  "pct_wear_mask_most_time",
  "pct_wear_mask_half_time",
  "pct_wear_mask_some_time",
  "pct_wear_mask_none_time",
  "pct_no_public",
  "pct_feel_nervous_all_time",
  "pct_feel_nervous_most_time",
  "pct_feel_nervous_some_time",
  "pct_feel_nervous_little_time",
  "pct_feel_nervous_none_time",
  "pct_feel_depressed_all_time",
  "pct_feel_depressed_most_time",
  "pct_feel_depressed_some_time",
  "pct_feel_depressed_little_time",
  "pct_feel_depressed_none_time",
  "pct_worried_ill_covid19_very",
  "pct_worried_ill_covid19_somewhat",
  "pct_worried_ill_covid19_notTooWorried",
  "pct_worried_ill_covid19_notWorried",
  "pct_enough_toEat_very_worried",
  "pct_enough_toEat_somewhat_worried",
  "pct_enough_toEat_notToo_worried",
  "pct_enough_toEat_not_worried"
  #  "cases_contagious",
  #  "cases_active",
  #  "cases",
  #  "cases_daily"
  #,
  #  "pct_chills",
  #  "pct_finances_very_worried",
  #  "pct_finances_somewhat_worried",
  #  "pct_finances_notToo_worried",
  #  "pct_finances_not_worried"
)

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
iso_codes <- c("ES")

# Load and process given countries
for (code in iso_codes)
{
  cat(" Doing ", code, ": ")
  all_df <- load_and_combine(code,TRUE)
  
  ## Prepare sources and target 
  y <- signal_to_match
  y_df <- all_df %>% dplyr::select(date,y)
  ry <- max(y_df$date)
  ly <- min(y_df$date)
  
  x_df <- all_df %>% dplyr::select(date)
  for (signal in signals_to_try)
  {
    s_df <- all_df %>% dplyr::select(date,signal)
    x_df <- x_df %>% full_join(s_df, by = "date")
  }
  x_df <- x_df[complete.cases(x_df), ]
    rx <- max(x_df$date)
  lx <- min(x_df$date)
  
  ## todo
  
  ## Calculate correlations
  
  
}




