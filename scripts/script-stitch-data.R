library(dplyr)
library(stringr)

load_and_combine <- function(code, nsum = FALSE) {
  cat("\n working on ", code)
    ## Load and clean official data targets
    loaded_confirmed_df <- read.csv(paste0("../data/estimates-confirmed/PlotData/", code, "-estimate.csv"))
    df_confirmed <- loaded_confirmed_df %>%
      mutate(date = as.Date(date)) %>%
      dplyr::select(date, deaths, cases, population)
    # df_confirmed$cases = pmax(df_confirmed$cases, 0) # get rid of negatives
    # df_confirmed$deaths = pmax(df_confirmed$deaths, 0) # get rid of negatives
    df_confirmed$deaths[df_confirmed$deaths < 0] <- NA
    df_confirmed$cases[df_confirmed$cases < 0] <- NA
    
    pop <- loaded_confirmed_df$population[1]
    cat("[loaded confirmed]")
    
    #    cat(colnames(loaded_confirmed_df))
    
    ## Load and clean UMD regressors ----
    loaded_umd_df <- read.csv(paste0("../data/estimates-umd-unbatched/PlotData/",
                                     code, "_UMD_country_nobatch_past_smooth.csv"))
    df_umd <- loaded_umd_df %>% dplyr::select(starts_with("pct"))
    df_umd <- df_umd * pop / 100
    df_umd$date <- as.Date(loaded_umd_df$date)
    
    cat("[loaded UMD]")
    #    cat(colnames(loaded_umd_df))
    
    ## Load and clean CCFR regressors
    loaded_ccfr_df <- read.csv(paste0("../data/estimates-ccfr-based/PlotData/",
                                      code, "-estimate.csv"))
    
    df_ccfr <- loaded_ccfr_df %>%
      mutate(date = as.Date(date)) %>%
      dplyr::select(date, cases_daily, cases_contagious, cases_active)
    
    cat("[loaded CCFR]")
    #    cat(colnames(loaded_ccfr_df))
    
    ## Load NSUM and clean regressors, not all countries have this
    if (nsum) {
      loaded_nsum_df <- read.csv(paste0("../data/estimates-W/past_smooth/", code, "-estimate-past-smooth.csv"))
      
      # df_nsum <- loaded_nsum_df %>% dplyr::select(p_cases, p_cases_recent, p_cases_fatalities, p_cases_stillsick)
                                                  # p_cases_past_smooth, p_cases_fatalities_past_smooth,
                                                  # p_cases_recent_past_smooth, p_cases_stillsick_past_smooth)
      df_nsum <- subset(loaded_nsum_df, select = -c(date) )
      df_nsum <- df_nsum * pop
      df_nsum$date <- as.Date(loaded_nsum_df$date)
      
      cat("[loaded NSUM]")
    }
    
    ## Stitch together data frames ...
    
    all_df <- df_confirmed %>% full_join(df_ccfr, by = "date")
    
    if (nsum)
    {
      all_df <- all_df %>% full_join(df_nsum, by = "date")
    }
    
    all_df <- all_df %>% full_join(df_umd, by = "date")
    
    #    cat(colnames(all_df))
    all_df <- all_df[order(all_df$date),]
    out_path <- paste0("../data/all_giant_df/", code, "_alldf.csv")
    write.csv(all_df, out_path)
    cat("[saved data]")
}

#save data for countries of interest

interest <- list.files("../data/estimates-umd-unbatched/PlotData/", pattern="*.csv", full.names=FALSE)
interest <- substring(interest, 1, 2)
interest <- interest[interest != "HK"]
interest <- interest[interest != "TW"]

dd <- sapply(interest, load_and_combine, nsum = F)

#load_and_combine(code = "ES", nsum = T)
interest <- c("BR", "DE", "EC", "PT", "UA", "ES", "IT", "CL", "FR", "GB")
              #"US", "CY")
dd <- sapply(interest, load_and_combine, nsum = T)


load_and_combine(code = "ES", nsum = T)
