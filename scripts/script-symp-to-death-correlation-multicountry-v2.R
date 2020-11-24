## Libraries
library(dplyr)
library(stringr)
library(zoo) # to use rollmean
library(foreign)
library(MASS)
library(ggplot2)

check_lags <-
  function(df_response,
           df_add_regressors,
           columns_to_try,
           min_lag = 7,
           max_lag = 60) {
    
    df_response$date <- as.Date(df_response$date)
    
    df_out <- data.frame()
    
    for (column_in in columns_to_try) {
      for (date_shift in seq(min_lag,max_lag)) {
        df_single_symp <- df_add_regressors[, c("date", column_in)]
        
        # time shifting in extra regressors:
        df_single_symp$date <-
          as.Date(df_single_symp$date) + date_shift
        
        # set the same dates in df_response and df_single_symp:
        start_date <-
          max(min(df_single_symp$date), min(df_response$date))
        end_date <-
          min(max(df_single_symp$date), max(df_response$date))
        
        df_single_symp <-
          df_single_symp %>% filter(date >= start_date, date <= end_date)
        df_response <-
          df_response %>% filter(date >= start_date, date <= end_date)
        
        # df_single_symp <- df_single_symp[1:165, ]
        # df_response <- df_response[1:165, ]
        
        win_size <-
          as.integer(max(df_response$date) - min(df_response$date))
        
        correl <-
          cor(df_response$y, df_single_symp[, column_in], method = "spearman")
        corTest <-
          cor.test(df_response$y, df_single_symp[, column_in], method = "spearman")
        
        df_correl <-
          data.frame(
            shift = date_shift,
            correlations = corTest$estimate,
            pval = corTest$p.value,
            win_size = win_size,
            signal = column_in
          )
        
        df_out <- rbind(df_out, df_correl)
      } # loop-date_shift
    } # loop-column_in
    
    
    return(df_out)
    
  } # end-check_lags

columns_to_try = c(
  "pct_anosmia_ageusia_past_smooth",
  "pct_sore_throat_past_smooth",
  "pct_fever_past_smooth",
  "pct_cmnty_sick_past_smooth",
  "pct_direct_contact_with_non_hh_past_smooth"
)


file_in_path <- "../data/estimates-umd-unbatched/PlotData/"
file_in_pattern <- ".*_UMD_country_nobatch_past_smooth.csv"

files <- dir(file_in_path, pattern = file_in_pattern)

opt_correls <- data.frame()

for (file in files) {
  tryCatch({
    iso_code_country <- substr(file, 1, 2)
    cat("doing ", iso_code_country, "-> \n")
    ## Load UMD regressors ----
    
    #data_df <-  read.csv(paste0("../data/estimates-umd-batches/", iso_code_country , "/", iso_code_country ,"_UMD_country_data.csv"))
    data_df <-
      read.csv(
        paste0(
          "../data/estimates-umd-unbatched/PlotData/",
          iso_code_country ,
          "_UMD_country_nobatch_past_smooth.csv"
        )
      )
    
    
    data_df$date <- as.Date(data_df$date)
    
    ## remove "..._smooth", "..._high/low"
    df_umd <- data_df[, str_detect(colnames(data_df), "pct_")]
    #df_umd <- df_umd[, !str_detect(colnames(df_umd), "smooth")]
    df_umd <- df_umd[, !str_detect(colnames(df_umd), "high")]
    df_umd <- df_umd[, !str_detect(colnames(df_umd), "low")]
    df_umd <- df_umd[, !str_detect(colnames(df_umd), "batched")]
    df_umd <- df_umd * data_df$population / 100
    df_umd$date <- data_df$date
    
    colnames(df_umd)
    
    ## Load CCFR regressors
    df_ccfr <-
      read.csv(
        paste0(
          "../data/estimates-ccfr-based/PlotData/",
          iso_code_country,
          "-estimate.csv"
        )
      ) %>%
      mutate(date = as.Date(date))
    
    ## Load NSUM regressors TODO
    
    #df_nsum <- read.csv(paste0("../data/estimates-ccfr-based/PlotData/", iso_code_country,"-estimate.csv")) %>%
    #  mutate(date = as.Date(date) )
    
    ## Load number of deaths ----
    
    df_deaths <-
      read.csv(
        paste0(
          "../data/estimates-confirmed/PlotData/",
          iso_code_country,
          "-estimate.csv"
        )
      ) %>%
      #df_deaths <- read.csv(paste0("../../coronasurveys/coronasurveys/data/estimates-ccfr-based/PlotData/", iso_code_country,"-estimate.csv")) %>%
      mutate(date = as.Date(date)) %>%
      mutate(y = deaths) %>%
      mutate(y = rollmean(y, 1, fill = NA)) %>%
      filter(!is.na(y)) 
    
    
    ## Correlations ----
    # compute all correlations for all lags in min_lag to max_lag:
    correls_single_country <- check_lags(
      df_response = df_deaths,
      df_add_regressors = df_umd,
      columns_to_try = columns_to_try,
      min_lag = 7,
      max_lag = 60)
    
    # extract lag with significant-maximum correlation by signal:
    opt_correl_single_country <- correls_single_country %>% 
      filter(pval <= 0.05) %>% 
      group_by(signal) %>% 
      filter(abs(correlations) == max(abs(correlations))) %>% 
      ungroup()
    
    # save optimal correlations:
    opt_correl_single_country$country <- iso_code_country
    opt_correls <- rbind(opt_correls, opt_correl_single_country)
    
    # plot of correlations for single country:
    ifelse(!dir.exists(file.path("../data/estimates-umd-unbatched/", "Plots")), 
           dir.create(file.path("../data/estimates-umd-unbatched/", "Plots")), FALSE)
    p <-
      ggplot(data = correls_single_country, aes(x = shift, y = correlations, color = signal)) +
      geom_line(alpha = 0.8) +
      ylim(-1, 1) +
      labs(title = iso_code_country) +
      theme_light()
    ggsave(plot = p, 
           filename = paste0(
             "../data/estimates-symptom-lags/Plots-Correlations/",
             iso_code_country,
             "-predictor-correlation.png"
           ), width = 10, height = 7
    )
    
    
  }, error = function(cond) {
    message(paste("error in country", iso_code_country))
  })
}

glimpse(opt_correls)

write.csv(opt_correls, file = paste0(
  "../data/estimates-symptom-lags/optimal-lags.csv"
))