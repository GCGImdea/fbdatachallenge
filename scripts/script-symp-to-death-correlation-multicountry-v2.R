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
    cat("doing ", iso_code_country, ": ")
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
    
    # opt_correl_single_country <- 
    #   opt_correl_single_country[-duplicated(opt_correl_single_country[ , c("correlations", "signal")]),]
    
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
    
    ## GLM ----
    
    # contruct a single data frame with response and regressors:
    df_glm <- df_deaths %>% 
      select(date, deaths)
    
    for (indep_var in unique(opt_correl_single_country$signal)) {
      
      df_indep_var_temp <- df_umd %>% 
        select(date, all_of(indep_var)) %>% 
        mutate(date = date + 
                 opt_correl_single_country$shift[opt_correl_single_country$signal == indep_var]
               )
      df_glm <- df_glm %>% full_join(df_indep_var_temp, by = "date")
      
    }
    # get only complete cases (remove rows with NAs)
    df_glm <- df_glm[complete.cases(df_glm), ]
    
    # fit model (DO NOT USE DATE!!!):
    m1 <- glm.nb(deaths ~ . -date , data = df_glm)
    summary(m1)
    
    ## Plot + CI
    ## grab the inverse link function
    ilink <- family(m1)$linkinv
    ## add fit and se.fit on the **link** scale
    df_glm <- bind_cols(df_glm, setNames(as_tibble(predict(m1, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link'))) %>% 
      mutate(fit_resp  = ilink(fit_link),
             right_upr = ilink(fit_link + (2 * se_link)),
             right_lwr = ilink(fit_link - (2 * se_link)))
      
    ## join with official deaths
    my_colors <- c("Official" = "red", 
                   "Estimated" = "blue")
    
    p_model <- ggplot(data = df_glm, aes(x = date) ) +
      geom_line(aes(y = fit_resp, colour = "Estimated"), size = 1, alpha = 0.8) +
      geom_point(aes(y = fit_resp, colour = "Estimated"), size = 1.5, alpha = 0.6) +
      geom_ribbon(aes(ymin = right_lwr, ymax = right_upr),
                  alpha = 0.1, fill = my_colors["Estimated"]) +
      geom_point(aes(y = deaths, colour = "Official"), size = 1.5, alpha = 0.6) +
      geom_line(aes(y = deaths, colour = "Official"), size = 0.2, alpha = 0.6) +
      scale_color_manual(values = my_colors) +
      labs(x = "Date", y =  "Number of deaths", title = iso_code_country,  colour = "") +
      theme_light(base_size = 15) +
      theme(legend.position="bottom")
    # p_model
    ggsave(plot = p_model, 
           filename = paste0(
             "../data/estimates-symptom-lags/Plots-GLM/",
             iso_code_country,
             "-deaths-vs-predicted.png"
           ), width = 10, height = 7
    )
    
    message("succeeded")
  }, error = function(cond) {
    message(paste("error in country", iso_code_country))
  })
}

# glimpse(opt_correls)

write.csv(opt_correls, file = paste0(
  "../data/estimates-symptom-lags/optimal-lags.csv"
))