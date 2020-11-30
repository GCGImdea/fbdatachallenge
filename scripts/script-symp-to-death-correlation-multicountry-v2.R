## Libraries
library(dplyr)
library(stringr)
library(zoo) # to use rollmean
library(foreign)
library(MASS)
library(ggplot2)
library(grid) # annotate a ggplot
library(caret)


remove_correlated = T
cutoff_remove_correlated = 0.9

check_lags <-
  function(df_response,
           df_add_regressors,
           columns_to_try,
           min_lag = 7,
           max_lag = 60) {
    df_response$date <- as.Date(df_response$date)
    
    df_out <- data.frame()
    
    for (column_in in columns_to_try) {
      for (date_shift in seq(min_lag, max_lag)) {
        # replaced these  df_single_symp <- df_add_regressors[, c("date", column_in)]
        # df_single_symp$date <-
        #  as.Date(df_single_symp$date) + date_shift
        # joined <- (df_single_symp %>% dplyr::select(date, column_in)) 
        # by the following
        joined <- df_add_regressors %>% dplyr::select(date, column_in) %>% mutate(date=as.Date(date)+date_shift)%>% inner_join( df_response, by="date")
        # remove NAs in what we want, we can do it because we only have column_in now
        joined  <- joined[complete.cases(joined), ]
        
        win_size <-
          as.integer(max(joined$date) - min(joined$date))
        
        # if (sum(is.na(joined[, column_in]))==0){
        tryCatch({
          corTest <-
            cor.test(joined$y, joined[, column_in], method = "spearman")
          
          df_correl <-
            data.frame(
              shift = date_shift,
              correlations = corTest$estimate,
              pval = corTest$p.value,
              win_size = win_size,
              signal = column_in
            )
        }, error = function(cond){
          message("Error in correlation: ")
          message(cond)
          traceback()
          df_correl <-
            data.frame(
              shift = date_shift,
              correlations = 0,
              pval = 1,
              win_size = win_size,
              signal = column_in
            )
        })
        df_out <- rbind(df_out, df_correl)
        # } else {
        #   # print(paste("got NAs for column ",column_in, " and date shift ", date_shift))
        # }#endif
        # 
        
      } # loop-date_shift
    } # loop-column_in
    
    
    return(df_out)
    
  } # end-check_lags

# columns_to_try = c(
#   "pct_anosmia_ageusia_past_smooth",
#   "pct_sore_throat_past_smooth",
#   "pct_fever_past_smooth",
#   "pct_cmnty_sick_past_smooth",
#   "pct_direct_contact_with_non_hh_past_smooth"
# )

columns_to_try <- c(# "pct_cli",
  # "pct_ili",
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
  "pct_enough_toEat_not_worried",
  "pct_chills",
  "pct_finances_very_worried",
  "pct_finances_somewhat_worried",
  "pct_finances_notToo_worried",
  "pct_finances_not_worried")


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
    # ifelse(!dir.exists(file.path("../data/estimates-umd-unbatched/", "Plots")), 
    #        dir.create(file.path("../data/estimates-umd-unbatched/", "Plots")), FALSE)
    p <-
      ggplot(data = correls_single_country, aes(x = shift, y = correlations, color = signal)) +
      geom_line(alpha = 0.8) +
      ylim(-1, 1) +
      labs(title = iso_code_country) +
      theme_light()
    # ggsave(plot = p, 
    #        filename = paste0(
    #          "../data/estimates-symptom-lags/Plots-Correlations/",
    #          iso_code_country,
    #          "-predictor-correlation.png"
    #        ), width = 10, height = 7
    # )
    
    # construct a single data frame with response and regressors:
    df_glm <- df_deaths %>% 
      dplyr::select(date, deaths)
    
    for (indep_var in unique(opt_correl_single_country$signal)) {
      
      df_indep_var_temp <- df_umd %>% 
        dplyr::select(date, all_of(indep_var)) %>% 
        mutate(date = date + 
                 opt_correl_single_country$shift[opt_correl_single_country$signal == indep_var]
        )
      df_glm <- df_glm %>% full_join(df_indep_var_temp, by = "date")
      
    }
    # get only complete cases (remove rows with NAs)
    df_glm <- df_glm[complete.cases(df_glm), ]
    
    ## Remove correlated vars----
    if (remove_correlated) {
      # plug in the train data frame:
      
      rm_high_correl <- findCorrelation(
        cor( dplyr::select(df_glm, !all_of(c("date", "deaths"))), 
             method = "spearman"),
        cutoff = cutoff_remove_correlated, 
        verbose = F,
        names = T,
        exact = T
      )
      
      # remove the variables from the analysis 
      df_glm <- df_glm %>% 
        dplyr::select(!all_of(rm_high_correl))
      
    } # end-if remove correlated
    
    ## GLM ----
    
    # # Lasso GLM-Negative Binomial:
    # library(mpath)
    # # m_lasso <- glmregNB(deaths ~ . -date , data = df_glm, parallel=TRUE, n.cores=2)
    # cv_lasso <- cv.glmregNB(deaths ~ . -date , data = df_glm, parallel=TRUE, n.cores=2)
    # coef(cv_lasso)
    # 
    # m_lasso <- glmregNB(deaths ~ . -date, lambda = cv_lasso$lambda.optim,
    #                     data = df_glm, parallel=TRUE, n.cores=2)
    # coef(m_lasso)
    # m1<- m_lasso
    
    # fit model (DO NOT USE DATE!!!):
    m1 <- glm.nb(deaths ~ . -date , data = df_glm)
    summary(m1)
    
    # Stepwise regression model
    m1 <- stepAIC(m1, direction = "both", 
                          trace = FALSE)
    # summary(m1)
    # m1$anova
    
    ## Plot + CI
    ## grab the inverse link function
    ilink <- family(m1)$linkinv
    ## add fit and se.fit on the **link** scale
    df_glm <- bind_cols(df_glm, setNames(as_tibble(predict(m1, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link'))) %>% 
      mutate(fit_resp  = ilink(fit_link),
             fit_resp_upr = ilink(fit_link + (2 * se_link)),
             fit_resp_lwr = ilink(fit_link - (2 * se_link)))
    
    # get the coefficients
    m1_coef <- m1[["coefficients"]]
    
    # construct a data frame to save results 
    # (original data, coeffs, pvals, AIC, deviance)
    df_glm_save <- df_glm %>% 
      dplyr::select(date, 
                    deaths, 
                    fit_resp,
                    fit_resp_upr,
                    fit_resp_lwr,
                    names(m1_coef)[-1])
    
    # save coefficients as coef_xxx_signal
    names(m1_coef) <- paste0("coef_", names(m1_coef) )
    for (each_coef in names(m1_coef)) {
      df_glm_save[ , each_coef] <- m1_coef[each_coef]
    }
    
    # save p-values as pvals_xxx_signal
    m1_pvals <- coef(summary(m1))[,4]
    names(m1_pvals) <- paste0( "pvals_", names(m1_pvals) )
    for (each_coef in names(m1_pvals)) {
      df_glm_save[ , each_coef] <- m1_pvals[each_coef]
    }
    
    
    # save AIC, deviance, null_deviance
    df_glm_save$aic <- m1[["aic"]]
    df_glm_save$deviance <- m1[["deviance"]]
    df_glm_save$null_deviance <- m1[["null.deviance"]]
    
    
    write.csv(x = df_glm_save, file = paste0(
      "../data/estimates-symptom-lags/PlotData/",
      iso_code_country,
      "-deaths-vs-predicted.csv"))
    
    ## Some Plots ----
    ## join with official deaths
    my_colors <- c("Official" = "red", 
                   "Estimated" = "blue")
    
    # grob <- grobTree(textGrob(, x=0.1,  y=0.95, hjust=0,
    #                           gp=gpar(col="black", fontsize=13, fontface="italic")))
    
    p_model <- ggplot(data = df_glm, aes(x = date) ) +
      geom_line(aes(y = fit_resp, colour = "Estimated"), size = 1, alpha = 0.8) +
      geom_point(aes(y = fit_resp, colour = "Estimated"), size = 1.5, alpha = 0.6) +
      geom_ribbon(aes(ymin = fit_resp_lwr, ymax = fit_resp_upr),
                  alpha = 0.1, fill = my_colors["Estimated"]) +
      geom_point(aes(y = deaths, colour = "Official"), size = 1.5, alpha = 0.6) +
      geom_line(aes(y = deaths, colour = "Official"), size = 0.2, alpha = 0.6) +
      scale_color_manual(values = my_colors) +
      labs(x = "Date", y =  "Number of deaths", title = iso_code_country,  colour = "") +
      theme_light(base_size = 15) +
      theme(legend.position="bottom") # + annotation_custom(grob)
    # p_model
    # ggsave(plot = p_model, 
    #        filename = paste0(
    #          "../data/estimates-symptom-lags/Plots-GLM/",
    #          iso_code_country,
    #          "-deaths-vs-predicted.png"
    #        ), width = 10, height = 7
    # )
    
    message("succeeded")
  }, error = function(cond) {
    message(paste("error in country", iso_code_country))
  })
}

# glimpse(opt_correls)

write.csv(opt_correls, file = paste0(
  "../data/estimates-symptom-lags/optimal-lags.csv"
))

# ## Study the stable signals:
# 
# files_model_path <- "../data/estimates-symptom-lags/PlotData/"
# files_model <- dir(files_model_path)
# 
# stable_regressors <- matrix(0, nrow = length(files_model), ncol = length(columns_to_try))
# colnames(stable_regressors) <- columns_to_try
# rownames(stable_regressors) <- str_sub(files_model, 1, 2)
# 
# for (country_model in files_model) {
#   temp <- read.csv(paste0(files_model_path, country_model)) %>% 
#     dplyr::select(!starts_with("pvals")) %>% 
#     dplyr::select(!starts_with("coef")) %>% 
#     dplyr::select(!contains("X")) %>% 
#     dplyr::select(!contains("aic")) %>% 
#     dplyr::select(!contains("deviance")) %>% 
#     dplyr::select(!contains("fit")) %>% 
#     dplyr::select(!contains("deaths")) %>% 
#     dplyr::select(!contains("date")) %>% 
#     colnames()
#   stable_regressors[str_sub(country_model, 1, 2), temp] = 1
#   
# }
# 
# times_stable_predictors <- sort(colSums(stable_regressors), decreasing = T)
# 
# times_stable_predictors <- tibble(regressor = names(times_stable_predictors),
#                                       times = times_stable_predictors)
# 
# head(times_stable_predictors)
# 
# 
# p_stable <- ggplot(times_stable_predictors, aes(x =regressor, y = times)) +
#   geom_col() +
#   theme_light(base_size = 15) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
# p_stable
# 
# ggsave(plot = p_stable, 
#        filename = paste0(
#          "../data/estimates-symptom-lags/stable-regressors.png"
#        ), width = 12, height = 10
# )
