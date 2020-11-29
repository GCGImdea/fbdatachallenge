## Libraries
library(dplyr)
library(stringr)
library(zoo) # to use rollmean
library(foreign)
library(MASS)
library(ggplot2)
library(grid) # annotate a ggplot
library(Metrics)
library(mpath) # lasso/elastic-net
library(caret)

use_penalty = T # T: use penalized regression (elastic-net)
alpha_in = 0.5 # tradeoff between Ridge and Lasso regression
remove_correlated = F # prior removal of highly correlated predictors
cutoff_remove_correlated = 0.9 # cutoff for remove_correlated

milag=7
mxlag=60
plotCorrel=FALSE
plotForecast=TRUE

signal_to_match <- "deaths"
#signal_to_match <- "cases"

basefileid <-paste0(signal_to_match,"-",milag,"-",mxlag,"-pen",use_penalty,"-alpha",alpha_in,"-rc",remove_correlated,"-co",cutoff_remove_correlated)
signals_umd <- c(
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
  #  "pct_chills",
  #  "pct_finances_very_worried",
  #  "pct_finances_somewhat_worried",
  #  "pct_finances_notToo_worried",
  #  "pct_finances_not_worried"
)

signals_umd_past_smooth <- c(
  "pct_fever_past_smooth",
  "pct_cough_past_smooth",
  "pct_difficulty_breathing_past_smooth",
  "pct_fatigue_past_smooth",
  "pct_stuffy_runny_nose_past_smooth",
  "pct_aches_muscle_pain_past_smooth",
  "pct_sore_throat_past_smooth",
  "pct_chest_pain_past_smooth",
  "pct_nausea_past_smooth",
  "pct_anosmia_ageusia_past_smooth",
  "pct_eye_pain_past_smooth",
  "pct_headache_past_smooth",
  "pct_cmnty_sick_past_smooth",
  "pct_ever_tested_past_smooth",
  "pct_tested_recently_past_smooth",
  "pct_worked_outside_home_past_smooth",
  "pct_grocery_outside_home_past_smooth",
  "pct_ate_outside_home_past_smooth",
  "pct_spent_time_with_non_hh_past_smooth",
  "pct_attended_public_event_past_smooth",
  "pct_used_public_transit_past_smooth",
  "pct_direct_contact_with_non_hh_past_smooth",
  "pct_wear_mask_all_time_past_smooth",
  "pct_wear_mask_most_time_past_smooth",
  "pct_wear_mask_half_time_past_smooth",
  "pct_wear_mask_some_time_past_smooth",
  "pct_wear_mask_none_time_past_smooth",
  "pct_no_public_past_smooth",
  "pct_feel_nervous_all_time_past_smooth",
  "pct_feel_nervous_most_time_past_smooth",
  "pct_feel_nervous_some_time_past_smooth",
  "pct_feel_nervous_little_time_past_smooth",
  "pct_feel_nervous_none_time_past_smooth",
  "pct_feel_depressed_all_time_past_smooth",
  "pct_feel_depressed_most_time_past_smooth",
  "pct_feel_depressed_some_time_past_smooth",
  "pct_feel_depressed_little_time_past_smooth",
  "pct_feel_depressed_none_time_past_smooth",
  "pct_worried_ill_covid19_very_past_smooth",
  "pct_worried_ill_covid19_somewhat_past_smooth",
  "pct_worried_ill_covid19_notTooWorried_past_smooth",
  "pct_worried_ill_covid19_notWorried_past_smooth",
  "pct_enough_toEat_very_worried_past_smooth",
  "pct_enough_toEat_somewhat_worried_past_smooth",
  "pct_enough_toEat_notToo_worried_past_smooth",
  "pct_enough_toEat_not_worried_past_smooth"
  #  "pct_chills",
  #  "pct_finances_very_worried",
  #  "pct_finances_somewhat_worried",
  #  "pct_finances_notToo_worried",
  #  "pct_finances_not_worried"
)


signals_ccfr <- c(
  "cases",
  "cases_daily",
  "cases_active",
  "cases_contagious"
)

signals_nsum <- c(
  "p_cases",
  "p_cases_recent",
  "p_cases_fatalities",
  "p_cases_stillsick"
)

#signals_to_try <- c(signals_umd,signals_ccfr) 
#signals_to_try <- c(signals_umd,signals_ccfr,signals_nsum) 
#signals_to_try <- signals_nsum
#signals_to_try <- signals_ccfr
#signals_to_try <- signals_umd_past_smooth
signals_to_try <- signals_umd

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


doCorrelations <-
  function(toPredict,
           modelPar,
           columns_to_try,
           iso_code_country) {
    ## Correlations ----
    # compute all correlations for all lags in min_lag to max_lag:
    #    print(paste("columns to try size",length(columns_to_try)))
    correls_single_country <- check_lags(
      df_response = toPredict,
      df_add_regressors = modelPar,
      columns_to_try = columns_to_try,
      min_lag = milag,
      max_lag = mxlag
    )
    #print(paste("correls_single_country size",nrow(correls_single_country)))
    # extract lag with significant-maximum correlation by signal:----
    opt_correl_single_country <- correls_single_country %>%
      filter(pval <= 0.05) %>%
      group_by(signal) %>%
      filter(abs(correlations) == max(abs(correlations))) %>%
      ungroup()
    #print(paste("opt_correls_single_country size",nrow(opt_correl_single_country)))
    
    # opt_correl_single_country <-
    #   opt_correl_single_country[-duplicated(opt_correl_single_country[ , c("correlations", "signal")]),]
    
    # save optimal correlations:
    opt_correl_single_country$country <- iso_code_country
    opt_correls <- rbind(opt_correls, opt_correl_single_country)
    
    
    if (plotCorrel){
      # plot of correlations for single country:
      ifelse(!dir.exists(file.path(
        "../data/estimates-umd-unbatched/", "Plots"
      )),
      dir.create(file.path(
        "../data/estimates-umd-unbatched/", "Plots"
      )), FALSE)
      p <-
        ggplot(data = correls_single_country, aes(x = shift, y = correlations, color = signal)) +
        geom_line(alpha = 0.8) +
        ylim(-1, 1) +
        labs(title = iso_code_country) +
        theme_light()
      ggsave(
        plot = p,
        filename = paste0(
          "../data/estimates-symptom-lags/Plots-Correlations/",
          fileid,
          "-predictor-correlation.png"
        ),
        width = 10,
        height = 7
      )
    }# if plotCorrel
    #   print(paste("opt_correls_single_country size",nrow(opt_correl_single_country)))
    #    print(unique(opt_correl_single_country$signal))
    return (opt_correl_single_country)
  }



shiftSignals <- function(baseForOutputDF, inputDF, correl) {
  for (indep_var in unique(correl$signal)) {
    df_indep_var_temp <- inputDF %>%
      dplyr::select(date, all_of(indep_var)) %>%
      mutate(date = date +
               correl$shift[correl$signal == indep_var])
    #  print("iteration")
    # print(df_indep_var_temp)
    baseForOutputDF <- baseForOutputDF %>% full_join(df_indep_var_temp, by = "date")
    
  }
  return (baseForOutputDF)
}

doGLM <-
  function(modelPar,
           iso_code_country) {
    
    
    modelPar <- modelPar[complete.cases(modelPar), ]
    
    
    m1 <- glm.nb(y ~ . - date , data = modelPar)
    
    #summary(m1)
    
    # Stepwise regression model
    m1 <- stepAIC(m1, direction = "both",
                  trace = FALSE)
    #summary(m1)
    #m1$anova
    
    ## Plot + CI
    ## grab the inverse link function
    
    return (m1)
    
    
  }

doGLM_penalty <-
  function(modelPar,
           iso_code_country) {
    
    
    modelPar <- modelPar[complete.cases(modelPar), ]
    
    # CV elastic-net
    cv_enet <- cv.glmregNB(y ~ . -date , 
                           data = modelPar, 
                           parallel=TRUE, 
                           n.cores=2,
                           alpha = alpha_in,
                           plot.it = F,
                           nfolds = 5)
    
    m_enet <- glmregNB(y ~ . -date, 
                       lambda = cv_enet$lambda.optim,
                       data = modelPar, 
                       parallel=TRUE, n.cores=2,
                       plot.it = F,
                       alpha = alpha_in)
    
    return (m_enet)
    
    
  }

doPrediction <- function(m, testSignals) {
  
  
  ## grab the inverse link function
  ilink <- family(m)$linkinv
  ## add fit and se.fit on the **link** scale
  prediction <- bind_cols(testSignals %>% dplyr::select(date),
                          setNames(as_tibble(predict(
                            m, testSignals, se.fit = TRUE
                          )[1:2]),
                          c('fit_link', 'se_link'))) %>%
    mutate(
      fore  = ilink(fit_link),
      fore_low = ilink(fit_link + (2 * se_link)),
      fore_high = ilink(fit_link - (2 * se_link))
    )
  return (prediction)
}

doPrediction_penalty <- function(m, testSignals) {
  
  
  ## add fit and avoid CIs
  prediction <- bind_cols(
    testSignals %>% dplyr::select(date), 
    setNames(as_tibble( predict(m, testSignals, type = "response") ),
             'fit_link' )) %>%
    mutate(
      se_link = NA,
      fore = fit_link,
      fore_low = NA,
      fore_high = NA
    )
  return (prediction)
}

getMinAndMaxDatesAsString <- function (df){
  mx=max(df$date)
  mn=min(df$date)
  return (paste("from",mn, "to",mx , "window",(mx-mn)))
}

doTest <- function(m, testSignals, testResp, cutoff, metricDF) {
  # keep only kept regressors and date
  if (use_penalty){
    nonzeros<-rownames(data.frame(coeffs = coef(m)) %>% filter(coeffs!=0))
    nonzeros <- c(nonzeros,"date")
    allcols <-names(testSignals)
    toZero <- setdiff(allcols, nonzeros)
    testSignals[,toZero]<-0 
  } else {
    testSignals <-
      testSignals[, (names(testSignals) %in% c(labels(m$terms),"date"))]
  }
 
  # get only complete cases (remove rows with NAs)
  testResp<-testResp[complete.cases(testResp), ] 
  testSignals <- testSignals[complete.cases(testSignals), ]
  minTestDate <- min(testSignals$date)
  maxTestDate <- max(testSignals$date)
  testResp <- testResp %>% filter(date >= minTestDate, date <= maxTestDate)
  
  joined <- testSignals %>% inner_join(testResp, by="date")
  testSignals <- joined[,names(joined) %in% names(testSignals)]
  testResp <- joined[,names(joined) %in% names(testResp)]
  
  firstdate=min(joined$date)
  finaldate=max(joined$date)
  testWindow = finaldate - firstdate
  
  if (use_penalty) {
    prediction=doPrediction_penalty(m=m,testSignals = testSignals)
  }else {
    prediction=doPrediction(m=m,testSignals = testSignals)
  }
  
  # print(prediction)
  toWrite <-prediction %>% dplyr::select(date, fore, fore_low, fore_high)
  toWrite <- toWrite %>% left_join(testResp %>% dplyr::select(date, y), by="date")
  metricDF[1,"startDate"] <-firstdate
  metricDF[1,"endDate"] <-finaldate
  metricDF[1,"mape"] <- mape(testResp$y, prediction$fore)
  metricDF[1,"mae"] <- mae(testResp$y, prediction$fore)
  metricDF[1,"mse"] <- mse(testResp$y, prediction$fore)
  metricDF[1,"rmse"] <- rmse(testResp$y, prediction$fore)
  # metricDF$nrmse <- nrmse(testResp$y, prediction$fore)
  metricDF[1,"smape"] <-smape(testResp$y, prediction$fore)
  
  
  
  
  # print(paste("results for cutoff ", as.Date(cutoff)," from ", firstdate, " to ", finaldate, " window ", testWindow))
  # print(paste("mape=", mape))
  # print(paste("smape=", smape))
  # print(paste("mae=", mae))
  # print(paste("mse=", mse))
  # print(paste("rmse=", rmse1))
  # print(paste("rmse=", rmse2))
  #
  #
  if (plotForecast){
    
    
    # Add target for plotting
    x_plot_df <- prediction %>% full_join(y_df, by="date")
    # x_plot_df <- x_plot_df %>% full_join(y_pre_df, by="date")
    # x_plot_df <- x_plot_df %>% full_join(y_post_df, by="date")
    # Add a smoothed response
    x_plot_df$fore_smooth <- rollmean(x_plot_df$fore, 1, fill = NA)
    
    
    my_colors <- c("Actual Value" = "red", 
                   "Estimated" = "blue", "Post" = "black")
    
    
    p_model <- ggplot(data = x_plot_df, aes(x = date) ) +
      geom_line(aes(y = fore_smooth, colour = "Estimated"), size = 1, alpha = 0.8) +
      geom_point(aes(y = fore_smooth, colour = "Estimated"), size = 1.5, alpha = 0.6)
    if (!use_penalty) {
      p_model <- p_model + geom_ribbon(aes(ymin = fore_low, ymax = fore_high),
                                       alpha = 0.1, fill = my_colors["Estimated"]) 
    }
    p_model <- p_model +
      geom_point(aes(y = y, colour = "Fitting"), size = 1.5, alpha = 0.6) +
      geom_line(aes(y = y, colour = "Fitting"), size = 0.2, alpha = 0.6) +
      # geom_point(aes(y = y_post, colour = "Post"), size = 1.5, alpha = 0.6) +
      # geom_line(aes(y = y_post, colour = "Post"), size = 0.2, alpha = 0.6) +     
      # scale_color_manual(values = my_colors) +
      labs(x = "Date", y =  paste("Number of ",signal_to_match), title = iso_code_country,  colour = "") +
      theme_light(base_size = 15) +
      theme(legend.position="bottom") # + annotation_custom(grob)
    # p_model
    ggsave(plot = p_model, 
           filename = paste0(
             "../data/estimates-symptom-lags/cutoffs/Plots/",
             fileid,
             "-forecast.png"
           ), width = 10, height = 7
    )
    
    # ## join with official deaths
    # my_colors <- c("Official" = "red",
    #                "Estimated" = "blue")
    #
    # # grob <- grobTree(textGrob(, x=0.1,  y=0.95, hjust=0,
    # #                           gp=gpar(col="black", fontsize=13, fontface="italic")))
    #
    # p_model <- ggplot(data = df_glm, aes(x = date) ) +
    #   geom_line(aes(y = fore, colour = "Estimated"), size = 1, alpha = 0.8) +
    #   geom_point(aes(y = fore, colour = "Estimated"), size = 1.5, alpha = 0.6) +
    #   geom_ribbon(aes(ymin = fore_low, ymax = fore_high),
    #               alpha = 0.1, fill = my_colors["Estimated"]) +
    #   geom_point(aes(y = deaths, colour = "Official"), size = 1.5, alpha = 0.6) +
    #   geom_line(aes(y = deaths, colour = "Official"), size = 0.2, alpha = 0.6) +
    #   scale_color_manual(values = my_colors) +
    #   labs(x = "Date", y =  "Number of deaths", title = iso_code_country,  colour = "") +
    #   theme_light(base_size = 15) +
    #   theme(legend.position="bottom") # + annotation_custom(grob)
    # # p_model
    # ggsave(plot = p_model,
    #        filename = paste0(
    #          "../data/estimates-symptom-lags/Plots-GLM/",
    #          iso_code_country,
    #          "-deaths-vs-predicted.png"
    #        ), width = 10, height = 7
    # )
  }
  return (toWrite)
}


y <- signal_to_match

#file_in_path <- "../data/estimates-umd-unbatched/PlotData/"
#file_in_pattern <- ".*_UMD_country_nobatch_past_smooth.csv"

file_in_path <- "../data/all_giant_df/"
file_in_pattern <- ".*alldf.csv"

files <- dir(file_in_path, pattern = file_in_pattern)

countriesToExclude <- c("") # c("AT","BG")
countriesDone <- c("") # c("AE","AF","AM","AO","AR","AU","AZ","BD","BE","BO","BR","BY","CA","CL","CO","CR","DE","DO","DZ","EG","FR","GB","GH","GR","GT","HN","HR","HU","ID","IL","IN","IQ","JP","KE","KR","KW","LB","LY","MA","MD","MX","NG","NI","NL","NP","NZ","PA","PH","PK","PL","PR","PS","PT","QA","RO","RS","RU","SA","SD","SE","SG","SV","TR","UA","UZ","VE","ZA")
countriesToExclude <- c(countriesToExclude, countriesDone)
countriesToDo <-c("BR", "DE", "EC", "PT", "UA", "ES", "IT", "CL", "FR", "GB")
opt_correls <- data.frame()

excludeVsChoose=FALSE # true for excluding countries and false for choosing them



for (file in files) {
  tryCatch({
    iso_code_country <- substr(file, 1, 2)
    if (excludeVsChoose){
      choice <- (iso_code_country  %notin% countriesToExclude)
    } else {
      choice <- (iso_code_country  %in% countriesToDo)
    }
    if (choice){
      cat("doing ", iso_code_country, ": ")
      all_df <- read.csv(paste0("../data/all_giant_df/", iso_code_country, "_alldf.csv"))
      all_df <- all_df %>% mutate(date = as.Date(date))
      y <- signal_to_match
      y_df <- all_df %>% dplyr::select(date,y)
      colnames(y_df) <- c("date","y")
      
      x_df <- all_df %>% dplyr::select(date)
      for (signal in signals_to_try)
      {
        s_df <- all_df %>% dplyr::select(date,signal)
        x_df <- x_df %>% full_join(s_df, by = "date")
      }
      
      # I think the following line should not be done commented out
      # x_df <- x_df[complete.cases(x_df), ]
          
      # ## remove "..._smooth", "..._high/low"
      # x_df <- data_df[, str_detect(colnames(data_df), "pct_")]
      # #x_df <- x_df[, !str_detect(colnames(x_df), "smooth")]
      # x_df <- x_df[, !str_detect(colnames(x_df), "high")]
      # x_df <- x_df[, !str_detect(colnames(x_df), "low")]
      # x_df <- x_df[, !str_detect(colnames(x_df), "batched")]
      # x_df <- x_df * data_df$population / 100
      # x_df$date <- data_df$date
      # 
      # colnames(x_df)
      
     
      
     
      
      # y_df <-
      #   read.csv(
      #     paste0(
      #       "../data/estimates-confirmed/PlotData/",
      #       iso_code_country,
      #       "-estimate.csv"
      #     )
      #   ) %>% mutate(date = as.Date(date))
      # y_df$deaths[y_df$deaths < 0] <- NA
      # y_df$cases[y_df$cases < 0] <- NA
      
      # y_df <- y_df %>%
      #   mutate(y = deaths) %>%
      #   mutate(y = rollmean(y, 1, fill = NA)) %>%
      #   dplyr::select (date, y) %>%
      #   filter(!is.na(y)) # keeping only what we need so that we can filter out NAs as follows
      # #########
      # # filter out NAs
      # y_df <- y_df[complete.cases(y_df), ]
      
      ### compute cutoffs, start from last date in signals and progress backwards every 15 days until firstCutoff
      
      firstCutoff <- as.Date("2020-9-10")
      lastCutoff <- as.Date("2020-11-10")
      
      cutoff <- lastCutoff
      cutoffs <- vector()
      while (cutoff >= firstCutoff) {
        cutoffs <- append(cutoffs, cutoff, after = 0)
        cutoff <- cutoff - 1
        #print(cutoffs)
      }
      
      #
      #  print (cutoffs)
      
      # print(y_df)
      strawmanPred=(y_df %>% filter(date==cutoff))$y
      toWrite <- data.frame()
      metricsToWrite<-data.frame()
      for (cutoff in cutoffs) {
        fileid <- paste0(iso_code_country,"-",basefileid,"-",as.Date(cutoff))
        cutoff=as.Date(cutoff)
        tryCatch({
          # dplyr::select training set
          y_df_train <-
            y_df %>% filter(date <= cutoff) 
          x_df_train <-
            x_df %>% filter(date <= cutoff)
          
          # dplyr::select test set
          y_df_test <-
            y_df %>% filter(date > cutoff)
          x_df_test <-
            x_df %>% filter(date > cutoff)
          
          # call Correl ----
          print(paste("doing CORREL for cutoff ",as.Date(cutoff)))
          opt_correl_single_country <-
            doCorrelations(
              toPredict = y_df_train,
              modelPar = x_df_train,
              columns_to_try = signals_to_try,
              iso_code_country = iso_code_country
            )
          # Shift train signals 
          shiftedSignals <- shiftSignals(baseForOutputDF=(y_df_train %>% dplyr::select(date, y)), inputDF=x_df_train, correl=opt_correl_single_country)
          
          # keep only signals before cutoff after shifting, this is unnecessary but it does not hurt
          shiftedTrainSignal <- shiftedSignals %>% filter(date <= cutoff)
          
          leftoverFromShifted <- shiftedSignals %>% filter(date > cutoff) %>% dplyr::select(-y)
          
          ## Remove correlated vars----
          if (remove_correlated) {
            # plug in the train data frame:
            temp_shiftedTrainSignal <- shiftedTrainSignal[complete.cases(shiftedTrainSignal), ]
            rm_high_correl <- findCorrelation(
              cor( dplyr::select(temp_shiftedTrainSignal, !all_of(c("date", "y"))), 
                   method = "spearman"),
              cutoff = cutoff_remove_correlated, 
              verbose = F,
              names = T,
              exact = T
            )
            
            # remove the variables from the analysis (bot train and test?)
            shiftedTrainSignal <- shiftedTrainSignal %>% 
              dplyr::select(!all_of(rm_high_correl))
            leftoverFromShifted <- leftoverFromShifted %>% 
              dplyr::select(!all_of(rm_high_correl))
          } # end-if remove correlated
          
          
          ## call GLM ----
          
          #print (opt_correl_single_country, n=Inf)
          
          if (use_penalty) {
            print(paste("doing penalized GLM for cutoff ", as.Date(cutoff)))
            m <-
              doGLM_penalty(
                modelPar = shiftedTrainSignal,
                iso_code_country = iso_code_country
              )
          }else {
            print(paste("doing GLM for cutoff ", as.Date(cutoff)))
            m <-
              doGLM(
                modelPar = shiftedTrainSignal,
                iso_code_country = iso_code_country
              )
          }
          
          print(paste("doing TESTING for cutoff ", as.Date(cutoff)))
          # call Test ----
          # but before shift the test set to the future We will remove NAs from x_df_test later
          shiftedTest <- shiftSignals(baseForOutputDF=(x_df_test %>% dplyr::select(date)),
                                      inputDF=x_df_test, correl=opt_correl_single_country)
          
          # remove the highly correlated signals:
          if (remove_correlated) {
            shiftedTest <- shiftedTest %>% 
              dplyr::select(!all_of(rm_high_correl))
          } # end-if remove correlated
          
          #  print(paste("now iterating through ",unique(opt_correl_single_country$signal)))
          
          
        
          # restrict as suggested by carlos. Complete cases is done in doTest
          # Not doing it here it was already in doTest
          # leftoverFromShifted <- leftoverFromShifted[ , (names(leftoverFromShifted) %in% names)]
          # shiftedTest <- shiftedTest[ , (names(shiftedTest) %in% names)]
          tryCatch({
          print(paste("prepared leftover test signals", getMinAndMaxDatesAsString(leftoverFromShifted)))
          metricDF<-data.frame()
          metricDF[1,"cutoff"]=as.Date(cutoff)
          metricDF[1,"predType"]="nearFuture"
          outDF=doTest(m = m, 
                       testSignals = leftoverFromShifted,
                       testResp = y_df_test,
                       cutoff = cutoff,
                       metricDF = metricDF)
          if (nrow(outDF)>0){
            outDF["cutoff"]=as.Date(cutoff)
            outDF["strawman"]=NA
            outDF["scaled_abs_err"]=NA
            for (row in 1:nrow(outDF)) {
              ourEstimate <- outDF[row, "fore"]
              curdate  <- outDF[row, "date"]
              strawmandev <- abs(strawmanPred - (y_df_test %>% filter(date == curdate))$y)+10^-6
              
              modeldev <- abs(ourEstimate - (y_df_test %>% filter(date == curdate))$y)
              scaled_abs_err <- modeldev/strawmandev
              outDF[row,"strawman"]=strawmanPred
              outDF[row,"scaled_abs_err"]=scaled_abs_err
              # message(paste(“sca 7 =“,scaled_abs_err))
            }
            outDF["predType"]="nearFuture"
            
            toWrite<-rbind(toWrite,outDF)
            metricsToWrite<-rbind(metricsToWrite, metricDF)
          }
          
          }, error=function(cond){
            message(paste("error in country", iso_code_country, " nearFuture for cutoff ", as.Date(cutoff)))
            message(cond)
            traceback()
          })
          tryCatch({
            
          # doing far future test
          
          print(paste("prepared shifted test signals", getMinAndMaxDatesAsString(shiftedTest)))
          metricDF<-data.frame()
          metricDF[1,"cutoff"]=as.Date(cutoff)
          metricDF[1,"predType"]="farFuture"
          outDF=doTest(m = m,
                       testSignals = shiftedTest,
                       testResp = y_df_test,
                       cutoff=cutoff,
                       metricDF = metricDF)
          if (nrow(outDF)>0){
            outDF["cutoff"]=as.Date(cutoff)
            outDF["strawman"]=NA
            outDF["scaled_abs_err"]=NA
            outDF["predType"]="farFuture"
            metricsToWrite<-rbind(metricsToWrite, metricDF)
            toWrite<-rbind(toWrite,outDF)
          }
          
          }, error=function(cond){
            message(paste("error in country", iso_code_country, " farFuture for cutoff ", as.Date(cutoff)))
            message(cond)
            traceback()
          })
          tryCatch({
          
          # do nearfar  test
          
          combinedSignals = rbind(leftoverFromShifted, shiftedTest)
          print(paste("prepared combined signals", getMinAndMaxDatesAsString(combinedSignals)))
          metricDF<-data.frame()
          metricDF[1,"cutoff"]=as.Date(cutoff)
          metricDF[1,"predType"]="nearFar"
          outDF=doTest(m = m,
                       testSignals = combinedSignals,
                       testResp = y_df_test,
                       cutoff=cutoff,
                       metricDF = metricDF)
          if (nrow(outDF)>0){
            outDF["cutoff"]=as.Date(cutoff)
            outDF["strawman"]=NA
            outDF["scaled_abs_err"]=NA
            outDF["predType"]="nearFar"
            metricsToWrite<-rbind(metricsToWrite, metricDF)
            toWrite<-rbind(toWrite,outDF)
          }
          }, error=function(cond){
            message(paste("error in country", iso_code_country, " nearFar for cutoff ", as.Date(cutoff)))
            message(cond)
            traceback()
          })
          
        },
        error = function(cond) {
          message(paste("error in country", iso_code_country, " for cutoff ", as.Date(cutoff)))
          message(cond)
          traceback()
        })
      }
      tryCatch({
        toWrite["lag"]=toWrite["date"]-toWrite["cutoff"]
        #"","date","fore","fore_low","fore_high","cutoff","strawman","scaled_abs_err","predType","lag"
        toWrite<-toWrite[,c(6,1,10,9,2,3,4,5,7,8)]
        write.csv(toWrite,file = paste0("../data/estimates-symptom-lags/cutoffs/PlotData/",fileid,"-estimates-lag-daily.csv"))
        write.csv(metricsToWrite,file = paste0("../data/estimates-symptom-lags/cutoffs/PlotData/",fileid,"-aggregatemetrics-lag.csv"))
        message("succeeded")
      }, error=function(cond){
        message(paste("error writing country ",iso_code_country))
        message (cond)
        traceback()
      })
      
    }
  }
  ,error = function(cond){
    message(paste("error in country", iso_code_country))
    message(cond)
    traceback()
  })
}


# glimpse(opt_correls)

# write.csv(opt_correls,
#           file = paste0("../data/estimates-symptom-lags/optimal-lags.csv"))
