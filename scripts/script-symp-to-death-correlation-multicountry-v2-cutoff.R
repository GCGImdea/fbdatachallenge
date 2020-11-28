## Libraries
library(dplyr)
library(stringr)
library(zoo) # to use rollmean
library(foreign)
library(MASS)
library(ggplot2)
library(grid) # annotate a ggplot
library(Metrics)
milag=7
mxlag=60
plotCorrel=FALSE
plotForecast=FALSE


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
          iso_code_country,
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



getMinAndMaxDatesAsString <- function (df){
  mx=max(df$date)
  mn=min(df$date)
  return (paste("from",mn, "to",mx , "window",(mx-mn)))
}

doTest <- function(m, testSignals, testResp, cutoff, metricDF) {
  # keep only kept regressors and date
  testSignals <-
    testSignals[, (names(testSignals) %in% c(labels(m$terms), "date"))]
  # get only complete cases (remove rows with NAs)
  testResp<-testResp[complete.cases(testResp), ] # this is probably redundant because we did it in the main
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
  prediction=doPrediction(m=m,testSignals = testSignals)
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
# columns_to_try = c(
#   "pct_anosmia_ageusia_past_smooth",
#   "pct_sore_throat_past_smooth",
#   "pct_fever_past_smooth",
#   "pct_cmnty_sick_past_smooth",
#   "pct_direct_contact_with_non_hh_past_smooth"
# )

columns_to_try <- c(
  # "pct_cli",
  # "pct_ili",
  ########## the two above were not used
  # "pct_fever",
  # "pct_cough",
  # "pct_difficulty_breathing",
  # "pct_fatigue",
  # "pct_stuffy_runny_nose",
  # "pct_aches_muscle_pain",
  # "pct_sore_throat",
  # "pct_chest_pain",
  # "pct_nausea",
  # "pct_anosmia_ageusia",
  # "pct_eye_pain",
  # "pct_headache",
  # "pct_cmnty_sick",
  # "pct_ever_tested",
  # "pct_tested_recently",
  # "pct_worked_outside_home",
  # "pct_grocery_outside_home",
  # "pct_ate_outside_home",
  # "pct_spent_time_with_non_hh",
  # "pct_attended_public_event",
  # "pct_used_public_transit",
  # "pct_direct_contact_with_non_hh",
  # "pct_wear_mask_all_time",
  # "pct_wear_mask_most_time",
  # "pct_wear_mask_half_time",
  # "pct_wear_mask_some_time",
  # "pct_wear_mask_none_time",
  # "pct_no_public",
  # "pct_feel_nervous_all_time",
  # "pct_feel_nervous_most_time",
  # "pct_feel_nervous_some_time",
  # "pct_feel_nervous_little_time",
  # "pct_feel_nervous_none_time",
  # "pct_feel_depressed_all_time",
  # "pct_feel_depressed_most_time",
  # "pct_feel_depressed_some_time",
  # "pct_feel_depressed_little_time",
  # "pct_feel_depressed_none_time",
  # "pct_worried_ill_covid19_very",
  # "pct_worried_ill_covid19_somewhat",
  # "pct_worried_ill_covid19_notTooWorried",
  # "pct_worried_ill_covid19_notWorried",
  # "pct_enough_toEat_very_worried",
  # "pct_enough_toEat_somewhat_worried",
  # "pct_enough_toEat_notToo_worried",
  # "pct_enough_toEat_not_worried",
  # "pct_chills",
  # "pct_finances_very_worried",
  # "pct_finances_somewhat_worried",
  # "pct_finances_notToo_worried",
  # "pct_finances_not_worried"
  
  ##### the following are those carlos was using. 
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
)


file_in_path <- "../data/estimates-umd-unbatched/PlotData/"
file_in_pattern <- ".*_UMD_country_nobatch_past_smooth.csv"

files <- dir(file_in_path, pattern = file_in_pattern)

countriesToExclude <- c("") # c("AT","BG")
countriesDone <- c("") # c("AE","AF","AM","AO","AR","AU","AZ","BD","BE","BO","BR","BY","CA","CL","CO","CR","DE","DO","DZ","EG","FR","GB","GH","GR","GT","HN","HR","HU","ID","IL","IN","IQ","JP","KE","KR","KW","LB","LY","MA","MD","MX","NG","NI","NL","NP","NZ","PA","PH","PK","PL","PR","PS","PT","QA","RO","RS","RU","SA","SD","SE","SG","SV","TR","UA","UZ","VE","ZA")
countriesToExclude <- c(countriesToExclude, countriesDone)
countriesToDo <-c("PT")
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
        ) %>% mutate(date = as.Date(date))
      df_deaths$deaths[df_deaths$deaths < 0] <- NA
      df_deaths$cases[df_deaths$cases < 0] <- NA
      
      df_deaths <- df_deaths %>%
        mutate(y = deaths) %>%
        mutate(y = rollmean(y, 1, fill = NA)) %>%
        dplyr::select (date, y) %>%
        filter(!is.na(y)) # keeping only what we need so that we can filter out NAs as follows
      #########
      # filter out NAs
      df_deaths <- df_deaths[complete.cases(df_deaths), ]
      
      ### compute cutoffs, start from last date in signals and progress backwards every 15 days until firstCutoff
      
      firstCutoff <- as.Date("2020-11-10")
      endDate <- max(df_umd$date)
      
      cutoff <- endDate
      cutoffs <- vector()
      while (cutoff >= firstCutoff) {
        cutoffs <- append(cutoffs, cutoff, after = 0)
        cutoff <- cutoff - 15
        #print(cutoffs)
      }
      
      #
      #  print (cutoffs)
      
      # print(df_deaths)
      strawmanPred=(df_deaths %>% filter(date==cutoff))$y
      toWrite <- data.frame()
      metricsToWrite<-data.frame()
      for (cutoff in cutoffs) {
        tryCatch({
          # dplyr::select training set
          df_deaths_train <-
            df_deaths %>% filter(date <= cutoff) 
          df_umd_train <-
            df_umd %>% filter(date <= cutoff)
          
          # dplyr::select test set
          df_deaths_test <-
            df_deaths %>% filter(date > cutoff)
          df_umd_test <-
            df_umd %>% filter(date > cutoff)
          
          # call Correl ----
          print(paste("doing CORREL for cutoff ",as.Date(cutoff)))
          opt_correl_single_country <-
            doCorrelations(
              toPredict = df_deaths_train,
              modelPar = df_umd_train,
              columns_to_try = columns_to_try,
              iso_code_country = iso_code_country
            )
          # Shift signals We will remove NAs from df_umd_test later
          shiftedSignals <- shiftSignals(baseForOutputDF=(df_deaths_train %>% dplyr::select(date, y)), inputDF=df_umd_train, correl=opt_correl_single_country)
          
          # keep only signals before cutoff after shifting, this is unnecessary but it does not hurt
          shiftedTrainSignal <- shiftedSignals %>% filter(date <= cutoff)
          
          leftoverFromShifted <- shiftedSignals %>% filter(date > cutoff)
          ## call GLM ----
          
          print(paste("doing GLM for cutoff ", as.Date(cutoff)))
          #print (opt_correl_single_country, n=Inf)
          
          m <-
            doGLM(
              modelPar = shiftedTrainSignal,
              iso_code_country = iso_code_country
            )
          print(paste("doing TESTING for cutoff ", as.Date(cutoff)))
          # call Test ----
          # but before shift the test set to the future We will remove NAs from df_umd_test later
          shiftedTest <- shiftSignals(baseForOutputDF=(df_umd_test %>% dplyr::select(date)),
                                      inputDF=df_umd_test, correl=opt_correl_single_country)
          
          
          
          #  print(paste("now iterating through ",unique(opt_correl_single_country$signal)))
          
          
          print(paste("prepared leftover test signals", getMinAndMaxDatesAsString(leftoverFromShifted)))
          metricDF<-data.frame()
          metricDF[1,"cutoff"]=as.Date(cutoff)
          metricDF[1,"predType"]="nearFuture"
          outDF=doTest(m = m, 
                       testSignals = leftoverFromShifted,
                       testResp = df_deaths_test,
                       cutoff = cutoff,
                       metricDF = metricDF)
          if (nrow(outDF)>0){
            outDF["cutoff"]=as.Date(cutoff)
            outDF["strawman"]=NA
            outDF["scaled_abs_err"]=NA
            for (row in 1:nrow(outDF)) {
              ourEstimate <- outDF[row, "fore"]
              curdate  <- outDF[row, "date"]
              strawmandev <- abs(strawmanPred - (df_deaths_test %>% filter(date == curdate))$y)+10^-6
              
              modeldev <- abs(ourEstimate - (df_deaths_test %>% filter(date == curdate))$y)
              scaled_abs_err <- modeldev/strawmandev
              outDF[row,"strawman"]=strawmanPred
              outDF[row,"scaled_abs_err"]=scaled_abs_err
              # message(paste(“sca 7 =“,scaled_abs_err))
            }
            outDF["predType"]="nearFuture"
            
            toWrite<-rbind(toWrite,outDF)
            metricsToWrite<-rbind(metricsToWrite, metricDF)
          }
          print(paste("prepared shifted test signals", getMinAndMaxDatesAsString(shiftedTest)))
          metricDF<-data.frame()
          metricDF[1,"cutoff"]=as.Date(cutoff)
          metricDF[1,"predType"]="nearFuture"
          outDF=doTest(m = m,
                       testSignals = shiftedTest,
                       testResp = df_deaths_test,
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
          combinedSignals = rbind(leftoverFromShifted, shiftedSignals)
          print(paste("prepared combined signals", getMinAndMaxDatesAsString(combinedSignals)))
          metricDF<-data.frame()
          metricDF[1,"cutoff"]=as.Date(cutoff)
          metricDF[1,"predType"]="nearFuture"
          outDF=doTest(m = m,
                       testSignals = combinedSignals,
                       testResp = df_deaths_test,
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
        write.csv(toWrite,file = paste0("../data/estimates-symptom-lags/cutoffs/PlotData/",iso_code_country,"-estimates-lag-daily.csv"))
        write.csv(metricsToWrite,file = paste0("../data/estimates-symptom-lags/cutoffs/PlotData/",iso_code_country,"-aggregatemetrics-lag.csv"))
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