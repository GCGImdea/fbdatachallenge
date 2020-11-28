## Libraries
library(dplyr)
library(stringr)
library(zoo) # to use rollmean
library(foreign)
library(MASS)
library(ggplot2)
library(grid) # annotate a ggplot

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
        # joined <- (df_single_symp %>% select(date, column_in)) 
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



opt_correls <- data.frame()

signal_to_match <- "cases"

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

signals_to_try <- c(signals_umd,signals_ccfr) 
#signals_to_try <- c(signals_umd,signals_ccfr,signals_nsum) 
#signals_to_try <- signals_nsum
#signals_to_try <- signals_ccfr
#signals_to_try <- signals_umd_past_smooth
#signals_to_try <- signals_umd

lag <- 7 # decide here the min lag and only project up to cutoff+lag

# Files to consider,
file_in_path <- "../data/all_giant_df/"
file_in_pattern <- ".*alldf.csv"
files <- dir(file_in_path, pattern = file_in_pattern)

# ISO codes to process
iso_codes <- c()
for (file in files) {
  iso_codes <- c(iso_codes,substr(file, 1, 2))
}

# Debug: Optionaly reduce to some countries to run faster
#iso_codes <- c("GB")
iso_codes <- c("BR", "DE", "EC", "PT", "UA", "ES", "IT", "CL", "FR", "GB")

# Load and process given countries
for (code in iso_codes)
{
  cat(" Doing ", code, ": ")
  all_df <- read.csv(paste0("../data/all_giant_df/", code, "_alldf.csv"))
  all_df <- all_df %>% mutate(date = as.Date(date))
  
  ## Prepare sources and target 
  y <- signal_to_match
  y_df <- all_df %>% dplyr::select(date,y)
    ry <- max(y_df$date) # last day in target
  ly <- min(y_df$date) # first day in target
  
  x_df <- all_df %>% dplyr::select(date)
  for (signal in signals_to_try)
  {
    s_df <- all_df %>% dplyr::select(date,signal)
    x_df <- x_df %>% full_join(s_df, by = "date")
  }
  x_df <- x_df[complete.cases(x_df), ]
  rx <- max(x_df$date) # last day in sources
  lx <- min(x_df$date) # first dayin sources
  
  cutoff <- rx
  
  # Separate and rename target after sources stop date, to be ploted in black
  y_post_df <- y_df %>%
    filter(date > cutoff)
  colnames(y_post_df) <- c("date","y_post")
  
  # Prepare target for training, up to last signal day
  y_pre_df <- y_df %>%
    filter(date <= cutoff)
  colnames(y_pre_df) <- c("date","y_pre")
  
  # Harold and David, bellow is the adaption of the code I had to plot a single graph
  # We should look at this bellow and plug your new stuff
  
  ## Correlations ----
  # compute all correlations for all lags in min_lag to max_lag:
  correls_single_country <- check_lags(
    df_response = y_pre_df,
    df_add_regressors = x_df,
    columns_to_try = signals_to_try,
    min_lag = lag,
    max_lag = 40)
  
  # extract lag with significant-maximum correlation by signal:
  opt_correl_single_country <- correls_single_country %>% 
    filter(pval <= 0.05) %>% 
    group_by(signal) %>% 
    filter(abs(correlations) == max(abs(correlations))) %>% 
    ungroup()
  
  # save optimal correlations:
  opt_correl_single_country$country <- code
  opt_correls <- rbind(opt_correls, opt_correl_single_country)
  
  # plot of correlations for single country:
  ifelse(!dir.exists(file.path("../data/estimates-umd-unbatched/", "Plots")), 
         dir.create(file.path("../data/estimates-umd-unbatched/", "Plots")), FALSE)
  p <-
    ggplot(data = correls_single_country, aes(x = shift, y = correlations, color = signal)) +
    geom_line(alpha = 0.8) +
    ylim(-1, 1) +
    labs(title = paste0(code," ",signal_to_match)) +
    theme_light()
  ggsave(plot = p, 
         filename = paste0(
           "../data/estimates-symptom-lags/Plots-Correlations/",
           code,"-",signal_to_match,
           "-predictor-correlation.png"
         ), width = 10, height = 7
  )
  
  ## GLM ----
  
  # contruct a single data frame with response and regressors:
  x_shift_df <- data.frame(seq(min(lx,ly),max(rx,ry),by="day"))
  colnames(x_shift_df) <- c("date")
  
  for (indep_var in unique(opt_correl_single_country$signal)) {
    
    df_indep_var_temp <- x_df %>% 
      dplyr::select(date, all_of(indep_var)) %>% 
      mutate(date = date + 
               opt_correl_single_country$shift[opt_correl_single_country$signal == indep_var]
      )
    x_shift_df <- x_shift_df %>% full_join(df_indep_var_temp, by = "date")
    
  }

  # Copy predictors for later predict
  x_predict_df <- x_shift_df
  # optionally impose a limit
  x_predict_df <- x_predict_df %>% filter(date <= cutoff + lag)
  
  # join target for training
  xy_train_df <- x_shift_df
  xy_train_df <- xy_train_df %>% 
    full_join(y_pre_df, by = "date") %>%
    filter(date <= cutoff )
  
  # TODO: Now would be a good to remove signals that dont have good data ranges
  
  # get only complete cases (remove rows with NAs)
  xy_train_df <- xy_train_df[complete.cases(xy_train_df), ]
  #min(df_glm$date); max(df_glm$date)
  
  # fit model (DO NOT USE DATE!!!):
  m1 <- glm.nb(y_pre ~ . -date , data = xy_train_df)
  summary(m1)
  
  # Stepwise regression model
  m1 <- stepAIC(m1, direction = "both", 
                trace = FALSE)
  summary(m1)
  m1$anova
  
  # now for prediction keep only kept regressors and date
  x_predict_df <- x_predict_df[ , (names(x_predict_df) %in% c(labels(m1$terms),"date"))]
  # get only complete cases (remove rows with NAs)
  x_predict_df <- x_predict_df[complete.cases(x_predict_df), ]
  
  print(labels(m1$terms))
  
  ## Plot + CI
  ## grab the inverse link function
  ilink <- family(m1)$linkinv
  ## add fit and se.fit on the **link** scale
  x_predict_df <- bind_cols(x_predict_df, setNames(as_tibble(predict(m1,x_predict_df, se.fit = TRUE)[1:2]),
                                         c('fit_link','se_link'))) %>% 
    mutate(fit_resp  = ilink(fit_link),
           right_upr = ilink(fit_link + (2 * se_link)),
           right_lwr = ilink(fit_link - (2 * se_link)))
  
  # Add target for plotting
  x_plot_df <- x_predict_df
  x_plot_df <- x_plot_df %>% full_join(y_pre_df, by="date")
  x_plot_df <- x_plot_df %>% full_join(y_post_df, by="date")
  # Add a smoothed response
  x_plot_df$fit_resp_smooth <- rollmean(x_plot_df$fit_resp, 1, fill = NA)
 

  my_colors <- c("Fitting" = "red", 
                 "Estimated" = "blue", "Post" = "black")
  

  p_model <- ggplot(data = x_plot_df, aes(x = date) ) +
    geom_line(aes(y = fit_resp_smooth, colour = "Estimated"), size = 1, alpha = 0.8) +
    geom_point(aes(y = fit_resp_smooth, colour = "Estimated"), size = 1.5, alpha = 0.6) +
    geom_ribbon(aes(ymin = right_lwr, ymax = right_upr),
                alpha = 0.1, fill = my_colors["Estimated"]) +
    geom_point(aes(y = y_pre, colour = "Fitting"), size = 1.5, alpha = 0.6) +
    geom_line(aes(y = y_pre, colour = "Fitting"), size = 0.2, alpha = 0.6) +
    geom_point(aes(y = y_post, colour = "Post"), size = 1.5, alpha = 0.6) +
    geom_line(aes(y = y_post, colour = "Post"), size = 0.2, alpha = 0.6) +     
    scale_color_manual(values = my_colors) +
    labs(x = "Date", y =  paste("Number of ",signal_to_match), title = code,  colour = "") +
    theme_light(base_size = 15) +
    theme(legend.position="bottom") # + annotation_custom(grob)
  # p_model
  ggsave(plot = p_model, 
         filename = paste0(
           "../data/estimates-symptom-lags/Plots-GLM/",
           code,"-",signal_to_match,
           "-vs-predicted.png"
         ), width = 10, height = 7
  )
  
  
  sae <- c()
  for (i in seq(1,lag))
  {
    predi <- x_plot_df[x_plot_df$date==(cutoff+i),]$fit_resp_smooth
    straw0 <- x_plot_df[x_plot_df$date==(cutoff),]$y_pre
    valuei <- x_plot_df[x_plot_df$date==(cutoff+i),]$y_post
    sca_abs_err <- abs(valuei-predi)/(abs(valuei-straw0)+10^-6)
    sae <- c(sae,sca_abs_err)
    print(paste0("day ",i," scaled absolute error=",sca_abs_err))
  }
  print(mean(sae))
    
  
  
}




