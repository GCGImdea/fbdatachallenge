## Libraries
library(prophet)
library(dplyr)
library(tidyr)
library(stringr)
library(forecast)
library(ggplot2)
library(gridExtra)
library(zoo)
library(plotly)


propheting_data <- function(df_response, 
                            name_date_var = "ds",
                            name_target_var = "y",
                            grwth = 'logistic', 
                            chg_point_prior = 0.5, 
                            to_plot = F){
  
  tryCatch( {
    df_forecasted_series_out <- data.frame()
    df_cv_performance_out <- data.frame()
    
    df_in_prophet <- df_response
    
    country <- df_in_prophet$countrycode[1]
    
    colnames(df_in_prophet) <- str_replace(colnames(df_in_prophet), 
                                           pattern = name_date_var, 
                                           replacement = "ds")
    colnames(df_in_prophet) <- str_replace(colnames(df_in_prophet), 
                                           pattern = name_target_var, 
                                           replacement = "y")
    
    df_in_prophet <- df_in_prophet %>% mutate(y = pmax(y, 0))
    
    # construct future (whole date period + 2 weeks):
    future <- data.frame(ds = df_in_prophet$ds + 14) %>% 
      left_join(df_in_prophet, by = "ds") 
    
    # now past (whole date period):
    df_past <- df_in_prophet 
    
    if (grwth == 'logistic') {
      future$cap <- max(future$y, na.rm = T) * 1.5
      future$floor <- 0
      
      df_past$cap <- max(df_past$y, na.rm = T) * 1.5
      df_past$floor <- 0
    }
    
    ## Forecasting response with no extra regressors ----
    
    m <- prophet(changepoint.range = 0.99,
                 changepoint.prior.scale = chg_point_prior,
                 growth = grwth,
                 yearly.seasonality = F,
                 weekly.seasonality = F,
                 daily.seasonality = F)
    
    m <- fit.prophet(m, df_past)
    
    forecast <- predict(m, future) %>% 
      mutate(yhat_lower = pmax(yhat_lower, 0)) # to avoid negative values in CI
    
    
    if (to_plot) {
      # p_compo <- prophet_plot_components(m, forecast) 
      # ggsave(plot = p_compo,
      #        filename =  paste0(out_path_forecast_response, "Plots-Pred-vs-Actual-Components/", 
      #                           country, "-prophet-forecast-components.png"),
      #        width = 7, height = 5)
      
      p_forecast <- plot(m, forecast) + add_changepoints_to_plot(m) + theme_light() +
        labs(title = paste0(country, "-series forecast"), x = "Date", y = "Number of deaths")
      ggsave(plot = p_forecast,
             filename =  paste0(out_path_forecast_response, "Plots-Pred-vs-Actual/", 
                                country, "-prophet-forecast.png"),
             width = 7, height = 5)
    }
    
    ## save forecasting:
    df_forecasted_series_out <- forecast %>% 
      select(ds, yhat, yhat_lower, yhat_upper, trend, trend_lower, trend_upper) %>% 
      left_join(df_in_prophet, by = "ds") %>% 
      mutate(date = ds) %>% 
      select(!ds)
    
    write.csv(df_forecasted_series_out, 
              file = paste0(out_path_forecast_response, "PlotData/", 
                            country, "-prophet-forecast.csv"))
    
    ## CV forecasting ----
    df.cv <- cross_validation(m, initial = round(nrow(forecast)*0.7), period = 2, horizon = 7, units = 'days')
    df.p <- performance_metrics(df.cv)
    df_cv_performance_out <- df.p
    
    write.csv(df_cv_performance_out, 
              file = paste0(out_path_forecast_response, "CV-Performance/", 
                                                   country, "-cv-performance.csv"))
    
    df.p <- pivot_longer(data = df.p, 
                         cols = colnames(df.p)[-1],
                         names_to = "performance", values_to = "value")
    
    
    if (to_plot) {
      p_performance <- ggplot(data = df.p, 
                              aes(x = horizon, y = value, group = performance)) +
        facet_wrap(~performance, scales = "free_y") + 
        geom_line( size = 1, alpha = 0.4) +
        geom_point( size = 2, alpha = 0.8) +
        theme_light() +
        labs(title = paste0(country, ": cv-performance")) +
        ylab("Performance") + xlab("Horizon") +
        theme(legend.position = "bottom") 
      ggsave(plot = p_performance,
             filename =  paste0(out_path_forecast_response, "Plots-CV-Performance/", 
                                country, "-cv-performance.png"),
             width = 7, height = 5)
    }
    
    
    return(list(df_cv_performance = df_cv_performance_out, 
                df_forecasted_series = df_forecasted_series_out))
  }, error=function(cond) {
    
    message("Error!")
    return(NA)
  }
    
  )
  
}

## DO for different countries ----

file_in_deaths <- "../data/estimates-confirmed/PlotData/"
file_in_deaths_pattern <- ".*-estimate.csv"

out_path_forecast_response <- "../data/estimates-prophet-deaths/"

files <- dir(file_in_deaths, pattern = file_in_deaths_pattern)

list_df_cv_performance <- list()

for (file in files) {
  iso_code_country <- substr(file, 1, 2)
  cat("doing ", iso_code_country, ": ")
  
  
  ## Load number of deaths ----
  
  df_deaths <-
    read.csv(paste0(file_in_deaths, file)) %>%
    select(!X) %>% 
    mutate(date = as.Date(date)) %>%
    mutate(y = deaths) %>%
    mutate(y = rollmean(y, 1, fill = NA)) %>%
    mutate(deaths = round(y)) %>% # CBM change
    filter(!is.na(y))
  
  df_out_prophet <- propheting_data(df_response = df_deaths, 
                              name_date_var = "date",
                              name_target_var = "y",
                              grwth = 'logistic', 
                              chg_point_prior = 0.5, 
                              to_plot = T)
  
  if (!is.na(df_out_prophet)) {
    list_df_cv_performance[[file]] <- df_out_prophet[["df_cv_performance"]]
  }
  
  # cat("----> ", iso_code_country, ": ")
  # message("success ")
}

save(list_df_cv_performance, 
     file = paste0(out_path_forecast_response, 
                   "all-countries-cv-performance.Rdata"))
