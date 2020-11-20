## Libraries
library(dplyr)
library(stringr)
library(zoo) # to use rollmean
library(ggplot2)

## Load UMD data ----

iso_code_country <- "ES"
# iso_code_country <- "BR"

data_df <-  read.csv(paste0("../data/estimates-umd-batches/", iso_code_country , "/", iso_code_country ,"_UMD_country_data.csv"))

data_df$date <- as.Date(data_df$date)

## remove "..._smooth", "..._high/low"
df_umd <- data_df[, str_detect(colnames(data_df), "pct_")]
df_umd <- df_umd[, !str_detect(colnames(df_umd), "smooth")]  
df_umd <- df_umd[, !str_detect(colnames(df_umd), "high")]
df_umd <- df_umd[, !str_detect(colnames(df_umd), "low")]
df_umd <- df_umd[, !str_detect(colnames(df_umd), "batched")]
df_umd <- df_umd*data_df$population[1]/100
df_umd$date <- data_df$date

colnames(df_umd)

## Load datadista's number of deaths ----  

df_deaths <- read.csv(paste0("../data/estimates-ccfr-based/PlotData/", iso_code_country,"-estimate.csv")) %>% 
  mutate(date = as.Date(date) ) %>% 
  # mutate(y = rollmean(total_deads, 3, fill = NA)) %>% 
  mutate(y = deaths) %>% 
  filter(!is.na(y)) %>% 
  select(date, y) %>% 
  filter(date <= "2020-11-04")


check_lags <- function(df_response, df_add_regressors, columns_to_try, date_shift = 0){
  
  df_response$date <- as.Date(df_response$date)
  
  df_out <- data.frame()
  
  for (column_in in columns_to_try) {
    
    df_single_symp <- df_add_regressors[ ,c("date", column_in)]
    
    # time shifting in extra regressors:
    df_single_symp$date <- as.Date(df_single_symp$date) + date_shift
    
    # set the same dates in df_response and df_single_symp:
    start_date <- max( min(df_single_symp$date), min(df_response$date) )
    end_date <- min( max(df_single_symp$date), max(df_response$date) )
    
    # win_size <- as.integer(end_date - start_date )
    
    # print(paste0( "starting: ", start_date, " -> end: ", end_date))
    
    df_single_symp <- df_single_symp %>% filter(date >= start_date, date <= end_date)
    df_response <- df_response %>% filter(date >= start_date, date <= end_date)
    
    # df_single_symp <- df_single_symp[1:165, ]
    # df_response <- df_response[1:165, ]
    
    win_size <- as.integer(max(df_response$date) - min(df_response$date) )
    
    correl <- cor(df_response$y, df_single_symp[, column_in], method = "spearman" )
    
    df_correl <- data.frame(shift = date_shift, correlations = correl, 
                         win_size = win_size,
                         signal = column_in)
    
    df_out <- rbind(df_out, df_correl)
    
  }
  
  
  return(df_out)
  
}


all_correls <- data.frame()
for (try_shift in seq(0, 30)) {
  all_correls <- rbind(all_correls, 
                       check_lags(df_deaths, df_umd, 
                                  columns_to_try = c("pct_anosmia_ageusia", 
                                                     "pct_sore_throat", 
                                                     "pct_fever"), 
                                  date_shift = try_shift))
  
}


head(all_correls)

p <- ggplot(data = all_correls, aes(x = shift, y = correlations, color = signal)) +
  geom_line(alpha = 0.8) + 
  labs(title = iso_code_country) +
  theme_light()
fig <- ggplotly(p)
fig

