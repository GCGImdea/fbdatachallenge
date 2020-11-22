## Libraries
library(dplyr)
library(stringr)
library(zoo) # to use rollmean
library(foreign)
library(MASS)
library(ggplot2)

iso_code_country <- "PT"

## Load UMD regressors ----

#data_df <-  read.csv(paste0("../data/estimates-umd-batches/", iso_code_country , "/", iso_code_country ,"_UMD_country_data.csv"))
data_df <-  read.csv(paste0("../data/estimates-umd-unbatched/PlotData/", iso_code_country ,"_UMD_country_nobatch_past_smooth.csv"))


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

## Load CCFR regressors

#df_ccfr <- read.csv(paste0("../data/estimates-ccfr-based/PlotData/", iso_code_country,"-estimate.csv")) %>% 
#  mutate(date = as.Date(date) )
df_ccfr <- read.csv(paste0("../../coronasurveys/coronasurveys/data/estimates-ccfr-based/PlotData/", iso_code_country,"-estimate.csv")) %>% 
  mutate(date = as.Date(date) )
  
## Load NSUM regressors TODO

#df_nsum <- read.csv(paste0("../data/estimates-ccfr-based/PlotData/", iso_code_country,"-estimate.csv")) %>% 
#  mutate(date = as.Date(date) )  

## Load number of deaths ----  

#df_deaths <- read.csv(paste0("../data/estimates-ccfr-based/PlotData/", iso_code_country,"-estimate.csv")) %>% 
df_deaths <- read.csv(paste0("../../coronasurveys/coronasurveys/data/estimates-ccfr-based/PlotData/", iso_code_country,"-estimate.csv")) %>% 
  mutate(date = as.Date(date) ) %>% 
  mutate(y = deaths) %>% 
  mutate(y = rollmean(y, 1, fill = NA)) %>%
  filter(!is.na(y)) #%>% 
#  select(date, y) %>%
#  filter(date <= "2020-11-18")
# 
# temp_deaths <- df_deaths %>%
#   select(date, deaths_prev_week) %>%
#   mutate(date = date - 6, deaths_prev_week_6 = deaths_prev_week/7) %>%
#   select(date, deaths_prev_week_6)
# 
# df_deaths <- left_join(df_deaths, temp_deaths) %>% 
#   mutate(y = deaths_prev_week_6) %>% 
#   select(date, y)
# p1 <- ggplot(temp_deaths, aes(x = date )) +
#   geom_line(aes(y = y), color ="red") +
#   geom_line(aes(y = deaths_prev_week/7), color = "blue") +
# geom_line(aes(y = deaths_prev_week7/7), color = "green")
# ggplotly(p1)

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
    corTest <- cor.test(df_response$y, df_single_symp[, column_in], method = "spearman" )
    
    df_correl <- data.frame(shift = date_shift, correlations = corTest$estimate, pval = corTest$p.value,

                        win_size = win_size,
                         signal = column_in)
    
    df_out <- rbind(df_out, df_correl)
    
  }
  
  
  return(df_out)
  
}


all_correls <- data.frame()
for (try_shift in seq(0, 2*30)) {
  all_correls <- rbind(all_correls, 
                       check_lags(df_deaths, df_umd, 
                                  columns_to_try = c("pct_anosmia_ageusia", 
                                                     "pct_sore_throat", 
                                                     "pct_fever","pct_cmnty_sick"), 
#                        check_lags(df_deaths, df_ccfr,
#                                   columns_to_try = c("cases","cases_daily","cases_active","cases_contagious","deaths"),

                                  date_shift = try_shift))
  
}


head(all_correls)

p <- ggplot(data = all_correls, aes(x = shift, y = correlations, color = signal)) +
  geom_line(alpha = 0.8) + 
  ylim(-1,1) +
#  ylim(0,0.05) +
  labs(title = iso_code_country) +
  theme_light()
fig <- ggplotly(p)
fig

# all_correls_no_moving <- all_correls
# all_correls_no_moving$signal <- paste0(all_correls_no_moving$signal, "_daily_deaths")
# 
# all_correls_no_moving <- rbind(all_correls_no_moving, all_correls)
# 
# p <- ggplot(data = all_correls_no_moving, aes(x = shift, y = correlations, color = signal)) +
#   geom_line(alpha = 0.8) + 
#   labs(title = iso_code_country) +
#   theme_light()
# fig <- ggplotly(p)
# fig


#### NEW


lag <- 30 # Choose a good lag by looking at max correlations of predictors

df_umd$date <- as.Date(df_umd$date) + lag
rend <- min(max(df_umd$date),max(df_deaths$date))
lend <- max(min(df_umd$date),min(df_deaths$date))
df_umd_ss <- df_umd %>% 
  filter(date <= rend) %>%
  filter(date >= lend)
df_deaths_ss <- df_deaths %>% 
  filter(date <= rend) %>%
  filter(date >= lend)

length(df_deaths_ss$y)
dados<-data.frame(Y=df_deaths_ss$y, 
                  P1=df_umd_ss$pct_cmnty_sick, 
#                  P2=df_umd_ss$pct_anosmia_ageusia,
                  P3=df_umd_ss$pct_fever,
                  P4=df_umd_ss$pct_sore_throat)

length(df_deaths_ss$y); lend; rend;

summary(m1 <- glm.nb(Y ~ P1 + P3 + P4, data = dados))

names(m1)

future <- df_umd #%>% filter(date > rend+1)

pred <- data.frame(P1 = future$pct_cmnty_sick, P3 = future$pct_fever, P4 = future$pct_sore_throat)
pred$Y <- predict(m1, pred, type = "response")
pred$date <- future$date

sm <- 3 # optional smooth on prediction output

plot(pred$date,runmed(pred$Y,sm),type="b",pch=19, col="blue",cex=.8); points(df_deaths$date,df_deaths$deaths,pch=19, col="red")


