## Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

country <- "ES"
estimates_path <- "../data/estimates-confirmed/"

## Load data from datatista's github ----
df <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_datos_sanidad_nueva_serie.csv",
                     encoding = "UTF-8")
names(df) <- tolower(names(df))
df <- df %>% rename(date = fecha,
                    reg_name = ccaa,
                    cases = casos_diagnosticados,
                    deaths = fallecidos,
                    hospital = hospitalizados,
                    icu = uci)

ine_dict <- data.frame(cod_ine = 1:19, 
                       reg_code = c("ESAN", "ESAR", "ESAS", "ESIB", "ESCN", "ESCB", "ESCL",
                                    "ESCM", "ESCT", "ESVC", "ESEX", "ESGA", "ESMD",
                                    "ESMC", "ESNC", "ESPV", "ESRI", "ESCE", "ESML"))
regsdata <- read.csv("../data/common_data/regions-tree-population.csv", as.is = T) %>% 
  filter(countrycode == "ES") %>% 
  group_by(regioncode) %>% 
  summarise(population = sum(population))

df <- df %>% left_join(ine_dict, by = "cod_ine") %>% left_join(regsdata, by = c("reg_code" = "regioncode"))

df <- df %>% select(date, reg_code, reg_name, population, cases, deaths, hospital, icu)

df$date <- as.Date(df$date)
min_date <- min(df$date)
# dates <- seq(as.Date(min(df$date)), Sys.Date(), by = "day")

for (region in unique(df$reg_code)) {
  df_reg <- df %>% filter(reg_code == region)
  
  # Complete all dates
  df_reg <- df_reg %>%
    mutate(date = as.Date(date)) %>%
    complete(date = seq.Date(min_date, Sys.Date(), by="day")) %>%
    fill(reg_code, reg_name, population)
  df_reg[["cases"]][is.na(df_reg[["cases"]])] <- 0
  df_reg[["deaths"]][is.na(df_reg[["deaths"]])] <- 0
  df_reg[["hospital"]][is.na(df_reg[["hospital"]])] <- 0
  df_reg[["icu"]][is.na(df_reg[["icu"]])] <- 0
  
  #Cumulative previous week
  if (nrow(df_reg) >= 7){
    df_reg$cases_prev_week <- cumsum(c(df_reg$cases[1:7], diff(df_reg$cases, lag = 7)))
    df_reg$deaths_prev_week <- cumsum(c(df_reg$deaths[1:7], diff(df_reg$deaths, lag = 7)))
    df_reg$hospital_prev_week <- cumsum(c(df_reg$hospital[1:7], diff(df_reg$hospital, lag = 7)))
    df_reg$icu_prev_week <- cumsum(c(df_reg$icu[1:7], diff(df_reg$icu, lag = 7)))
  }
  else {
    df_reg$cases_prev_week <- df_reg$deaths_prev_week <- df_reg$hospital_prev_week <- df_reg$icu_prev_week <- NA
  }
  
  dir.create(estimates_path, showWarnings = F)
  dir.create(paste0(estimates_path, country), showWarnings = F)
  cat("::- script-ccfr-based: Writing data for", region, "::\n")
  write.csv(df_reg, paste0(estimates_path, country, "/", region, "-estimate.csv"),
            row.names = FALSE)
}
