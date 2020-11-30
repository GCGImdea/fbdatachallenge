library(tidyverse)
library(zoo)

path_symptom_lags <- "../data/estimates-symptom-lags/cutoffs/PlotData/"
path_baseline <- "../data/baseline_outputs/"
path_out <- "../data/estimates-symptom-lags/Plots-baseline-vs-GLM/"

# country_iso <- "PT"

# for (lag_in in 1:7) {
#   df_col_rm <- read.csv(file = paste0(path_symptom_lags,
#                                       country_iso,
#                                       "-cases-7-60-penFALSE-alpha0.5-rmccTRUE-rmth0.9-2020-11-10-estimates-lag-daily.csv")) %>% 
#     mutate(date = as.Date(date)) %>% 
#     filter(lag == lag_in & predType == "nearFuture") 
#   
#   
#   df_no_col_rm <- read.csv(file = paste0(path_symptom_lags,
#                                          country_iso,
#                                          "-cases-7-60-penFALSE-alpha0.5-rmccFALSE-rmth0.9-2020-11-10-estimates-lag-daily.csv")) %>% 
#     mutate(date = as.Date(date)) %>% 
#     filter(lag == lag_in & predType == "nearFuture") 
#   
#   df_baseline <- read.csv(file = paste0(path_baseline,
#                                         country_iso,
#                                         "-baseline.csv")) %>% 
#     mutate(date = as.Date(real_date)) %>% 
#     filter(date >= min(df_no_col_rm$date) & date <= max(df_no_col_rm$date))
#   
#   # 5-colors palettes:
#   # my.palette <- c("red", "blue", "black", "magenta", "brown")
#   my.palette <- c("#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#073b4c")
#   
#   colors <- c("Model" = my.palette[3], 
#               "Model Fb." = my.palette[4],
#               "Official" = my.palette[5],
#               "Remove Correlated" = my.palette[1],
#               "Retain Correlated" = my.palette[2])
#   
#   p1 <- ggplot() +
#     geom_line(data = df_baseline, aes(x = date, y = case_model, colour = "Model"), size = 1, alpha = 0.6) +
#     geom_line(data = df_baseline, aes(x = date, y = case_fb_model, colour = "Model Fb."), size = 1, alpha = 0.6) +
#     geom_line(data = df_col_rm, aes(x = date, y = fore, colour = "Remove Correlated"), size = 1, alpha = 0.6) +
#     geom_point(data = df_col_rm, aes(x = date, y = y, colour = "Official"), size = 2, alpha = 0.6) +
#     geom_line(data = df_no_col_rm, aes(x = date, y = fore, colour = "Retain Correlated"), size = 1, alpha = 0.6) +
#     theme_light(base_size = 15) + 
#     labs(x = "Date", y = "Cases", colour = "", 
#          title = paste0(country_iso, ": lag ", lag_in)) +
#     scale_color_manual(values = colors) +
#     theme(legend.position = "bottom")
#   # print(p1)  
#   ggsave(plot = p1,
#          filename =  paste0(path_out, country_iso, "_lag_", lag_in, "_baseline_vs_glm.png"),
#          width = 9, height = 7)
#   
#   
#   # Rolling mean:
#   k = 7 # rolling window
#   p2 <- ggplot() +
#     geom_line(data = df_baseline, aes(x = date, y = case_model, colour = "Model"), size = 1, alpha = 0.6) +
#     geom_line(data = df_baseline, aes(x = date, y = case_fb_model, colour = "Model Fb."), size = 1, alpha = 0.6) +
#     geom_line(data = df_col_rm, aes(x = date, 
#                                     y = rollmean(fore, k = k, fill = NA, align = "right"), 
#                                     colour = "Remove Correlated"), size = 1, alpha = 0.6) +
#     geom_point(data = df_col_rm, aes(x = date, 
#                                      y = rollmean(y, k = k, fill = NA, align = "right"), 
#                                      colour = "Official"), size = 2, alpha = 0.6) +
#     geom_line(data = df_no_col_rm, aes(x = date, 
#                                        y = rollmean(fore, k = k, fill = NA, align = "right"), 
#                                        colour = "Retain Correlated"), size = 1, alpha = 0.6) +
#     theme_light(base_size = 15) + 
#     labs(x = "Date", y = "Cases", colour = "", 
#          title = paste0(country_iso, ": lag ", lag_in, " (", k, " days rolling mean)" )) +
#     scale_color_manual(values = colors) +
#     theme(legend.position = "bottom")
#   # print(p2)
#   ggsave(plot = p2,
#          filename =  paste0(path_out, country_iso, "_lag_", lag_in, "_rollmean_", k, "_baseline_vs_glm.png"),
#          width = 9, height = 7)
#   
# }




country_iso <- "PT"
pen=TRUE
rmcc=TRUE
umd=TRUE
ccfr=FALSE
nsum=FALSE
minlag=7
for (lag_in in 1:7) {
  df_col_rm <- read.csv(file = paste0(path_symptom_lags,
                                      country_iso,
                                      "-cases-",minlag,"-60-pen",pen,"-alpha0.5-rmcc",rmcc,"-rmth0.9-2020-09-10-2020-11-10-1-",umd,"-",ccfr,"0-",nsum,"-estimates-lag-daily.csv")) %>%
    mutate(date = as.Date(date)) %>% 
    filter(lag == lag_in & predType == "nearFuture") 
  rmcc=FALSE
  df_no_col_rm <- read.csv(file = paste0(path_symptom_lags,
                                      country_iso,
                                      "-cases-",minlag,"-60-pen",pen,"-alpha0.5-rmcc",rmcc,"-rmth0.9-2020-09-10-2020-11-10-1-",umd,"-",ccfr,"0-",nsum,"-estimates-lag-daily.csv")) %>%
   mutate(date = as.Date(date)) %>% 
    filter(lag == lag_in & predType == "nearFuture") 
  
  df_baseline <- read.csv(file = paste0(path_baseline,
                                        country_iso,
                                        "-baseline.csv")) %>% 
    mutate(date = as.Date(real_date)) %>% 
    filter(date >= min(df_no_col_rm$date) & date <= max(df_no_col_rm$date))
  
  # 5-colors palettes:
  # my.palette <- c("red", "blue", "black", "magenta", "brown")
  my.palette <- c("#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#073b4c")
  
  colors <- c("Model" = my.palette[3], 
              "Model Fb." = my.palette[4],
              "Official" = my.palette[5],
              "Remove Correlated" = my.palette[1],
              "Retain Correlated" = my.palette[2])
  
  p1 <- ggplot() +
    geom_line(data = df_baseline, aes(x = date, y = case_model, colour = "Delphi baseline"), size = 1, alpha = 0.6) +
    geom_line(data = df_baseline, aes(x = date, y = case_fb_model, colour = "Delphi baseline w/ Fb. data"), size = 1, alpha = 0.6) +
    geom_line(data = df_col_rm, aes(x = date, y = fore, colour = "Remove Correlated"), size = 1, alpha = 0.6) +
    geom_point(data = df_col_rm, aes(x = date, y = y, colour = "Official"), size = 2, alpha = 0.6) +
    geom_line(data = df_no_col_rm, aes(x = date, y = fore, colour = "Retain Correlated"), size = 1, alpha = 0.6) +
    theme_light(base_size = 15) + 
    labs(x = "Date", y = "Cases", colour = "", 
         title = paste0(country_iso, ": lag ", lag_in)) +
    scale_color_manual(values = colors) +
    theme(legend.position = "bottom")
  # print(p1)  
  ggsave(plot = p1,
         filename =  paste0(path_out, country_iso, "_lag_", lag_in,"pen",pen,"-umd",umd,"-ccfr",ccfr,"-nsum",nsum,"-minlag",minlag, "_baseline_vs_glm.png"),
         width = 9, height = 7)
  
  pen=TRUE
  rmcc=TRUE
  umd=TRUE
  ccfr=FALSE
  nsum=FALSE
  minlag=7
  # Rolling mean:
  k = 7 # rolling window
  p2 <- ggplot() +
    geom_line(data = df_baseline, aes(x = date, y = case_model, colour = "Delphi baseline"), size = 1, alpha = 0.6) +
    geom_line(data = df_baseline, aes(x = date, y = case_fb_model, colour = "Delphi baseline w/ Fb. data"), size = 1, alpha = 0.6) +
    geom_line(data = df_col_rm, aes(x = date, 
                                    y = rollmean(fore, k = k, fill = NA, align = "right"), 
                                    colour = "Remove Correlated"), size = 1, alpha = 0.6) +
    geom_point(data = df_col_rm, aes(x = date, 
                                     y = rollmean(y, k = k, fill = NA, align = "right"), 
                                     colour = "Official"), size = 2, alpha = 0.6) +
    geom_line(data = df_no_col_rm, aes(x = date, 
                                       y = rollmean(fore, k = k, fill = NA, align = "right"), 
                                       colour = "Retain Correlated"), size = 1, alpha = 0.6) +
    theme_light(base_size = 15) + 
    labs(x = "Date", y = "Cases", colour = "", 
         title = paste0(country_iso, ": lag ", lag_in, " (", k, " days rolling mean)" )) +
    scale_color_manual(values = colors) +
    theme(legend.position = "bottom")
  # print(p2)
  ggsave(plot = p2,
         filename =  paste0(path_out, country_iso, "_lag_", lag_in,"pen",pen,"-umd",umd,"-ccfr",ccfr,"-nsum",nsum,"-minlag",minlag, "_rollmean_", k, "_baseline_vs_glm.png"),
         width = 9, height = 7)
  
  ## BOXPLOT ----
  df_box <- data.frame(SAE = df_baseline$case_sae, Model = "Delphi baseline") %>% 
    rbind(data.frame(SAE = df_baseline$case_fb_model, Model = "Delphi baseline w/ Fb. data")) %>% 
    rbind(data.frame(SAE = df_col_rm$scaled_abs_err, Model = "Remove Correlated")) %>% 
    rbind(data.frame(SAE = df_no_col_rm$scaled_abs_err, Model = "Retain Correlated"))
  
  p_box <-  ggplot(df_box, aes(y = SAE, x = Model)) +
    geom_boxplot() +
    theme_light(base_size = 15) +
    labs(x = "Model", y = "Scaled absolute error", 
         title = paste0(country_iso, ": lag ", lag_in))
  # print(p_box)
  ggsave(plot = p_box,
         filename =  paste0(path_out, country_iso, "_lag_", lag_in,"pen",pen,"-umd",umd,"-ccfr",ccfr,"-nsum",nsum,"-minlag",minlag, "_rollmean_", k, "_boxplot_baseline_vs_glm.png"),
         width = 9, height = 7)
  
}
