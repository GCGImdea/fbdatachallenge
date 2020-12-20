library(tidyverse)
library(zoo)
library(dplyr)

path_symptom_lags <- "../data/estimates-symptom-lags/cutoffs/PlotData/"
path_baseline <- "../data/baseline_outputs/"
path_out <- "../data/estimates-symptom-lags/Plots-baseline-vs-GLM/"

#try_countries <- c("BR", "GB", "DE", "EC", "PT", "UA", "ES", "CL", "FR")
try_countries <- c("PT")
# set upper limit to boxplot (p_box2):
y_lim_box = 10

for (country_iso in try_countries) {
  
  # ---
  # country_iso <- "DE"
  for (pen in c(FALSE, TRUE)){
    
    
    
    for (umd in c(TRUE)){
      for (ccfr in c(FALSE)){
        for (nsum in c(0)){
          for (minlag in c(7)){
            
            for (lag_in in 7:7) {
              
              rmcc=TRUE
              skipColRm=F
              skipNoColRm=F
              
              filename<-paste0(path_symptom_lags,
                               country_iso,
                               "-cases-",minlag,"-60-pen",pen,"-alpha0.5-rmcc",rmcc,"-rmth0.9-smthTRUE15-2020-09-11-2020-11-11-1-",umd,"-",ccfr,as.numeric(nsum),"-FALSE-estimates-lag-daily.csv")
              if (file.exists(filename)){
                df_col_rm <- read.csv(file = filename) %>%
                  mutate(date = as.Date(date)) %>% 
                  filter(lag == lag_in & predType == "nearFuture") 
              } else {
                skipColRm=T
              }
              
              
              
              
              rmcc=FALSE
              
              filename<-paste0(path_symptom_lags,
                               country_iso,
                               "-cases-",minlag,"-60-pen",pen,"-alpha0.5-rmcc",rmcc,"-rmth0.9-2020-09-10-2020-11-10-1-",umd,"-",ccfr,as.numeric(nsum),"-FALSE-estimates-lag-daily.csv")
              
              if (file.exists(filename)){
                df_no_col_rm <- read.csv(file = paste0(path_symptom_lags,
                                                       country_iso,
                                                       "-cases-",minlag,"-60-pen",pen,"-alpha0.5-rmcc",rmcc,"-rmth0.9-2020-09-10-2020-11-10-1-",umd,"-",ccfr,as.numeric(nsum),"-FALSE-estimates-lag-daily.csv")) %>%
                  mutate(date = as.Date(date)) %>% 
                  filter(lag == lag_in & predType == "nearFuture") 
                if (skipColRm){
                  df_col_rm <-df_no_col_rm
                }
              } else {
                if (skipColRm){
                  next
                } else {
                  df_no_col_rm=df_col_rm
                }
              } 
              df_baseline <- read.csv(file = paste0(path_baseline,
                                                    country_iso,
                                                    "-baseline.csv")) %>% 
                mutate(date = as.Date(real_date)) %>% 
                filter(date >= min(df_no_col_rm$date) & date <= max(df_no_col_rm$date))
              
              ######## debugging Carlos's sync method
              # #CBM variation
              # df_no_col_rm$fore_sync <- df_no_col_rm$fore
              # for (p in seq(1,dim(df_no_col_rm)[1]-7))
              # {
              #   delta <- df_no_col_rm$fore[p+7]-df_no_col_rm$fore[p]
              #   df_no_col_rm$fore_sync[p+7] <- df_no_col_rm$y[p]+delta
              # }
              # 
              # 
              #######3
              
              # 5-colors palettes:
              # my.palette <- c("red", "blue", "black", "magenta", "brown")
              my.palette <- c("#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#073b4c", "#ffff00")
              
              colors <- c("Delphi baseline" = my.palette[3], 
                          "Delphi baseline w/ Fb. data" = my.palette[4],
                          "Official" = my.palette[5],
                          "Remove Correlated" = my.palette[1],
                          "Retain Correlated" = my.palette[6],
                          "RC Sync" = my.palette[2])
              
              p1 <- ggplot() +
                geom_line(data = df_baseline, aes(x = date, y = case_model, colour = "Delphi baseline"), size = 1, alpha = 0.6) +
                geom_line(data = df_baseline, aes(x = date, y = case_fb_model, colour = "Delphi baseline w/ Fb. data"), size = 1, alpha = 0.6) +
                geom_line(data = df_col_rm, aes(x = date, y = fore, colour = "Remove Correlated"), size = 1, alpha = 0.6) +
                geom_line(data = df_col_rm, aes(x = date, y = syncFore, colour = "RC Sync"), size = 1, alpha = 0.6) +
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
              
              
              # Rolling mean:
              k = 7 # rolling window
              p2 <- ggplot() +
                geom_line(data = df_baseline, aes(x = date, y = case_model, colour = "Delphi baseline"), size = 1, alpha = 0.6) +
                geom_line(data = df_baseline, aes(x = date, y = case_fb_model, colour = "Delphi baseline w/ Fb. data"), size = 1, alpha = 0.6) +
                geom_line(data = df_col_rm, aes(x = date, 
                                                y = rollmean(fore, k = k, fill = NA, align = "right"), 
                                                colour = "Remove Correlated"), size = 1, alpha = 0.6) +
                geom_line(data = df_col_rm, aes(x = date, y = rollmean(syncFore, k = k, fill = NA, align = "right"), 
                                                colour = "RC Sync"), size = 1, alpha = 0.6) +
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
              
              df_col_rm <- df_col_rm %>% 

                dplyr::select(date, y, fore,syncFore) %>% 

                inner_join(df_baseline, by = "date") %>% 
                mutate(scaled_abs_err = abs(fore-y)/abs(strawman - y))%>%
                mutate(scaled_abs_err_sync = abs(syncFore-y)/abs(strawman - y))
              
              df_no_col_rm <- df_no_col_rm %>% 
                dplyr::select(date, y, fore) %>% 
                inner_join(df_baseline, by = "date") %>% 
                mutate(scaled_abs_err = abs(fore-y)/abs(strawman - y))
              
              
              df_box <- data.frame(SAE = df_baseline$case_sae, Model = "Delphi baseline") %>% 
                rbind(data.frame(SAE = df_baseline$case_fb_model, Model = "Delphi baseline w/ Fb. data")) %>% 
                rbind(data.frame(SAE = df_col_rm$scaled_abs_err, Model = "Remove Correlated")) %>% 
                rbind(data.frame(SAE = df_col_rm$scaled_abs_err_sync, Model = "RC Sync")) %>% 
                rbind(data.frame(SAE = df_no_col_rm$scaled_abs_err, Model = "Retain Correlated"))
              
              p_box1 <-  ggplot(df_box, aes(y = SAE, x = Model)) +
                geom_boxplot() +
                theme_light(base_size = 15) +
                labs(x = "Model", y = "Scaled absolute error", 
                     title = paste0(country_iso, ": lag ", lag_in))
              # print(p_box1)
              ggsave(plot = p_box1,
                     filename =  paste0(path_out, country_iso, "_lag_", lag_in,"pen",pen,"-umd",umd,"-ccfr",ccfr,"-nsum",nsum,"-minlag",minlag, "_boxplot_baseline_vs_glm.png"),
                     width = 9, height = 7)
              
              df_box <- data.frame(SAE = df_baseline$case_sae, Model = "Delphi baseline") %>% 
                # rbind(data.frame(SAE = df_baseline$case_fb_model, Model = "Delphi baseline w/ Fb. data")) %>% 
                rbind(data.frame(SAE = df_col_rm$scaled_abs_err, Model = "Remove Correlated")) %>% 
                rbind(data.frame(SAE = df_col_rm$scaled_abs_err_sync, Model = "Remove Correlated Sync")) %>% 
                rbind(data.frame(SAE = df_no_col_rm$scaled_abs_err, Model = "Retain Correlated"))
              
              p_box2 <-  ggplot(df_box, aes(y = SAE, x = Model)) +
                geom_boxplot() +
                theme_light(base_size = 15) +
                ylim(NA, y_lim_box) +
                labs(x = "Model", y = "Scaled absolute error", 
                     title = paste0(country_iso, ": lag ", lag_in))
              # print(p_box2)
              ggsave(plot = p_box2,
                     filename =  paste0(path_out, country_iso, "_lag_", lag_in,"pen",pen,"-umd",umd,"-ccfr",ccfr,"-nsum",nsum,"-minlag",minlag, "_boxplot2_baseline_vs_glm.png"),
                     width = 9, height = 7)
              
            }
          }
        }
      }
    }
  }
  # ---
  
} # country



