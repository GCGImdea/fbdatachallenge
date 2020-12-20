library(tidyverse)
library(zoo)
library(dplyr)

path_symptom_lags <- "../data/estimates-symptom-lags/cutoffs/PlotData/"
path_baseline <- "../data/baseline_outputs/"
path_out <- "../data/estimates-symptom-lags/Plots-baseline-vs-GLM/"

#try_countries <- c("BR", "GB", "DE", "EC", "PT", "UA", "ES", "CL", "FR")
try_countries <- c("FR")
# set upper limit to boxplot (p_box2):
y_lim_box = 10


computeHighNotch <-function(column){
  return (median(column, na.rm=TRUE) + 1.58*IQR(column,na.rm=TRUE)/sqrt(sum(!is.na(column))))
  #  column[is.na(column)]=0
  #  return (median(column, na.rm=TRUE) + 1.58*IQR(column,na.rm=TRUE)/sqrt(length(column)))
}
colClasses = c("logical","character", "logical","logical","logical","numeric","logical","logical","logical","numeric","numeric","numeric","numeric","numeric","numeric")
colNames = "useBLStraw, country, penalty, rmcorcols, smooth, smoothbasisdim, use_umd, use_ccfr, use_nsum, mnlag, lag, saeForeMedian, saeForeHighNotch, saeSyncMedian, saeSyncHighNotch"



globalstats<- read.csv(text=colNames, colClasses=colClasses)
useBaselineStrawman=FALSE
for (country_iso in try_countries) {
  
  # ---
  # country_iso <- "DE"
  for (pen in c( TRUE)){#FALSE,
    for (rmcc in c( TRUE)){#FALSE,
      for (smth in c(FALSE)){#, TRUE
        if (smth){
          basisdim=15
        } else {
          basisdim=NA
        }
        for (umd in c(TRUE)){
          for (ccfr in c(FALSE)){#, TRUE
            for (nsum in c(0)){#, 1
              for (minlag in c(14)){#7, 
                for (useBaselineStrawman in c(FALSE)){
                  
                  
                  
                  filename<-paste0(path_symptom_lags,
                                   country_iso,
                                   "-cases-",minlag,"-60-pen",pen,"-alpha0.5-rmcc",rmcc,"-rmth0.9-smth",smth,basisdim,"-2020-09-10-2020-11-10-1-",umd,"-",ccfr,as.numeric(nsum),"-FALSE-estimates-lag-daily.csv")
                  print(paste("doing",filename))
                  if (file.exists(filename)){
                    dfall <- read.csv(file = filename) %>%
                      mutate(date = as.Date(date)) %>% 
                      mutate(cutoff = as.Date(cutoff))
                    for (lag_in in 1:minlag) {
                      df <- dfall %>% filter(lag == lag_in & predType == "nearFuture") 
                      
                      
                      
                      
                      
                      
                      
                      
                      df_baseline <- read.csv(file = paste0(path_baseline,
                                                            country_iso,
                                                            "-baseline.csv")) %>% 
                        mutate(date = as.Date(real_date)) %>% 
                        filter(date >= min(df$date) & date <= max(df$date))
                      
                      ######## debugging Carlos's sync method
                      # #CBM variation
                      # df$fore_sync <- df$fore
                      # for (p in seq(1,dim(df)[1]-7))
                      # {
                      #   delta <- df$fore[p+7]-df$fore[p]
                      #   df$fore_sync[p+7] <- df$y[p]+delta
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
                                  "Our Estimate" = my.palette[1],
                                  "Our Sync" = my.palette[2])
                      fileid=paste0(path_out, country_iso, "_lag_", lag_in,"-pen",pen,"-rmcc",rmcc,"-smth",smth,"-umd",umd,"-ccfr",ccfr,"-nsum",nsum,"-minlag",minlag,"-useBlStraw",useBaselineStrawman)
                      p1 <- ggplot() +
                        geom_line(data = df_baseline, aes(x = date, y = case_model, colour = "Delphi baseline"), size = 1, alpha = 0.6) +
                        geom_line(data = df_baseline, aes(x = date, y = case_fb_model, colour = "Delphi baseline w/ Fb. data"), size = 1, alpha = 0.6) +
                        geom_line(data = df, aes(x = date, y = fore, colour = "Our Estimate"), size = 1, alpha = 0.6) +
                        geom_line(data = df, aes(x = date, y = syncFore, colour = "Our Sync"), size = 1, alpha = 0.6) +
                        geom_point(data = df, aes(x = date, y = y, colour = "Official"), size = 2, alpha = 0.6) +
                        theme_light(base_size = 15) + 
                        labs(x = "Date", y = "Cases", colour = "", 
                             title = paste0(country_iso, ": lag ", lag_in)) +
                        scale_color_manual(values = colors) +
                        theme(legend.position = "bottom")
                      # print(p1)  
                      #    print(paste("saving",filename))
                      tryCatch({
                        ggsave(plot = p1,
                               filename = paste0(fileid, "plain_baseline_vs_glm.png"),
                               width = 9, height = 7)
                      },error=function(cond){
                        print(paste("error",cond," for p1 and file ",fileid))
                      })
                      
                      # Rolling mean:
                      k = 7 # rolling window
                      p2 <- ggplot() +
                        geom_line(data = df_baseline, aes(x = date, y = case_model, colour = "Delphi baseline"), size = 1, alpha = 0.6) +
                        geom_line(data = df_baseline, aes(x = date, y = case_fb_model, colour = "Delphi baseline w/ Fb. data"), size = 1, alpha = 0.6) +
                        geom_line(data = df, aes(x = date, 
                                                 y = rollmean(fore, k = k, fill = NA, align = "right"), 
                                                 colour = "Our Estimate"), size = 1, alpha = 0.6) +
                        geom_line(data = df, aes(x = date, y = rollmean(syncFore, k = k, fill = NA, align = "right"), 
                                                 colour = "Our Sync"), size = 1, alpha = 0.6) +
                        geom_point(data = df, aes(x = date, 
                                                  y = rollmean(y, k = k, fill = NA, align = "right"), 
                                                  colour = "Official"), size = 2, alpha = 0.6) +
                        theme_light(base_size = 15) + 
                        labs(x = "Date", y = "Cases", colour = "", 
                             title = paste0(country_iso, ": lag ", lag_in, " (", k, " days rolling mean)" )) +
                        scale_color_manual(values = colors) +
                        theme(legend.position = "bottom")
                      # print(p2)
                      tryCatch({
                        ggsave(plot = p2,
                               filename = paste0(fileid,  "_rollmean_", k, "_baseline_vs_glm.png"),
                               
                               width = 9, height = 7)
                      },error=function(cond){
                        print(paste("error",cond," for p2 and file ",fileid))
                      })
                      ## BOXPLOT ----
                      
                      if (useBaselineStrawman){
                        df_computed_sae<- df %>% 
                          dplyr::select(date, y, fore,syncFore) %>% 
                          inner_join(df_baseline, by = "date") 
                        
                      } else {
                        
                        yOnCutoffDate<-df %>% dplyr::select(date, y) %>% dplyr::rename(cutoff=date,strawman=y)
                        #by = c("first_name" = "name")
                        df_computed_sae <-df %>% dplyr::select(cutoff, date, y, fore,syncFore) %>% inner_join(yOnCutoffDate, by="cutoff")
                        
                      }
                      df_computed_sae<- df_computed_sae %>%
                        mutate(scaled_abs_err = abs(fore-y)/abs(strawman - y))%>%
                        mutate(scaled_abs_err_sync = abs(syncFore-y)/abs(strawman - y))
                      # we need to filter df_computed_sae so that we have no more leading NAs 
                      df_computed_sae <- na.trim(df_computed_sae, sides = 'left')
                      if (useBaselineStrawman){
                        df_check<- df_computed_sae %>% 
                          dplyr::select(date, y, fore,syncFore,scaled_abs_err,scaled_abs_err_sync) %>% 
                          inner_join(df_baseline, by = "date") %>%  
                          mutate(c_scaled_abs_err = abs(fore-y)/abs(strawman - y))%>%
                          mutate(c_scaled_abs_err_sync = abs(syncFore-y)/abs(strawman - y)) %>%
                          mutate(diff = c_scaled_abs_err - scaled_abs_err ) %>%
                          mutate(diffsync = c_scaled_abs_err_sync - scaled_abs_err_sync)
                      }else {
                        yOnCutoffDate<-df %>% dplyr::select(date, y) %>% dplyr::rename(cutoff=date,strawman=y)
                        df_check<- df_computed_sae %>% 
                          dplyr::select(date, cutoff, y, fore,syncFore,scaled_abs_err,scaled_abs_err_sync) %>% 
                          inner_join(yOnCutoffDate, by="cutoff") %>%  
                          mutate(c_scaled_abs_err = abs(fore-y)/abs(strawman - y))%>%
                          mutate(c_scaled_abs_err_sync = abs(syncFore-y)/abs(strawman - y)) %>%
                          mutate(diff = c_scaled_abs_err - scaled_abs_err ) %>%
                          mutate(diffsync = c_scaled_abs_err_sync - scaled_abs_err_sync)
                      }
                      write.csv(df_check,file = paste0(fileid,"-check-sae.csv"))
                      
                      ourMedian<-median(df_computed_sae$scaled_abs_err)
                      ourHighNotch<-computeHighNotch(df_computed_sae$scaled_abs_err)
                      syncMedian<-median(df_computed_sae$scaled_abs_err_sync)
                      syncHighNotch<-computeHighNotch(df_computed_sae$scaled_abs_err_sync)
                      
                      #  read.csv(text="country, penalty, rmcorcols, smooth, smoothbasisdim, use_umd, use_ccfr, use_nsum, mnlag, lag, saeForeMedian, saeForeHighNotch, saeSyncMedian, saeSyncHighNotch")
                      globalstats <- globalstats %>% add_row(useBLStraw=useBaselineStrawman, country=country_iso, penalty=pen, rmcorcols=rmcc, smooth=smth, smoothbasisdim=basisdim, use_umd=umd, use_ccfr=ccfr, use_nsum=nsum, mnlag=minlag, lag=lag_in, saeForeMedian=ourMedian, saeForeHighNotch=ourHighNotch, saeSyncMedian=syncMedian, saeSyncHighNotch=syncHighNotch)
                      df_box <- # data.frame(SAE = df_baseline$case_sae, Model = "Delphi baseline") %>% 
                        # rbind(data.frame(SAE = df_baseline$case_fb_model, Model = "Delphi baseline w/ Fb. data")) %>% 
                        rbind(data.frame(SAE = df_computed_sae$scaled_abs_err, Model = "Our Estimate")) %>% 
                        rbind(data.frame(SAE = df_computed_sae$scaled_abs_err_sync, Model = "Our Sync")) 
                      # notches are median +/- 1.58*IQR/sqrt(n)
                      p_box1 <-  ggplot(df_box, aes(y = SAE, x = Model)) +
                        geom_boxplot(notch=TRUE) +
                        theme_light(base_size = 15) +
                        geom_hline(yintercept=1, linetype="dashed", color = "red") +
                        annotate("point", x = "Our Estimate", y = ourHighNotch, colour = "blue", label="test" , size=0.1)+
                        annotate("point", x = "Our Sync", y = syncHighNotch, colour = "blue", label="test",size=0.1 )+
                        labs(x = "Model", y = "Scaled absolute error", 
                             title = paste0(country_iso, ": lag ", lag_in))
                      # print(p_box1)
                      tryCatch({ 
                        ggsave(plot = p_box1,
                               filename =   paste0(fileid,   "_boxplot_baseline_vs_glm.png"),
                               
                               width = 9, height = 7)
                      },error=function(cond){
                        print(paste("error",cond," for b1 and file ",fileid))
                      })
                      df_box <- #data.frame(SAE = df_baseline$case_sae, Model = "Delphi baseline") %>% 
                        # rbind(data.frame(SAE = df_baseline$case_fb_model, Model = "Delphi baseline w/ Fb. data")) %>% 
                        rbind(data.frame(SAE = df_computed_sae$scaled_abs_err, Model = "Our Estimate")) %>% 
                        rbind(data.frame(SAE = df_computed_sae$scaled_abs_err_sync, Model = "Our Sync")) 
                      
                      p_box2 <-  ggplot(df_box, aes(y = SAE, x = Model)) +
                        geom_boxplot(notch = TRUE) +
                        theme_light(base_size = 15) +
                        geom_hline(yintercept=1, linetype="dashed", color = "red") +
                        #ylim(NA, y_lim_box) +
                        coord_cartesian(ylim = c(NA, y_lim_box))+
                        annotate("point", x = "Our Estimate", y = ourHighNotch, colour = "blue", label="test", size=0.1 )+
                        annotate("point", x = "Our Sync", y = syncHighNotch, colour = "blue", label="test", size=0.1 )+
                        #  annotate("point", x = "Our Sync", y = median(df_computed_sae$scaled_abs_err_sync, na.rm = TRUE), colour = "red", label="test" )+
                        annotate("point", x = "Our Sync", y = median(df_computed_sae$scaled_abs_err_sync, na.rm = TRUE), colour = "green", label="test" )+
                        annotate("text", x = "Our Sync", y = syncHighNotch, colour = "blue", label=syncHighNotch )+
                        labs(x = "Model", y = "Scaled absolute error", 
                             title = paste0(country_iso, ": lag ", lag_in)
                        )
                      # print(p_box2)
                      tryCatch({
                        ggsave(plot = p_box2,
                               filename =  paste0(fileid,   "_boxplot2_baseline_vs_glm.png"),
                               width = 9, height = 7)
                      },error=function(cond){
                        print(paste("error",cond,"for b2 and file ",fileid))
                      })
                    }
                  } else {
                    print(paste("no file with name",filename))
                  }
                  
                  
                }
              }
            }
          }
        }
      }
    }      
  }
  # ---
  
} # country
write.csv(globalstats, paste0(path_out,"/predictorPerformance.csv"))


