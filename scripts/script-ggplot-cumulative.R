library(dplyr)
library(ggplot2)
library(lubridate)

estimates_NSUM_path <- "../data/estimates-W/PlotData/"
estimates_CCFR_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/estimates-ccfr-based/PlotData/"
# estimates_ccfr2_path <- "../data/estimates-ccfr-based/"
estimates_UMD_path <- "../data/estimates-umd-batches/"
estimates_W_dunbar_path <- "../data/estimates-W-dunbar/PlotData/"

output_path <- "../data/all-estimates/"

start_date <- "2020-03-01"


#do_CCFR_plotting <-function(countries){
        # estimates W
        dt_CCFR_ES <- read.csv(paste0(estimates_CCFR_path, "ES-estimate.csv"), as.is = T)
        dt_CCFR_ES$date <- as.Date(dt_CCFR_ES$date, format = "%Y/%m/%d")
        dt_CCFR_ES <- dt_CCFR_ES[dt_CCFR_ES$date >= ymd(start_date),]
        
        #dt_ES %>% rename(p_CCFR_infected_ES = p_cases_infected) -> dt_ES
        
        dt_CCFR_BR <- read.csv(paste0(estimates_CCFR_path, "BR-estimate.csv"), as.is = T)
        dt_CCFR_BR$date <- as.Date(dt_CCFR_BR$date, format = "%Y/%m/%d")
        dt_CCFR_BR <- dt_CCFR_BR[dt_CCFR_BR$date >= ymd(start_date),]
        #dt_BR %>% rename(p_CCFR_infected_BR = p_cases_infected) -> dt_BR
        
        dt_CCFR_US <- read.csv(paste0(estimates_CCFR_path, "US-estimate.csv"), as.is = T)
        dt_CCFR_US$date <- as.Date(dt_CCFR_US$date, format = "%Y/%m/%d")
        dt_CCFR_US <- dt_CCFR_US[dt_CCFR_US$date >= ymd(start_date),]
        #dt_US %>% rename(p_CCFR_infected_US = p_cases_infected) -> dt_US
        
        # merge all data
        # dtwhole <- merge(dt_ES, dt_BR, by="date")
        # dtwhole <- merge(dtwhole, dt_US, by="date")
        # 
        # dtplot_CCFR <- dtwhole[dtwhole$date >= ymd(start_date), c("date",  
        #                       "p_CCFR_infected_ES", 
        #                       "p_CCFR_infected_BR",
        #                       "p_CCFR_infected_US")]
        # 
        # dir.create(output_path, showWarnings = F)
        # cat(paste0("::- script-W: Writing the cumulative CCFR data \n"))
        # write.csv(dtplot_CCFR, paste0(output_path, "CCFR-cumulative.csv"))
        
        # 5-colors palettes:
        my.palette <- c("red", "blue", "black", "magenta", "brown")
        #my.palette <- c("#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#073b4c")
        # my.palette <- paste0("#", c("264653","2a9d8f","e9c46a","f4a261","e76f51"))
        # my.palette <- paste0("#", c("ffbe0b","fb5607","ff006e","8338ec","3a86ff"))
        
        colors <- c("Spain" = my.palette[1], 
                    "Brazil" = my.palette[2], 
                    "USA" = my.palette[3])
        
        p1 <- ggplot(data = dt_CCFR_ES, aes(x = date)) +
                geom_line(aes(y = 100*p_cases_infected, colour = "Spain"), size = 1) + 
                geom_line(data=dt_CCFR_BR, aes(y = 100*p_cases_infected, colour = "Brazil"), size = 1) + 
                geom_line(data=dt_CCFR_US, aes(y = 100*p_cases_infected, colour = "USA"), size =1) + 
                geom_line(aes(y = 100*cum_cases/population, colour = "Spain"), size = 1, linetype = "dashed", alpha=0.5) + 
                geom_line(data=dt_CCFR_BR, aes(y = 100*cum_cases/population, colour = "Brazil"), size = 1, linetype = "dashed", alpha=0.5) + 
                geom_line(data=dt_CCFR_US, aes(y = 100*cum_cases/population, colour = "USA"), size =1, linetype = "dashed", alpha=0.5) +
                # geom_line(aes(y = confirmed_active, colour = "Confirmed active cases"), size =1) +
                theme_bw() + 
                #ylim(-0.1, up.limit) +
                labs(x = "Date", y =  "% population cumulative incidente", title = "cCFR-based Estimates vs Confirmed Cumulative Incidence",  colour = "") +
                scale_color_manual(values = colors) + 
                theme(legend.position = "bottom") 
        print(p1)
        
        # Save the file.
        ggsave(plot = p1,
               filename =  paste0(output_path, "CCFR-cumulative.png"),
               width = 8, height = 6)
        
#        }

#do_NSUM_plotting <-function(countries){
        # estimates W
        dt_NSUM_ES <- read.csv(paste0(estimates_NSUM_path, "ES-estimate.csv"), as.is = T)
        dt_NSUM_ES$date <- as.Date(dt_NSUM_ES$date)
        dt_NSUM_ES %>% rename(p_NSUM_infected = p_cases_smooth) -> dt_NSUM_ES
        
        dt_NSUM_BR <- read.csv(paste0(estimates_NSUM_path, "BR-estimate.csv"), as.is = T)
        dt_NSUM_BR$date <- as.Date(dt_NSUM_BR$date)
        dt_NSUM_BR %>% rename(p_NSUM_infected = p_cases_smooth) -> dt_NSUM_BR
        
        dt_NSUM_US <- read.csv(paste0(estimates_NSUM_path, "US-estimate.csv"), as.is = T)
        dt_NSUM_US$date <- as.Date(dt_NSUM_US$date)
        dt_NSUM_US %>% rename(p_NSUM_infected = p_cases_smooth) -> dt_NSUM_US
        
        # # merge all data
        # dtwhole <- merge(dt_ES, dt_BR, by="date")
        # dtwhole <- merge(dtwhole, dt_US, by="date")
        # 
        # dtplot_NSUM <- dtwhole[dtwhole$date >= ymd(start_date), c("date",  
        #                                                      "p_NSUM_infected_ES", 
        #                                                      "p_NSUM_infected_BR",
        #                                                      "p_NSUM_infected_US")]
        # 
        # dir.create(output_path, showWarnings = F)
        # cat(paste0("::- script-W: Writing the cumulative NSUM data \n"))
        # write.csv(dtplot_NSUM, paste0(output_path, "NSUM-cumulative.csv"))
        
        # 5-colors palettes:
        my.palette <- c("red", "blue", "black", "magenta", "brown")
        #my.palette <- c("#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#073b4c")
        # my.palette <- paste0("#", c("264653","2a9d8f","e9c46a","f4a261","e76f51"))
        # my.palette <- paste0("#", c("ffbe0b","fb5607","ff006e","8338ec","3a86ff"))
        
        colors <- c("Spain" = my.palette[1], 
                    "Brazil" = my.palette[2], 
                    "USA" = my.palette[3])
        
        p1 <- ggplot(data = dt_NSUM_ES, aes(x = date)) +
                #p1 <- ggplot(data = dtplot_NSUM, aes(x = date)) +
                geom_line(aes(y = 100*p_NSUM_infected, colour = "Spain"), size = 1, linetype = "solid") +
                geom_line(data=dt_NSUM_BR, aes(y = 100*p_NSUM_infected, colour = "Brazil"), size = 1, linetype = "solid") +
                geom_line(data=dt_NSUM_US, aes(y = 100*p_NSUM_infected, colour = "USA"), size =1, linetype = "solid") +
                geom_line(aes(y = 100*p_cases, colour = "Spain"), size = 1, linetype = "dashed", alpha=0.5) +
                geom_line(data=dt_NSUM_BR, aes(y = 100*p_cases, colour = "Brazil"), size = 1, linetype = "dashed", alpha=0.5) +
                geom_line(data=dt_NSUM_US, aes(y = 100*p_cases, colour = "USA"), size =1, linetype = "dashed", alpha=0.5) +
                # geom_line(aes(y = 100*p_NSUM_infected_ES, colour = "Spain"), size = 1) + 
                # geom_line(aes(y = 100*p_NSUM_infected_BR, colour = "Brazil"), size = 1) + 
                # geom_line(aes(y = 100*p_NSUM_infected_US, colour = "USA"), size =1) + 
                # geom_line(aes(y = confirmed_active, colour = "Confirmed active cases"), size =1) +
                theme_bw() + 
                #ylim(-0.1, up.limit) +
                labs(x = "Date", y =  "% population cumulative incidente", title = "NSUM Estimates of Cumulative Incidence",  colour = "") +
                scale_color_manual(values = colors) + 
                theme(legend.position = "bottom") 
        print(p1)
        
        # Save the file.
        ggsave(plot = p1,
               filename =  paste0(output_path, "NSUM-cumulative.png"),
               width = 8, height = 6)
        
#}

        # 5-colors palettes:
        my.palette <- c("red", "blue", "black", "magenta", "brown")
        #my.palette <- c("#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#073b4c")
        # my.palette <- paste0("#", c("264653","2a9d8f","e9c46a","f4a261","e76f51"))
        # my.palette <- paste0("#", c("ffbe0b","fb5607","ff006e","8338ec","3a86ff"))
        
        colors <- c("Spain" = my.palette[1], 
                    "Brazil" = my.palette[2], 
                    "USA" = my.palette[3]
                    )
        
        p1 <- ggplot(data = dt_CCFR_ES, aes(x = date)) +
                geom_line(aes(y = 100*p_cases_infected, colour = "Spain"), size = 1, linetype = "dashed", alpha=0.5) + 
                geom_line(data=dt_CCFR_BR, aes(y = 100*p_cases_infected, colour = "Brazil"), size = 1, linetype = "dashed", alpha=0.5) + 
                geom_line(data=dt_CCFR_US, aes(y = 100*p_cases_infected, colour = "USA"), size =1, linetype = "dashed", alpha=0.5) +
                geom_line(data=dt_NSUM_ES, aes(y = 100*p_NSUM_infected, colour = "Spain"), size = 1, linetype = "solid") + 
                geom_line(data=dt_NSUM_BR, aes(y = 100*p_NSUM_infected, colour = "Brazil"), size = 1, linetype = "solid") + 
                geom_line(data=dt_NSUM_US, aes(y = 100*p_NSUM_infected, colour = "USA"), size =1, linetype = "solid") + 
                # geom_line(aes(y = confirmed_active, colour = "Confirmed active cases"), size =1) +
                theme_bw() + 
                #ylim(-0.1, up.limit) +
                labs(x = "Date", y =  "% population cumulative incidente", title = "NSUM vs CCFR Estimates of Cumulative Incidence",  colour = "") +
                scale_color_manual(values = colors) + 
                theme(legend.position = "bottom") 
        print(p1)
        
        # Save the file.
        ggsave(plot = p1,
               filename =  paste0(output_path, "CCFR-NSUM-cumulative.png"),
               width = 8, height = 6)
        

# do_CCFR_plotting(c("ES", "BR", "US"))
#                  
# do_NSUM_plotting(c("ES", "BR", "US"))

