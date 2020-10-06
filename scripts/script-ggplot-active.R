library(dplyr)
library(ggplot2)

estimates_W_path <- "../data/estimates-W/PlotData/"
# estimates_ccfr_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/estimates-ccfr-based/PlotData/"
estimates_ccfr2_path <- "../data/estimates-ccfr-based/"
estimates_UMD_path <- "../data/estimates-umd-batches/"
output_path <- "../data/all-estimates/"

estimates_W_dunbar_path <- "../data/estimates-W-dunbar/PlotData/"

active_window <- 18

do_plotting <-function(country ="ES"){
        # estimates W
        dt_W <- read.csv(paste0(estimates_W_path, country, "-estimate.csv"), as.is = T)
        dt_W$date <- as.Date(dt_W$date, format = "%Y/%m/%d")
        
        # estimates W-dunbar
        dt_W_dunb <- read.csv(paste0(estimates_W_dunbar_path, country, "-estimate.csv"), as.is = T)
        dt_W_dunb$date <- as.Date(dt_W_dunb$date, format = "%Y/%m/%d")
        colnames(dt_W_dunb)[4:ncol(dt_W_dunb)] <- paste0(colnames(dt_W_dunb)[4:ncol(dt_W_dunb)], "_dunb")
        
        # estimates cCFR
        dt_ccfr2 <- read.csv(paste0(estimates_ccfr2_path, country, "-country-ccfr-aggregate-estimate.csv"), 
                             as.is = T)
        dt_ccfr2$date <- as.Date(dt_ccfr2$date, format = "%Y-%m-%d")
        
        # estimates UMD data
        dt_UMD <- read.csv(paste0(estimates_UMD_path, country, "/", country, "_UMD_country_data.csv"), 
                           as.is = T)
        dt_UMD$date <- as.Date(dt_UMD$date, format = "%Y-%m-%d")
        
        
        # merge all data
        dtwhole <- merge(dt_W, dt_W_dunb, by="date")
        dtwhole <- merge(dtwhole, dt_ccfr2, by="date")
        dtwhole <- merge(dtwhole, dt_UMD, by="date")
        
        dtplot <- dtwhole[, c("date",  
                              "p_cases_recent_smooth", 
                              "p_cases_recent_smooth_dunb",
                              "total_cases", 
                              "total_cases_active", 
                              "pct_cli_smooth", 
                              "population")]
        
        dtplot %>% rename(cs_recent = p_cases_recent_smooth, 
                          cs_dunb_recent = p_cases_recent_smooth_dunb,
                          ccfr = total_cases_active, 
                          fb_umd = pct_cli_smooth, 
                          confirmed = total_cases) -> dtplot
        
        dtplot$cs_recent <- dtplot$cs_recent*100*active_window/7
        dtplot$cs_dunb_recent <- dtplot$cs_dunb_recent*100*active_window/7
        dtplot$ccfr <- dtplot$ccfr*100/dtplot$population
        
        # confirmed active cases:
        dtplot$confirmed_active <- cumsum(c(dtplot$confirmed[1:active_window],
                                            diff(dtplot$confirmed, lag = active_window)))
        dtplot$confirmed_active <- dtplot$confirmed_active*100/dtplot$population
        
        dir.create(output_path, showWarnings = F)
        cat(paste0("::- script-W: Writing the active data for ", country, "..\n"))
        write.csv(dtplot, paste0(output_path, country, "-active.csv"))
        
        # -- Active cases
        if (country == "ES") {
                country_large = "Spain"
        }else if (country == "BR") {
                country_large = "Brazil"
        }
        
        # 5-colors palettes:
        my.palette <- c("#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#073b4c")
        # my.palette <- paste0("#", c("264653","2a9d8f","e9c46a","f4a261","e76f51"))
        # my.palette <- paste0("#", c("ffbe0b","fb5607","ff006e","8338ec","3a86ff"))
        
        colors <- c("CS-Recent" = my.palette[1], 
                    "CS-Dunbar-Recent" = my.palette[5], 
                    "CCFR-based" = my.palette[2], 
                    "Batched CSDC CLI (smooth)" = my.palette[3],
                    "Confirmed active cases" = my.palette[4])
        
        if (country == "ES") {
                up.limit = 2
        }else {
                up.limit = 2.5
        }
        
        
        
        p1 <- ggplot(data = dtplot, aes(x = date)) +
                geom_line(aes(y = cs_recent, colour = "CS-Recent"), size = 1) +
                # geom_line(aes(y = cs_dunb_recent, colour = "CS-Dunbar-Recent"), size = 1) +
                geom_line(aes(y = ccfr, colour = "CCFR-based"), size = 1) + 
                geom_line(aes(y = fb_umd, colour = "Batched CSDC CLI (smooth)"), size =1) +
                geom_line(aes(y = confirmed_active, colour = "Confirmed active cases"), size =1) +
                theme_bw() + ylim(-0.1, up.limit) +
                labs(x = "Date", y =  "% symptomatic cases", title = country_large,  colour = "") +
                scale_color_manual(values = colors) + 
                theme(legend.position = "bottom") 
        print(p1)
        
        # Save the file.
        ggsave(plot = p1,
               filename =  paste0(output_path, country, "-active.png"),
               width = 8, height = 6)
}

do_plotting("ES")
do_plotting("BR")

