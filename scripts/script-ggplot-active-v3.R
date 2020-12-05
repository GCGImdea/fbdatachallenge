library(dplyr)
library(ggplot2)
library(lubridate)

# smoothed p_cases and CI:
source("smooth_column-v2.R")


data_path <- "../data/all_giant_df/"
output_path <- "../data/all-estimates/"

smooth_param <- 15
active_window <- 18
ES_start_date <- "2020-06-15"
BR_start_date <- "2020-06-15"
end_date <- "2020-10-30"

do_plotting <-function(country ="ES", start_date, end_date){
        dtplot <- read.csv(paste0(data_path, country, "_alldf.csv"))
        
        dtplot$date <- as.Date(dtplot$date)
        dtplot <- dtplot[dtplot$date >= ymd(start_date),]
        dtplot <- dtplot[dtplot$date <= ymd(end_date),]

        dtplot <- smooth_column(dtplot, "pct_no_public", basis_dim = smooth_param, link_in ="log", monotone = F)
        dtplot <- smooth_column(dtplot, "pct_wear_mask_none_time", basis_dim = smooth_param, link_in ="log", monotone = F)
        dtplot <- smooth_column(dtplot, "pct_cmnty_sick", basis_dim = smooth_param, link_in ="log", monotone = F)
        
        dtplot <- smooth_column(dtplot, "pct_chills", basis_dim = smooth_param, link_in ="log", monotone = F)
        dtplot <- smooth_column(dtplot, "pct_cli", basis_dim = smooth_param, link_in ="log", monotone = F)
        dtplot <- smooth_column(dtplot, "pct_anosmia_ageusia", basis_dim = smooth_param, link_in ="log", monotone = F)
        dtplot <- smooth_column(dtplot, "cases", basis_dim = smooth_param, link_in ="log", monotone = F)
        
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
        
        colors <- c("pct_cmnty_sick" = my.palette[1], 
                    "pct_anosmia_ageusia" = my.palette[5], 
                    "pct_cli" = my.palette[3],
                    "Corrected-CFR" = my.palette[2], 
                    "Confirmed" = my.palette[4])
        
        # if (country == "ES") {
        #         up.limit = 1.3
        # }else {
        #         up.limit = 2.5
        # }
        

        p1 <- ggplot(data = dtplot, aes(x = date)) +
                # geom_line(aes(y = 0.3681253122*pct_no_public/population*100, colour = "pct_no_public"), size = 1) +
                # geom_line(aes(y = 0.4581420115*pct_wear_mask_none_time/population*100, colour = "pct_no_public"), size = 1) +
                geom_line(aes(y = 0.1720012039*pct_cmnty_sick_smooth/population*100, colour = "pct_cmnty_sick"), size = 1) +
                geom_line(aes(y = 0.7147426159*pct_anosmia_ageusia_smooth/population*100, colour = "pct_anosmia_ageusia"), size =1) + 
                geom_line(aes(y = 2.240209451*pct_cli_smooth/population*100, colour = "pct_cli"), size =1) + 
                geom_line(aes(y = cases_active/population*100, colour = "Corrected-CFR"), size = 1) + 
                geom_line(aes(y = active_window * cases_smooth/population*100, colour = "Confirmed"), size =1) +
                theme_bw() + 
                # ylim(-0.1, up.limit) +
                labs(x = "Date", y =  "% active cases", title = country_large,  colour = "", size=20) +
                scale_color_manual(values = colors) + 
                theme(legend.position = "bottom",
                      legend.text = element_text(size = 11),
                      axis.text=element_text(size=20),
                      axis.title =element_text(size=20),
                      title=element_text(size=20))
        print(p1)
        
        # Save the file.
        ggsave(plot = p1,
               filename =  paste0(output_path, country, "-active.png"),
               width = 8, height = 6)
        
        
}

do_plotting("ES", start_date = ES_start_date, end_date)
do_plotting("BR", start_date = BR_start_date, end_date)
