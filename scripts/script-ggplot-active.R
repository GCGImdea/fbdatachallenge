library(dplyr)
library(ggplot2)

estimates_W_path <- "../data/estimates-W/PlotData/"
# estimates_ccfr_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/estimates-ccfr-based/PlotData/"
estimates_ccfr2_path <- "../data/estimates-ccfr-based/"
estimates_UMD_path <- "../data/estimates-umd-batches/"
output_path <- "../data/all-estimates/PlotData/"

active_window <- 18

do_plotting <-function(country ="ES"){

dt_W <- read.csv(paste0(estimates_W_path, country, "-estimate.csv"), as.is = T)
dt_W$date <- as.Date(dt_W$date, format = "%Y/%m/%d")
# dt_ccfr <- read.csv(paste0(estimates_ccfr_path, country, "-estimate.csv"), as.is = T)
# dt_ccfr$date <- as.Date(dt_ccfr$date, format = "%Y/%m/%d")

dt_ccfr2 <- read.csv(paste0(estimates_ccfr2_path, country, "-country-ccfr-aggregate-estimate.csv"), 
                     as.is = T)
dt_ccfr2$date <- as.Date(dt_ccfr2$date, format = "%Y-%m-%d")

dt_UMD <- read.csv(paste0(estimates_UMD_path, country, "/", country, "_UMD_country_data.csv"), 
                   as.is = T)
dt_UMD$date <- as.Date(dt_UMD$date, format = "%Y-%m-%d")

dtwhole <- merge(dt_W, dt_ccfr2, by="date")
dtwhole <- merge(dtwhole, dt_UMD, by="date")

dtplot <- dtwhole[, c("date",  "p_cases_recent_smooth",   "total_cases_active", 
                      "pct_cli_smooth", "population")]
dtplot %>% rename(cs_recent = p_cases_recent_smooth, ccfr = total_cases_active, 
                  fb_umd = pct_cli_smooth) -> dtplot
dtplot$cs_recent <- dtplot$cs_recent*100*active_window/7
dtplot$ccfr <- dtplot$ccfr*100/dtplot$population

dir.create(output_path, showWarnings = F)
cat(paste0("::- script-W: Writing the active data for ", country, "..\n"))
write.csv(dtplot, paste0(output_path, country, "-active.csv"))

# -- Active cases
if (country == "ES") {
        country_large = "Spain"
}else if (country == "BR") {
        country_large = "Brazil"
}

colors <- c("CS-Recent" = "black", 
            "CCFR-based" = "red", 
            "Batched CSDC CLI (smooth)" = "blue")

p1 <- ggplot(data = dtplot, aes(x = date)) +
        geom_line(aes(y = cs_recent, colour = "CS-Recent"), alpha = 0.6, size = 1) +
        geom_line(aes(y = ccfr, colour = "CCFR-based"), size = 1, alpha = 0.6) + 
        geom_line(aes(y = fb_umd, colour = "Batched CSDC CLI (smooth)"), size =1, alpha = 0.6) +
        theme_bw() + 
        labs(x = "Date", y =  "% symptomatic cases", title = country_large,  colour = "") +
        scale_color_manual(values = colors) + 
        theme(legend.position = "bottom") #c(0.8, 0.85)
print(p1)

# Save the file.
ggsave(plot = p1,
       filename =  paste0(output_path, country, "-active.png"),
       width = 7, height = 5)


}

do_plotting("ES")
do_plotting("BR")

