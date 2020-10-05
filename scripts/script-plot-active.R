library(dplyr)

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
png(file = paste0(output_path, country, "-active.png"))

ymax <- 2.5

plot(dtplot$date, dtplot$cs_recent, type="l", xlab = "Date", 
     ylab = "% symptomatic cases", main = paste0("Symptomatic cases: ", country),
     # xlim=c(xmin, xmax), 
     ylim=c(0, ymax))
# lines(dtwhole$date, dtwhole$p_active*100000,lty=1,col="blue")
# lines(dtwhole$date, dtwhole$p_active_smooth*100000,lty=1,col="magenta")
lines(dtplot$date, dtplot$ccfr,lty=1,col="red")
lines(dtplot$date, dtplot$fb_umd,lty=1,col="blue")
legend("topright", 
       legend=c("CS-Recent", 
                # "Active from cumulative", "Active from smoothed cumulative", 
                "CCFR-based", "CSDC CLI"), 
       col=c("black", "red", "blue", "magenta", "brown"), lty = 1, cex=0.8)

# Save the file.
dev.off()

}

do_plotting("ES")
do_plotting("BR")

