library(dplyr)

estimates_W_path <- "../data/estimates-W/PlotData/"
estimates_ccfr_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/estimates-ccfr-based/PlotData/"
estimates_UMD_path <- "../data/estimates-umd-batches/"
output_path <- "../data/all-estimates/PlotData/"



#do_plotting <-function(country ="ES"){
country <- "ES"

dt_W <- read.csv(paste0(estimates_W_path, country, "-estimate.csv"), as.is = T)
dt_W$date <- as.Date(dt_W$date, format = "%Y/%m/%d")
dt_ccfr <- read.csv(paste0(estimates_ccfr_path, country, "-estimate.csv"), as.is = T)
dt_ccfr$date <- as.Date(dt_ccfr$date, format = "%Y/%m/%d")
dt_UMD <- read.csv(paste0(estimates_UMD_path, country, "/", country, "_UMD_country_data.csv"), 
                   as.is = T)
dt_UMD$date <- as.Date(dt_UMD$date, format = "%Y-%m-%d")

dtwhole <- merge(dt_W, dt_ccfr, by="date")
dtwhole <- merge(dtwhole, dt_UMD, by="date")

dtplot <- dtwhole[, c("date",  "p_cases_recent_smooth",   "p_cases_active", "pct_cli_smooth")]
dtplot %>% rename(cs_recent = p_cases_recent_smooth, ccfr = p_cases_active, 
                  fb_umd = pct_cli_smooth) -> dtplot
dtplot$cs_recent <- dtplot$cs_recent*100*18/7
dtplot$ccfr <- dtplot$ccfr*100*18/12

dir.create(output_path, showWarnings = F)
cat(paste0("::- script-W: Writing the active data for ", country, "..\n"))
write.csv(dtplot, paste0(output_path, country, "-active.csv"))

# -- Active cases
png(file = paste0(output_path, country, "-active.png"))

ymax <- 3

plot(dtplot$date, dtplot$cs_recent, type="l", xlab = "Date", 
     ylab = "% active cases", main = "Active cases",
     # xlim=c(xmin, xmax), 
     ylim=c(0, ymax))
# lines(dtwhole$date, dtwhole$p_active*100000,lty=1,col="blue")
# lines(dtwhole$date, dtwhole$p_active_smooth*100000,lty=1,col="magenta")
lines(dtplot$date, dtplot$ccfr,lty=1,col="red")
lines(dtplot$date, dtplot$fb_umd,lty=1,col="brown")
legend("topright", 
       legend=c("Recent", 
                # "Active from cumulative", "Active from smoothed cumulative", 
                "Active CCFR", "FB UMD CLI"), 
       col=c("black", "blue", "magenta", "red", "brown"), lty = 1, cex=0.8)

# Save the file.
dev.off()

#do_plotting <-function(country ="BR"){
country <- "BR"

dt_W <- read.csv(paste0(estimates_W_path, country, "-estimate.csv"), as.is = T)
dt_W$date <- as.Date(dt_W$date, format = "%Y/%m/%d")
dt_ccfr <- read.csv(paste0(estimates_ccfr_path, country, "-estimate.csv"), as.is = T)
dt_ccfr$date <- as.Date(dt_ccfr$date, format = "%Y/%m/%d")
dt_UMD <- read.csv(paste0(estimates_UMD_path, country, "/", country, "_UMD_country_data.csv"), as.is = T)
dt_UMD$date <- as.Date(dt_UMD$date, format = "%Y-%m-%d")

dtwhole <- merge(dt_W, dt_ccfr, by="date")
dtwhole <- merge(dtwhole, dt_UMD, by="date")

dtplot <- dtwhole[, c("date",  "p_cases_recent_smooth",   "p_cases_active", "pct_cli_smooth")]
dtplot %>% rename(cs_recent = p_cases_recent_smooth, ccfr = p_cases_active, 
                  fb_umd = pct_cli_smooth) -> dtplot
dtplot$cs_recent <- dtplot$cs_recent*100*18/7
dtplot$ccfr <- dtplot$ccfr*100*18/12

dir.create(output_path, showWarnings = F)
cat(paste0("::- script-W: Writing the active data for ", country, "..\n"))
write.csv(dtplot, paste0(output_path, country, "-active.csv"))

# -- Active cases
png(file = paste0(output_path, country, "-active.png"))

ymax <- 3

plot(dtplot$date, dtplot$cs_recent, type="l", xlab = "Date", 
     ylab = "% active cases", main = "Active cases",
     # xlim=c(xmin, xmax), 
     ylim=c(0, ymax))
# lines(dtwhole$date, dtwhole$p_active*100000,lty=1,col="blue")
# lines(dtwhole$date, dtwhole$p_active_smooth*100000,lty=1,col="magenta")
lines(dtplot$date, dtplot$ccfr,lty=1,col="red")
lines(dtplot$date, dtplot$fb_umd,lty=1,col="brown")
legend("topright", 
       legend=c("Recent", 
                # "Active from cumulative", "Active from smoothed cumulative", 
                "Active CCFR", "FB UMD CLI"), 
       col=c("black", "blue", "magenta", "red", "brown"), lty = 1, cex=0.8)

# Save the file.
dev.off()




