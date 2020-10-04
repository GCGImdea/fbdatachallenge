library(dplyr)

estimates_W_path <- "../data/estimates-W/PlotData/"
estimates_ccfr_path <- "https://raw.githubusercontent.com/GCGImdea/coronasurveys/master/data/estimates-ccfr-based/PlotData/"
estimates_UMD_path <- "../data/estimates-umd-batches/spain/ES_UMD_country_data.csv"
output_path <- "../data/all-estimates/PlotData/"

#do_plotting <-function(country ="ES"){
country <- "ES"

dt_W <- read.csv(paste0(estimates_W_path, country, "-estimate.csv"), as.is = T)
dt_W$date <- as.Date(dt_W$date, format = "%Y/%m/%d")
dt_ccfr <- read.csv(paste0(estimates_ccfr_path, country, "-estimate.csv"), as.is = T)
dt_ccfr$date <- as.Date(dt_ccfr$date, format = "%Y/%m/%d")
dt_UMD <- read.csv(estimates_UMD_path, as.is = T)
dt_UMD$date <- as.Date(dt_UMD$date, format = "%Y-%m-%d")

dtwhole <- merge(dt_W, dt_ccfr, by="date")
dtwhole <- merge(dtwhole, dt_UMD, by="date")

# -- Active cases
png(file = paste0(output_path, country, "-active.png"))

ymax <- max(dtwhole$p_active*100000, na.rm = TRUE)

plot(dtwhole$date, dtwhole$p_cases_recent*100000, type="l", xlab = "Date", 
     ylab = "Active cases per 100,000", main = "Active cases",
     # xlim=c(xmin, xmax), 
     ylim=c(0, ymax))
lines(dtwhole$date, dtwhole$p_active*100000,lty=1,col="blue")
lines(dtwhole$date, dtwhole$p_active_smooth*100000,lty=1,col="magenta")
lines(dtwhole$date, dtwhole$p_cases_active*100000,lty=1,col="red")
lines(dtwhole$date, dtwhole$pct_cli_smooth*1000,lty=1,col="brown")
legend("topright", 
       legend=c("Recent", "Active from cumulative", "Active from smoothed cumulative"
                , "Active CCFR", "UMD CLI"
       ), 
       col=c("black", "blue", "magenta", "red", "brown"), lty = 1, cex=0.8)

# Save the file.
dev.off()

#do_plotting <-function(country ="BR"){
country <- "BR"

dt_W <- read.csv(paste0(estimates_W_path, country, "-estimate.csv"), as.is = T)
dt_W$date <- as.Date(dt_W$date, format = "%Y/%m/%d")
dt_ccfr <- read.csv(paste0(estimates_ccfr_path, country, "-estimate.csv"), as.is = T)
dt_ccfr$date <- as.Date(dt_ccfr$date, format = "%Y/%m/%d")
# dt_UMD <- read.csv(estimates_UMD_path, as.is = T)
# dt_UMD$date <- as.Date(dt_UMD$date, format = "%Y-%m-%d")

dtwhole <- merge(dt_W, dt_ccfr, by="date")
# dtwhole <- merge(dtwhole, dt_UMD, by="date")

# -- Active cases
png(file = paste0(output_path, country, "-active.png"))

ymax <- max(dtwhole$p_active*100000, na.rm = TRUE)

plot(dtwhole$date, dtwhole$p_cases_recent*100000, type="l", xlab = "Date", 
     ylab = "Active cases per 100,000", main = "Active cases",
     # xlim=c(xmin, xmax), 
     ylim=c(0, ymax))
lines(dtwhole$date, dtwhole$p_active*100000,lty=1,col="blue")
lines(dtwhole$date, dtwhole$p_active_smooth*100000,lty=1,col="magenta")
lines(dtwhole$date, dtwhole$p_cases_active*100000,lty=1,col="red")
# lines(dtwhole$date, dtwhole$pct_cli_smooth*1000,lty=1,col="brown")
legend("topright", 
       legend=c("Recent", "Active from cumulative", "Active from smoothed cumulative"
                , "Active CCFR", "UMD CLI"
       ), 
       col=c("black", "blue", "magenta", "red", "brown"), lty = 1, cex=0.8)

# Save the file.
dev.off()




