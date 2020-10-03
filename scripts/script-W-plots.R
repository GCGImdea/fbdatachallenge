library(dplyr)

estimates_path <- "../data/estimates-W/"
# estimates_path <- "./estimates-W/"

country <- "ES"

dt <- read.csv(paste0(estimates_path, "PlotData/", country, "-estimate.csv"), as.is = T)

# --- Ploting and saving

dt$fecha <- as.Date(dt$date, format = "%Y/%m/%d")

# -- Cummulative
png(file = paste0(estimates_path, "PlotData/", country, "-cumulative-estimate.png"))

plot(dt$fecha, dt$p_cases*100000, type="l", xlab = "Date", 
     ylab = "Cum. incidence per 100,000", main = "Cumulative incidence")
lines(dt$fecha, dt$p_cases_smooth*100000,lty=1,col="red")
legend("bottomright", legend=c("Original", "Smoothed"), 
       col=c("red", "blue"), lty = 1:2, cex=0.8)

# Save the file.
dev.off()

# -- Active cases
png(file = paste0(estimates_path, "PlotData/", country, "-active-estimate.png"))

plot(dt$fecha, dt$p_cases_recent*100000, type="l", xlab = "Date", 
     ylab = "Active cases per 100,000", main = "Active cases")
lines(dt$fecha, dt$p_cases_recent_smooth*100000,lty=1,col="red")
lines(dt$fecha, dt$p_active*100000,lty=1,col="blue")
lines(dt$fecha, dt$p_active_smooth*100000,lty=1,col="magenta")
legend("topright", legend=c("Recent", "Recent smoothed", "Active", "Active from smooth"), 
       col=c("black", "red", "blue", "magenta"), lty = 1, cex=0.8)

# Save the file.
dev.off()
