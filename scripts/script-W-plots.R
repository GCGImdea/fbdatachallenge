library(dplyr)

path_W <- "../data/estimates-W/"
path_W_dunbar <- "../data/estimates-W-dunbar/"
# estimates_path <- "./estimates-W/"


do_plotting <-function(country ="ES",
                       estimates_path){

dt <- read.csv(paste0(estimates_path, "PlotData/", country, "-estimate.csv"), as.is = T)

# --- Ploting and saving

dt$fecha <- as.Date(dt$date, format = "%Y/%m/%d")

# -- Cummulative
png(file = paste0(estimates_path, "PlotData/", country, "-cumulative-estimate.png"))

plot(dt$fecha, dt$p_cases*100, type="l", xlab = "Date", 
     ylab = "% cum. incidence", main = "Cumulative incidence")
lines(dt$fecha, dt$p_cases_smooth*100,lty=1,col="blue")
legend("bottomright", legend=c("Original", "Smoothed"), 
       col=c("black", "blue"), lty = 1, cex=0.8)

# Save the file.
dev.off()

# -- Active cases
png(file = paste0(estimates_path, "PlotData/", country, "-active-estimate.png"))

plot(dt$fecha, dt$p_active*100, type="l", xlab = "Date", 
     ylab = "% active cases", main = "Active cases")
lines(dt$fecha, dt$p_cases_recent*100,lty=1,col="blue")
lines(dt$fecha, dt$p_active_smooth*100,lty=1,col="magenta")
lines(dt$fecha, dt$p_cases_recent_smooth*100,lty=1,col="red")
legend("topright", 
       legend=c("Active from cumulative", "Recent", "Active from smoothed cumulative", 
                "Recent smoothed"), 
       col=c("black", "blue", "magenta", "red"), lty = 1, cex=0.8)

# Save the file.
dev.off()
}

cat("Plotting ES\n")
do_plotting("ES", path_W)
do_plotting("ES", path_W_dunbar)
cat("Plotting BR\n")
do_plotting("BR", path_W)
do_plotting("BR", path_W_dunbar)
cat("Plotting US\n")
do_plotting("US", path_W)
do_plotting("US", path_W_dunbar)

