library(dplyr)

path_W <- "../data/estimates-W/"
path_W_dunbar <- "../data/estimates-W-dunbar/"
# estimates_path <- "./estimates-W/"


do_plotting <-function(country ="ES",
                       estimates_path,
                       reach_text){

dt <- read.csv(paste0(estimates_path, "PlotData/", country, "-estimate-smooth.csv"), as.is = T)

# --- Ploting and saving

dt$fecha <- as.Date(dt$date, format = "%Y-%m-%d")

# -- Cummulative
png(file = paste0(estimates_path, "plots/", country, "-cumulative-estimate.png"))

plot(dt$fecha, dt$p_cases*100, type="l", xlab = "Date", 
     ylab = "% cum. incidence", main = paste0("Cumulative incidence (Reach ", reach_text,")"))
lines(dt$fecha, dt$p_cases_smooth*100,lty=1,col="blue")
legend("bottomright", legend=c("Original", "Smoothed"), 
       col=c("black", "blue"), lty = 1, cex=0.8)

# Save the file.
dev.off()

# -- Active cases
png(file = paste0(estimates_path, "plots/", country, "-active-estimate.png"))

plot(dt$fecha, dt$p_active*100, type="l", xlab = "Date", 
     ylab = "% active cases", main = paste0("Active cases (Reach ", reach_text,")"))
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
do_plotting("ES", path_W, "per response")
do_plotting("ES", path_W_dunbar, "67.24397")
cat("Plotting BR\n")
do_plotting("BR", path_W, "per response")
do_plotting("BR", path_W_dunbar, "59.06486")
cat("Plotting US\n")
do_plotting("US", path_W, "per response")
do_plotting("US", path_W_dunbar, "37.45614")
do_plotting("PT", path_W, "per response")
do_plotting("PT", path_W_dunbar, "65.00961")
