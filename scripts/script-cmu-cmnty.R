library(dplyr)
library(zoo)

reach <- 150
smooth_param <- 15

estimates_path <- "../data/cmu-state/cmu-state-ma.csv"
output_path <- "../data/cmu-state/cmu-state-ma.png"


#--------------------------------------------------------
smooth_column <- function(df_in, col_s, basis_dim = 15){
        require(mgcv)
        
        # add a number of "day" column:
        to.smooth <- df_in
        to.smooth$day <- 1:nrow(to.smooth)
        
        # change the name of column to be smoothed:
        colnames(to.smooth)[colnames(to.smooth) == col_s] = "y"
        
        # not NA elements to be smoothed:
        ind_not_na <- !is.na(to.smooth$y)
        
        # data to be smoothed:
        to.smooth <- to.smooth[ind_not_na, c("y", "day")]
        
        # Mono-smoothing with scam ----
        b1 <- gam(y ~ s(day, k = basis_dim, bs="ps"),
                  data=to.smooth)
        
        # save to column "xxx_smooth":
        df_in$y_smooth <- NA
        # df_in[frst_n_zero:nrow(df_in) , "y_smooth"] <- b1$fitted.values
        newd <- data.frame(day = 1:nrow(df_in))
        df_in[ , "y_smooth"] <- predict(b1, newd)
        colnames(df_in)[colnames(df_in) == "y_smooth"] <- paste0(col_s, 
                                                                 "_smooth")
        return(df_in)
}
#---------------------------------------------------------------------------


df <- read.csv(estimates_path, as.is = T)
df$date <- as.Date(df$date, format = "%Y-%m-%d")

reach_cutoff <- boxplot.stats(df$mean_cmnty_cli_ct)$stats[5] # changed cutoff to upper fence
df[df$mean_cmnty_cli_ct > reach_cutoff, "mean_cmnty_cli_ct"] <- NA

df$nsum <- 100 * df$mean_cmnty_cli_ct / reach

df <- smooth_column(df, "pct_cli", smooth_param)
df$nsum_roll <- rollmean(df$nsum, 3, fill=NA)
df <- smooth_column(df, "nsum_roll", 15)

library(ggplot2)
p_temp <- ggplot(data = df, aes(x = date)) +
        geom_point(aes(y = pct_cli), color = "blue", alpha = 0.2) +
        geom_line(aes(y = pct_cli_smooth), color = "blue", size = 0.6) +
        geom_point(aes(y = nsum), color = "red", alpha = 0.2) +
        geom_line(aes(y = nsum_roll), color = "red", alpha = 0.2, size = 0.6) +
        geom_line(aes(y = nsum_roll_smooth), color = "red", alpha = 0.6, size = 1) +
        theme_bw()
p_temp 

# -- Active cases
png(file = output_path)

ymax <- 4

plot(df$date, df$pct_cli_smooth, type="l", xlab = "Date", 
     ylab = "% symptomatic cases", main = "Symptomatic cases",
     # xlim=c(xmin, xmax), 
     ylim=c(0, ymax)
     )
# lines(dtwhole$date, dtwhole$p_active*100000,lty=1,col="blue")
# lines(dtwhole$date, dtwhole$p_active_smooth*100000,lty=1,col="magenta")
lines(df$date, df$nsum_roll_smooth,lty=1,col="red")
# lines(dtplot$date, dtplot$fb_umd,lty=1,col="blue")
legend("topright", 
       legend=c("pct_cli", 
                # "Active from cumulative", "Active from smoothed cumulative", "CCFR-based", 
                "NSUM"), 
       col=c("black", "red", "blue", "magenta", "brown"), lty = 1, cex=0.8)

# Save the file.
dev.off()
