library(dplyr)


reach <- 150
smooth_param <- 15

estimates_path <- "../data/cmu-state/cmu-state-ma.csv"
output_path <- "../data/cmu-state/cmu-state-ma.png"


#--------------------------------------------------------
smooth_column <- function(df_in, col_s, basis_dim = 15){
        
        ## List of packages
        packages = c("scam")
        ## Install&load all
        package.check <- lapply(
                packages,
                FUN = function(x) {
                        if (!require(x, character.only = TRUE)) {
                                install.packages(x, dependencies = TRUE)
                                library(x, character.only = TRUE)
                        }
                }
        )
        
        # add a number of "day" column:
        to.smooth <- df_in
        to.smooth$day <- 1:nrow(to.smooth)
        
        # change the name of column to be smoothed:
        colnames(to.smooth)[colnames(to.smooth) == col_s] = "y"
        
        # first non zero element to be smoothed:
        frst_n_zero <- head(to.smooth[to.smooth$y!=0, "day"], 1)
        
        cat("Smoothing starting at row ", frst_n_zero, "..\n")
        
        # data to be smoothed:
        to.smooth <- to.smooth[frst_n_zero:nrow(df_in), ]
        
        # Mono-smoothing with scam ----
        # b1 <- scam(y ~ s(day, k = basis_dim, bs="mpi",m=2),
        #            family=gaussian(link="identity"), data=to.smooth)
        b1 <- gam(y ~ s(day, k = basis_dim, bs="ps"),
                  data=to.smooth)
        
        # save to column "xxx_smooth":
        df_in$y_smooth <- NA
        df_in[frst_n_zero:nrow(df_in) , "y_smooth"] <- b1$fitted.values
        colnames(df_in)[colnames(df_in) == "y_smooth"] <- paste0(col_s, 
                                                                 "_smooth")
        return(df_in)
}
#---------------------------------------------------------------------------


df <- read.csv(estimates_path, as.is = T)
df$date <- as.Date(df$date, format = "%Y-%m-%d")

df$nsum <- 100 * df$mean_cmnty_cli_ct / reach

df <- smooth_column(df, "pct_cli", smooth_param)
df <- smooth_column(df, "nsum", smooth_param)

# -- Active cases
png(file = output_path)

ymax <- 2.5

plot(df$date, df$pct_cli_smooth, type="l", xlab = "Date", 
     ylab = "% symptomatic cases", main = "Symptomatic cases",
     # xlim=c(xmin, xmax), 
     ylim=c(0, ymax))
# lines(dtwhole$date, dtwhole$p_active*100000,lty=1,col="blue")
# lines(dtwhole$date, dtwhole$p_active_smooth*100000,lty=1,col="magenta")
lines(df$date, df$nsum_smooth,lty=1,col="red")
# lines(dtplot$date, dtplot$fb_umd,lty=1,col="blue")
legend("topright", 
       legend=c("pct_cli", 
                # "Active from cumulative", "Active from smoothed cumulative", "CCFR-based", 
                "NSUM"), 
       col=c("black", "red", "blue", "magenta", "brown"), lty = 1, cex=0.8)

# Save the file.
dev.off()
