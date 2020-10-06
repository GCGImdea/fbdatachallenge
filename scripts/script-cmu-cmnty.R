library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)

reach <- 150
smooth_param <- 15

estimates_path <- "../data/cmu-state/cmu-state-ma.csv"
output_path2 <- "../data/cmu-state/cmu-state-ma2.png"
output_path1 <- "../data/cmu-state/cmu-state-ma1.png"

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


dtcdc <- read.csv("../data/US_cdc_active_cases/us_cdc_active_cases_ma.csv", as.is = T)[,-1]
dtcdc$date <- as.Date(dtcdc$date, format = "%Y-%m-%d")

df <- left_join(df, dtcdc, by = "date")


reach_cutoff <- boxplot.stats(df$mean_cmnty_cli_ct)$stats[5] # changed cutoff to upper fence
df[df$mean_cmnty_cli_ct > reach_cutoff, "mean_cmnty_cli_ct"] <- NA

df$nsum <- 100 * df$mean_cmnty_cli_ct / reach

df <- smooth_column(df, "pct_cli", smooth_param)
df$nsum_roll <- zoo::rollmean(df$nsum, 3, fill=NA)
df <- smooth_column(df, "nsum_roll", 15)
df$pct_cases_active_cdc <- df$cases_active_cdc/df$population*100
df <- smooth_column(df, "pct_cases_active_cdc", smooth_param)


# Harold's ----
ymax <- 4

# my_colors <- c("yellow", "yellow", "blue", "blue", "red", "red", "red")
my_colors <- c("#ffd166", "#ffd166", "#118ab2", "#118ab2", "#ef476f", "#ef476f", "#ef476f")

p <- ggplot(data = df, aes(x = date)) +
        geom_point(aes(y = pct_cases_active_cdc, color = "Confirmed"), alpha = 0.4) +
        geom_line(aes(y = pct_cases_active_cdc_smooth, color = "Confirmed (smooth)"), size = 0.8) +
        geom_point(aes(y = pct_cli, color = "CSDC CLI"), alpha = 0.4) +
        geom_line(aes(y = pct_cli_smooth, color = "CSDC CLI (smooth)"), size = 0.8) +
        geom_point(aes(y = nsum, color = "NSUM"), alpha = 0.4) +
        geom_line(aes(y = nsum_roll, color = "NSUM (rolling mean)"), linetype = "dashed", alpha = 0.3, size = 0.8) +
        geom_line(aes(y = nsum_roll_smooth, color = "NSUM (smooth)"), alpha = 0.6, size = 0.8) +
        theme_bw() + 
        labs(title = "Symptomatic cases in Massachussets", x = "Date",
             y = "% symptomatic cases") +
        scale_colour_manual(name= "", values = my_colors,
                            guide = guide_legend(override.aes = list(
                                    linetype = c("blank", "solid", "blank", "solid", "blank", "dashed", "solid"),
                                    shape = c(1, NA, 1, NA, 1, NA, NA)))) + 
        ylim(-0.1, ymax) +
        theme(legend.position="bottom")
p

ggsave(plot = p, 
       filename = output_path1, 
       width = 8, height = 6)


# Segun's: ----
pal <- c("pct_cli" = "blue",
         "pct_cli_smooth" = "blue",
         "nsum" = "red",
         "nsum_roll" = "red",
         "nsum_roll_smooth" = "red",
         "pct_active_cases_jhu" = "green",
         "pct_active_cases_jhu_smooth" = "green")

dt_point <- df[,c("date", "pct_cli", "nsum", "pct_cases_active_cdc")] %>% 
        gather(key = "type", value = "pct", -1)

dt_line <-  df[,c("date", "pct_cli_smooth", "nsum_roll_smooth", "pct_cases_active_cdc_smooth")] %>% 
        rename(pct_cli = pct_cli_smooth,
               nsum = nsum_roll_smooth,
               pct_cases_active_cdc = pct_cases_active_cdc_smooth) %>% 
        gather(key = "type", value = "pct", -1)

pq <- ggplot(data = dt_point, aes(x = date, y = pct, colour = type))+
        geom_point() +
        geom_line(data = dt_line, aes(x = date, y = pct, colour = type))+
        theme_bw()+
        theme(legend.position = "bottom")+
        scale_color_manual(values = c("#ef476f", "#ffd166", "#118ab2", "#06d6a0"))

ggsave(plot = pq,
       filename =  output_path2,
       width = 8, height = 6)


# Antonio's ----
# # -- Active cases
# png(file = output_path)
# 
# ymax <- 4
# 
# plot(df$date, df$pct_cli_smooth, type="l", xlab = "Date", 
#      ylab = "% symptomatic cases", main = "Symptomatic cases in Massachussets",
#      # xlim=c(xmin, xmax), 
#      ylim=c(0, ymax)
#      )
# # lines(dtwhole$date, dtwhole$p_active*100000,lty=1,col="blue")
# # lines(dtwhole$date, dtwhole$p_active_smooth*100000,lty=1,col="magenta")
# lines(df$date, df$nsum_roll_smooth,lty=1,col="red")
# # lines(dtplot$date, dtplot$fb_umd,lty=1,col="blue")
# legend("topright", 
#        legend=c("pct_cli", 
#                 # "Active from cumulative", "Active from smoothed cumulative", "CCFR-based", 
#                 "NSUM"), 
#        col=c("black", "red", "blue", "magenta", "brown"), lty = 1, cex=0.8)
# 
# # Save the file.
# dev.off()
