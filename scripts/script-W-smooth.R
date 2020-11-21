library(dplyr)

# smoothed p_cases and CI:
source("smooth_column-v2.R")

smooth_param <- 35
active_window <- 18 #https://patient.info/news-and-features/coronavirus-how-quickly-do-covid-19-symptoms-develop-and-how-long-do-they-last
path_W <- "../data/estimates-W/"
path_W_dunbar <- "../data/estimates-W-dunbar/"
# estimates_path <- "./estimates-W/"

# #######
# 
# smooth_column_cum <- function(df_in, col_s, basis_dim = 15){
#   
#   ## List of packages
#   packages = c("scam")
#   ## Install&load all
#   package.check <- lapply(
#     packages,
#     FUN = function(x) {
#       if (!require(x, character.only = TRUE)) {
#         install.packages(x, dependencies = TRUE)
#         library(x, character.only = TRUE)
#       }
#     }
#   )
#   
#   # add a number of "day" column:
#   to.smooth <- df_in
#   to.smooth$day <- 1:nrow(to.smooth)
#   
#   # change the name of column to be smoothed:
#   colnames(to.smooth)[colnames(to.smooth) == col_s] = "y"
#   
#   # first non zero element to be smoothed:
#   frst_n_zero <- head(to.smooth[to.smooth$y!=0, "day"], 1)
#   
#   cat("Smoothing starting at row ", frst_n_zero, "..\n")
#   
#   # data to be smoothed:
#   to.smooth <- to.smooth[frst_n_zero:nrow(df_in), ]
#   
#   # Mono-smoothing with scam ----
#   b1 <- scam(y ~ s(day, k = basis_dim, bs="mpi",m=2),
#              family=gaussian(link="identity"), data=to.smooth)
#   
#   # save to column "xxx_smooth":
#   df_in$y_smooth <- NA
#   df_in[frst_n_zero:nrow(df_in) , "y_smooth"] <- b1$fitted.values
#   colnames(df_in)[colnames(df_in) == "y_smooth"] <- paste0(col_s, 
#                                                            "_smooth")
#   return(df_in)
# }
# 
# smooth_column <- function(df_in, col_s, basis_dim = 15){
#   
#   ## List of packages
#   packages = c("scam")
#   ## Install&load all
#   package.check <- lapply(
#     packages,
#     FUN = function(x) {
#       if (!require(x, character.only = TRUE)) {
#         install.packages(x, dependencies = TRUE)
#         library(x, character.only = TRUE)
#       }
#     }
#   )
#   
#   # add a number of "day" column:
#   to.smooth <- df_in
#   to.smooth$day <- 1:nrow(to.smooth)
#   
#   # change the name of column to be smoothed:
#   colnames(to.smooth)[colnames(to.smooth) == col_s] = "y"
#   
#   # first non zero element to be smoothed:
#   frst_n_zero <- head(to.smooth[to.smooth$y!=0, "day"], 1)
#   
#   cat("Smoothing starting at row ", frst_n_zero, "..\n")
#   
#   # data to be smoothed:
#   to.smooth <- to.smooth[frst_n_zero:nrow(df_in), ]
#   
#   # Mono-smoothing with scam ----
#   # b1 <- scam(y ~ s(day, k = basis_dim, bs="mpi",m=2),
#   #            family=gaussian(link="identity"), data=to.smooth)
#     b1 <- gam(y ~ s(day, k = basis_dim, bs="ps"),
#               data=to.smooth)
#   
#   # save to column "xxx_smooth":
#   df_in$y_smooth <- NA
#   df_in[frst_n_zero:nrow(df_in) , "y_smooth"] <- b1$fitted.values
#   colnames(df_in)[colnames(df_in) == "y_smooth"] <- paste0(col_s, 
#                                                            "_smooth")
#   return(df_in)
# }
# 
# 
# # smooth_column <- function(df_in, col_s, basis_dim = 15){
# #   require(mgcv)
# #   
# #   # add a number of "day" column:
# #   to.smooth <- df_in
# #   to.smooth$day <- 1:nrow(to.smooth)
# #   
# #   # change the name of column to be smoothed:
# #   colnames(to.smooth)[colnames(to.smooth) == col_s] = "y"
# #   
# #   # not NA elements to be smoothed:
# #   ind_not_na <- !is.na(to.smooth$y)
# #   
# #   # data to be smoothed:
# #   to.smooth <- to.smooth[ind_not_na, c("y", "day")]
# #   
# #   # Mono-smoothing with scam ----
# #   b1 <- gam(y ~ s(day, k = basis_dim, bs="ps"),
# #             data=to.smooth)
# #   
# #   # save to column "xxx_smooth":
# #   df_in$y_smooth <- NA
# #   # df_in[frst_n_zero:nrow(df_in) , "y_smooth"] <- b1$fitted.values
# #   newd <- data.frame(day = 1:nrow(df_in))
# #   df_in[ , "y_smooth"] <- predict(b1, newd)
# #   colnames(df_in)[colnames(df_in) == "y_smooth"] <- paste0(col_s, 
# #                                                            "_smooth")
# #   return(df_in)
# # }
# 
# #------------------------------------------------------------------------------


do_smoothing <- function(country = "ES",
                         estimates_path){
dt <- read.csv(paste0(estimates_path, "PlotData/", country, "-estimate.csv"), as.is = T)

# --- Smoothing

# dt[["p_cases"]][is.na(dt[["p_cases"]])] <- 0
# if (sum(dt$p_cases != 0) > smooth_param) {
#   dt <- smooth_column_cum(dt, "p_cases", smooth_param)
# }
# dt[["p_cases_recent"]][is.na(dt[["p_cases_recent"]])] <- 0
# if (sum(dt$p_cases_recent != 0) > smooth_param) {
#   dt <- smooth_column(dt, "p_cases_recent", smooth_param)
# }
# dt <- smooth_column_cum(dt, "p_cases", smooth_param)
# dt <- smooth_column(dt, "p_cases_recent", smooth_param)

  dt <- smooth_column(dt, "p_cases", basis_dim = smooth_param, link_in ="log", monotone = T)
  dt <- smooth_column(dt, "p_cases_recent", basis_dim = smooth_param, link_in ="log", monotone = F)
  
# --- Computing the daily differences of cases

dt$p_daily <- c(0 , diff(dt$p_cases))
dt[["p_cases_smooth"]][is.na(dt[["p_cases_smooth"]])] <- 0
dt$p_daily_smooth <- c(0 , diff(dt$p_cases_smooth))

#total active cases
dt$p_active <- cumsum(c(dt$p_daily[1:active_window],
                        diff(dt$p_daily, lag = active_window)))
dt$p_active_smooth <- cumsum(c(dt$p_daily_smooth[1:active_window],
                        diff(dt$p_daily_smooth, lag = active_window)))
# --- Saving new data

dir.create(paste0(estimates_path, "PlotData/"), showWarnings = F)
cat(paste0("::- script-W: Writing the smoothed region based estimate summary for ", country, "..\n"))
write.csv(dt, paste0(estimates_path, "PlotData/", country, "-estimate.csv"),
          row.names = FALSE)
}

cat("Smoothing ES\n")
do_smoothing("ES", path_W)
do_smoothing("ES", path_W_dunbar)
cat("Smoothing BR\n")
do_smoothing("BR", path_W)
do_smoothing("BR", path_W_dunbar)
cat("Smoothing US\n")
do_smoothing("US", path_W)
do_smoothing("US", path_W_dunbar)
do_smoothing("PT", path_W)
do_smoothing("PT", path_W_dunbar)

