## Libraries
library(dplyr)

source("smooth_column-v2.R")

## Functions ----
smooth_column_past <- function(df_in, 
                               col_s, 
                               basis_dim = 15, 
                               link_in = "identity", 
                               monotone = F,
                               conf_interval = F
                               ){

  min_vals <- basis_dim
  
  ## Column smoother using the packages "mgcv/scam":
  ##
  ## df_in: input data frame.
  ## col_s: string indicating the name of the column to be smoothed.
  ## basis_dim: integer indicating the basis dimension of the smoother,
  ##            default value is 15 (arbitrary). Should be smaller 
  ##            than the number of observations to be smoothed.
  ## link_in: model link function for Gaussian family. Possible values: 
  ##          "identity" (default), "log" and "inverse".  
  ## monotone: FALSE (default) uses a non-monotonic monotonic smoothing. Option
  ##           TRUE uses a monotone increasing smoother.
  
  ## List of packages
  packages = c("scam", "mgcv", "dplyr")
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
  
  to_smooth <- df_in
  colnames(to_smooth)[colnames(to_smooth) == col_s] = "y"
  to_smooth <- to_smooth %>% select(date, y)
 
  smoothed <- data.frame(date=to_smooth$date,
                         y_smooth=to_smooth$y)
  
  if (conf_interval){
    smoothed$y_smooth_low <- to_smooth$y
    smoothed$y_smooth_high <- to_smooth$y
  }

  # non-NA elements to be smoothed:
  first_non_NA <- min(which(!is.na(to_smooth$y)))
  # first non zero element to be smoothed:
  frst_n_zero <- min( which( to_smooth$y!=0 ) )
  
  min_vals <- max(first_non_NA,frst_n_zero)+min_vals
  
  for (i in min_vals:nrow(to_smooth)){

    aux <- to_smooth %>% slice(1:i)
    aux <- smooth_column(aux, "y", min(i,basis_dim), link_in, monotone)
    smoothed$y_smooth[i] <- aux$y_smooth[i]
    if (conf_interval){
      smoothed$y_smooth_low[i] <- aux$y_smooth_low[i]
      smoothed$y_smooth_high[i]  <- aux$y_smooth_high[i]
    }
    # # Change data to smoothed value to future use
    # to_smooth$y[i] <- aux$y_smooth[i]
  }
  
  # introduce new data into input-data frame:
  df_in <- full_join(df_in, smoothed, by = "date")
  
  # change to column "xxx_smooth":
  colnames(df_in)[colnames(df_in) == "y_smooth"] <- paste0(col_s, "_past_smooth")
  if (conf_interval) {
    colnames(df_in)[colnames(df_in) == "y_smooth_low"] <- paste0(col_s, "_past_smooth_low")
    colnames(df_in)[colnames(df_in) == "y_smooth_high"] <- paste0(col_s, "_past_smooth_high")
  }
  
  return(df_in)
}

# kk <- data.frame(date=1:200, value = sample(1:10, 200, replace=TRUE))
# kk <- smooth_column_past(kk, "value", basis_dim = 15, link_in = "log", monotone = F)
# plot(kk$date, kk$value_past_smooth)
