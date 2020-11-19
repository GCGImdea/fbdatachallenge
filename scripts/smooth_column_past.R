## Libraries
library(dplyr)

source("smooth_column-v2.R")

## Functions ----
smooth_column_past <- function(df_in, col_s, basis_dim = 15, link_in = "identity", monotone = F){
  
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
  
  to.smooth <- df_in
  colnames(to.smooth)[colnames(to.smooth) == col_s] = "y"
  
  smoothed <- data.frame(date=df_in$date)
  smoothed$y_smooth <- smoothed$y
  smoothed$y_smooth_low <- smoothed$y
  smoothed$y_smooth_high <- smoothed$y
  
  for (i in basis_dim:nrow(to.smooth)){
    aux <- to.smooth[1:i,]
    smooth_column(aux, "y", basis_dim, link_in, monotone)
    smoothed$y_smooth[i] <- aux$y_smooth[i]
    smoothed$y_smooth_low[i] <- aux$y_smooth_low[i]
    smoothed$y_smooth_high[i] <- aux$y_smooth_high[i]
  }
  
  # introduce new data into input-data frame:
  df_in <- full_join(df_in, smoothed, by = "date")
  
  # change to column "xxx_smooth":
  colnames(df_in)[colnames(df_in) == "y_smooth"] <- paste0(col_s, "_smooth")
  colnames(df_in)[colnames(df_in) == "y_smooth_low"] <- paste0(col_s, "_smooth_low")
  colnames(df_in)[colnames(df_in) == "y_smooth_high"] <- paste0(col_s, "_smooth_high")
  
  return(df_in)
}

kk <- data.frame(date=1:200, value = sample(1:100, 200, replace=TRUE))
kk <- smooth_column_past(kk, "value")
plot(kk$value_smooth)
