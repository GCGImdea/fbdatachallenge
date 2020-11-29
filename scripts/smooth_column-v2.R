## Libraries
library(dplyr)

## Functions ----
smooth_column <- function(df_in, col_s, basis_dim = 15, 
                          link_in = "identity", 
                          monotone = F,
                          compute_CI = T){
  
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
  ## compute_CI: TRUE (default) returns lower and upper confidence bands. 
  
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
  # add a number of "day" column:
  to.smooth <- df_in
  to.smooth$day <- 1:nrow(to.smooth)
  
  # change the name of column to be smoothed:
  colnames(to.smooth)[colnames(to.smooth) == col_s] = "y"
  
  # non-NA elements to be smoothed:
  first_non_NA <- min(which(!is.na(to.smooth$y)))
  
  # remove first NAs:
  to.smooth <- to.smooth[first_non_NA:nrow(to.smooth), ]
  
  # first non zero element to be smoothed:
  frst_n_zero <- min( which( to.smooth$y!=0 ) )
  
  # remove first 0s:
  # REALLY NEEDED ???????
  to.smooth <- to.smooth[frst_n_zero:nrow(to.smooth), ]
  
  # Smoothing with mgcv/scam ----
  # (note the response is perturbed to avoid -Inf when link = "log")
  if (monotone) { # we use S(hape)C(constrained)A(aditive)M(odel)
    
    b1 <- scam(I(y+.Machine$double.eps) ~ s(day, k = basis_dim, bs="mpi",m=2),
               family=gaussian(link = link_in), data=to.smooth)
    
  }else {   # we use G(eneral)A(aditive)M(odel)
    
    b1 <- gam(I(y+.Machine$double.eps) ~ s(day, k = basis_dim, bs="ps"),
              data=to.smooth, family=gaussian(link = link_in) )
  }
  
  # predict during the whole period:
  newd <- data.frame(day = 1:nrow(df_in))
  
  if (compute_CI) {
    pred_smooth <- predict(b1, newd, type = "response", se.fit = T)
    df_predicted <- data.frame(date = df_in$date,
                               y_smooth = pred_smooth$fit,
                               y_smooth_low = pred_smooth$fit - 2*pred_smooth$se.fit,
                               y_smooth_high = pred_smooth$fit + 2*pred_smooth$se.fit)
    
    # introduce new data into input-data frame:
    df_in <- full_join(df_in, df_predicted, by = "date")
    
    # change to column "xxx_smooth":
    colnames(df_in)[colnames(df_in) == "y_smooth"] <- paste0(col_s, "_smooth")
    colnames(df_in)[colnames(df_in) == "y_smooth_low"] <- paste0(col_s, "_smooth_low")
    colnames(df_in)[colnames(df_in) == "y_smooth_high"] <- paste0(col_s, "_smooth_high")
  }else {
    pred_smooth <- predict(b1, newd, type = "response", se.fit = F)
    df_predicted <- data.frame(date = df_in$date,
                               y_smooth = pred_smooth)
    
    # introduce new data into input-data frame:
    df_in <- full_join(df_in, df_predicted, by = "date")
    
    # change to column "xxx_smooth":
    colnames(df_in)[colnames(df_in) == "y_smooth"] <- paste0(col_s, "_smooth")
  }
  
  
  
  return(df_in)
}