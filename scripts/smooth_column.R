## Column monotone-smoother using the package "scam":
##
## df_in: input data frame.
## col_s: string indicating the name of the column to be smoothed.
## basis_dim: integer indicating the basis dimension of the smoother,
##            default value is 15 (arbitrary). Should be smaller 
##            than the number of observations to be smoothed.
          

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
  
  # data to be smoothed:
  to.smooth <- to.smooth[frst_n_zero:nrow(df_in), ]
  
  # Mono-smoothing with scam ----
  b1 <- scam(y ~ s(day, k = basis_dim, bs="mpi",m=2),
             family=gaussian(link="identity"), data=to.smooth)
  
  # save to column "xxx_smooth":
  df_in$y_smooth <- NA
  df_in[frst_n_zero:nrow(df_in) , "y_smooth"] <- b1$fitted.values
  colnames(df_in)[colnames(df_in) == "y_smooth"] <- paste0(col_s, 
                                                              "_smooth")
  return(df_in)
}