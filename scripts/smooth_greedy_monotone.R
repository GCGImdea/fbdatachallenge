# simple greedy smoothing inspired by Carlos
# with the assumption that all deficit d will be paid
# before the series terminates

smooth_greedy <- function(dt_column){
  l <- length(dt_column)
  dt_column[is.na(dt_column)] <- 0
  for (i in 2:l){
    if (dt_column[i] < dt_column[i-1])  { # track ecc instead
      d <- dt_column[i-1] - dt_column[i] 
      dt_column[i] <- dt_column[i-1]
      dt_column[i+1] <- dt_column[i+1] - d
    }
  }
  return(dt_column[1:l])
}
