# code from https://github.com/maduprey/DynamicTimeWarping

optimalWarpPath <- function(acc.cost.m) {
  # Compute the optimal warp path over an accumulated cost matrx
  
  i <- nrow(acc.cost.m)
  j <- ncol(acc.cost.m)
  
  # Optimal warping path
  path <- list(c(i, j, acc.cost.m[i, j]))
  
  while(i > 1 & j > 1) {
    if(i == 2) {
      j <- j - 1
    } else if(j == 2) {
      i <- i - 1
    } else {
      if(acc.cost.m[i - 1, j] == min(acc.cost.m[i - 1, j], acc.cost.m[i, j - 1], acc.cost.m[i - 1, j - 1])) {
        i <- i - 1
      } else if(acc.cost.m[i, j - 1] == min(acc.cost.m[i - 1, j], acc.cost.m[i, j - 1], acc.cost.m[i - 1, j - 1])) {
        j <- j - 1
      } else {
        i <- i - 1
        j <- j - 1
      }
    }
    path <- c(path, list(c(i, j, acc.cost.m[i, j])))
  }
  path <- c(path, list(c(1, 1, 1)))
  
  return(path)
}