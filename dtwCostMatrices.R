# code from https://github.com/maduprey/DynamicTimeWarping

dtwCostMatrices <- function(s, t) {
  # Computes accumulated and local warp cost matrices from two time series
  
  m <- length(s)
  n <- length(t)
  
  # Define a distance/cost function
  d <- function(x, y) abs(x - y) # Euclidean distance
  # d <- function(x, y) (x - y)^2 # Squared Euclidean distance
  
  # Accumulated cost matrix
  acc.cost.m <- matrix(nrow = m, ncol = n)
  
  # Local cost matrix
  local.cost.m <- matrix(nrow = m, ncol = n)
  
  # Set initial boundary distance
  acc.cost.m[2:m, 1] <- Inf
  acc.cost.m[1, 2:n] <- Inf
  acc.cost.m[1, 1] <- 0
  
  # Alternative method of setting boundary distance
  # for(i in 2:m) {
  #   acc.cost.m[i, 1] <- acc.cost.m[i - 1, 1] + d(i, 1)
  # }
  # 
  # for(j in 2:n) {
  #   acc.cost.m[1, j] <- acc.cost.m[1, j - 1] + d(1, j)
  # }
  
  for(i in 2:m) {
    for(j in 2:n) {
      cost <- d(s[i], t[j])
      
      acc.cost.m[i, j] <- cost + min(acc.cost.m[i - 1, j], acc.cost.m[i, j - 1], acc.cost.m[i - 1, j - 1])
      local.cost.m[i, j] <- cost
    }
  }
  
  # Trim initial boundary distance and extract the minimal distance between s- and t-sequences
  dtw <- list("acc.cost.m" = acc.cost.m[2:m, 2:n], 
              "local.cost.m" = local.cost.m[2:m, 2:n], 
              "min.dist" = acc.cost.m[m, n])
  
  return(dtw)
}