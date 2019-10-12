#' Brute_force_knapsack
#'
#' @param x A data frame that represents the wights and the values of each item.
#' @param  W A number that represents the total weight .
#' @param  parallel A logical value that indicate if you want to use parallel .
#' @return A list with the optimum value and the number of each item.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
brute_force_knapsack <- function(x, W, parallel=FALSE){
  
  if (!is.atomic(W) && length(W) != 1L) 
    stop("W should be a scalar number")
  if (W<=0 || W == Inf) 
    stop("W should be a positive number")
  if(!all(is.data.frame(x), 
          ncol(x) == 2,
          "v" %in% names(x),
          "w" %in% names(x)))
    stop('x should be a data.frame with two variables v and w')
  if (!(all(x>0))) 
    stop("x should have only positive values")
  
  bestV <- 0
  n = nrow(x)
  
  # for(i in 0:(2^n-1)){
  #   xi = intToBits(i)
  #   sumv = 0     
  #   sumw = 0     
  #   tempv = c()   
  #   for(j in 1:n){
  #     if(xi[j] == 1){
  #       sumv = sumv + x[j, ]$v
  #       sumw = sumw + x[j, ]$w
  #       tempv = c(tempv, j)
  #     }
  #   }
  #   
  #   if((sumv > bestV) && (sumw <= W)){
  #     bestV = sumv
  #     ev = tempv
  #   }
  # }
  
  if (parallel == FALSE) {
    permut <- sapply(1:(2^n-1), function(i) { intToBits(i)[1:n]})
    weights <- sapply(1:(2^n-1), function(i) { sum(as.numeric(permut[,i])*x$w) })
    values <- sapply(1:(2^n-1), function(i) { sum(as.numeric(permut[,i])*x$v) })
  }
  else
  {
    cluster = parallel::makeCluster(parallel::detectCores() - 1)  # Calculate the number of cores & Initiate cluster
    permut <- parallel::parSapply(cluster, 1:(2^n-1), function(i) {intToBits(i)[1:n]})
    weights <- parallel::parSapply(cluster, 1:(2^n-1), function(i) {sum(as.numeric(permut[,i])*x$w) })
    values <- parallel::parSapply(cluster, 1:(2^n-1), function(i) {sum(as.numeric(permut[,i])*x$v) })
    parallel::stopCluster(cluster)  # close the cluster
  }
  for (i in 1:(2^n-1))  
  {
    if (bestV<values[i] && weights[i] <= W)
    {
      bestV <- values[i]
      elements <- as.numeric(permut[,i])
    }
  }
  return(list(value=round(bestV,0), elements=which(as.logical(elements))))
  
}
  
