#' Γreedy_knapsack
#'
#' @param x A data frame that represents the wights and the values of each item.
#' @param  W A number that represents the total weight .
#' @return A list with the optimum value and the number of each item.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
greedy_knapsack <- function(x, W){
  if (!is.atomic(W) && length(W) != 1) 
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
  
  
  n <- nrow(x)
  x$arg <- 1:n
  x$vw <- x$v/x$w
  x <- x[order(-x$vw),]  #DESCENDING order
  el <- rep(0, n)
  totalW <- 0
  bestV <- 0
  
  for (i in 1:n){
    if (totalW+x$w[i] <= W){
      totalW <- totalW + x$w[i]
      bestV <- bestV + x$v[i]
      el[x$arg[i]] <- 1
    }
    else{ 
      break}
  }
  return(list(value=round(bestV), elements=which(as.logical(el))))
}