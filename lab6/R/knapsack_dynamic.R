#' Dynamic programming of knapsack problem
#'
#' @param x A dataframe that contains weights and values.
#' @param b A numer representing the limited weight.
#' @description A programme that solve the knapsack problem using dynamic programming.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
knapsack_dynamic <- function(x,W){
  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(W))
  value <- x$v
  weight <- x$w
  row_num <-nrow(x)
  m <- matrix(0,nrow=row_num,ncol=W)
  output = list()
  for (i in c(2:row_num)){
    for (j in c(1:W)){
      if (weight[i]>j){
        m[i,j] <- m[i-1,j]
      }
      else{
        m[i,j] <- max(m[i-1,j],m[i-1,j-weight[i]]+value[i])
      }
    }
  }
  output['value'] <- m[row_num,W]
  return_value <- m[row_num,W]
  small_w <-W 
  for (i in c(row_num:1)){
    if (return_value<=0){break}
    if (return_value==m[i-1,small_w][1]){
      next
    }
    else{
      output$elements <- append(output$elements,weight[i-1])
      return_value <- return_value - value[i-1]
      small_w <- small_w - weight[i-1]
    }
  }
  print(output)
} 