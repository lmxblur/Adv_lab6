knapsack_dynamic <- function(x,W){
  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(W))
  value <- x$v
  weight <- x$w
  row_num <-nrow(x)
  m <- matrix(0,nrow=row_num+1,ncol=W+1)
  output = list()
  for (i in c(1:row_num+1)){
    for (j in c(0:W+1)){
      if (weight[i-1]>j){
        m[i,j] <- m[i-1,j]
      }
      else{
        m[i,j] <- max(m[i-1,j],m[i-1,j-weight[i-1]]+value[i-1])
      }
    }
  }
  output['value'] <- m[row_num+1,W+1]
  return_value <- m[row_num+1,W+1]
  small_w <-W
  for (i in c((row_num+1):1)){
    if (return_value<=0){break}
    if (return_value==m[i-1,small_w+1][1]){
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