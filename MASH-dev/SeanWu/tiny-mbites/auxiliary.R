fequal <- function(a,b,tol=.Machine$double.eps^0.5){
  abs(a-b) <= tol
}

sample <- function(x, ...){
  x[sample.int(length(x), ...)]
}
