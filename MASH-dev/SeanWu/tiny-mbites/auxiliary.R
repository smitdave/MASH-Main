fequal <- function(a,b,tol=.Machine$double.eps^0.5){
  abs(a-b) <= tol
}
