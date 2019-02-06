tridiag = function(upper, lower, main) {
  out = matrix(0,length(main),length(main))
  diag(out) = main
  indx = seq.int(length(upper))
  out[cbind(indx+1,indx)] = lower
  out[cbind(indx,indx+1)] = upper
  return(out)
}

ageMatrix = function(size){
  u = rep(0,size-1)
  l = rep(1,size-1)
  m = rep(0,size)
  m[size] = 1
  tridiag(u,l,m)
}
