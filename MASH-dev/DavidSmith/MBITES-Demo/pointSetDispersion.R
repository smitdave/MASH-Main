
pointSetDispersion = function(xy, wts){
  DD = dist(xy[,c(1,2)], diag = TRUE, upper = TRUE)
  dd = as.vector(DD)
  ot = order(dd)
  dd = dd[ot]
  wts = as.vector(wts)[ot]
  list(d=dd,w=wts) 
}

smoothPointSetCDF = function(d,cdf,n=100){
  ix = round(seq(1, length(d), length.out = n))
  dd = d[ix]
  list(dd=d[ix], ccdf=cdf[ix])
}

smoothPointSetDispersal = function(d,w,n=100){
  sm = function(i){
    ix = which((d-dd[i])^2 < 2*d/n)
    weighted.mean(w[ix]*exp(-k*(d[ix]-dd[i])^2), exp(-k*(d[ix]-dd[i])^2))
  }
  df = max(d)-min(d) 
  k = 5*df/n
  dd = seq(min(d), max(d), length.out = n)
  list(dd=dd, sm=sapply(1:length(dd), sm)) 
}

plotPointSetDispersion = function(xy, wts){
  aa = pointSetDispersion(xy, wts)
  with(aa,{
    cdf = cumsum(w)/sum(w)
    plot(d, cdf, type = "l", xlab = "Distance", ylab = "Frequency")
    points(d, w/max(w), type = "h", col = grey(0.5))
    lines(d, cdf)
    
    with(smoothPointSetCDF(d,cdf),{
      lines(dd, ccdf, col = "red")
      dcdf = diff(ccdf)
      lines(dd[-1], dcdf/max(dcdf), col = "red")
    })
    return(list(d=d, w=w, cdf=cdf)) 
  })
}

ans = plotPointSetDispersion(xy.f, rpois(N.l^2, 200*Q))
#ker = smoothPointSetDispersal(ans$d, ans$w)