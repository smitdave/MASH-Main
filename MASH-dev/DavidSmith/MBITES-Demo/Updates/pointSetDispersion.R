
pointSetDispersion = function(dxx, fw){
  dd = as.vector(dxx)
  ot = order(dd)
  dd = dd[ot]
  fw = as.vector(fw)[ot]
  list(d=dd,w=fw)
}

smoothPointSetCDF = function(d,cdf,n=100){
  ix = round(seq(1, length(d), length.out = n))
  dd = d[ix]
  list(dd=d[ix], ccdf=cdf[ix])
}

smoothPointSetDispersal = function(d,w,n=300){
  sm = function(i){
    ix = which((d-dd[i])^2 < 2*d/n)
    weighted.mean(w[ix]*exp(-k*(d[ix]-dd[i])^2), exp(-k*(d[ix]-dd[i])^2))
  }
  df = max(d)-min(d) 
  k = 5*df/n
  dd = seq(min(d), max(d), length.out = n)
  list(dd=dd, sm=sapply(1:length(dd), sm)) 
}

plotPointSetDispersion = function(xy, wts, n=200){
  aa = pointSetDispersion(xy, wts)
  with(aa,{
    cdf = cumsum(w)/sum(w)
    plot(d, cdf, type = "l", xlab = "Distance", ylab = "Frequency")
    points(d, w/max(w), type = "h", col = grey(0.5))
    lines(d, cdf)
    
    with(smoothPointSetCDF(d,cdf,n),{
      lines(dd, ccdf, col = "red")
      dcdf = diff(ccdf)
      lines(dd[-1], dcdf/max(dcdf), col = "red")
    })
    return(list(d=d, w=w, cdf=cdf)) 
  })
}


#############################################
# Plot 
#############################################
plotByDistance = function(dd,QQ){
  N = dim(dd)[1]
  par(mfrow = c(2,1), mar = c(5,4,2,1))
  ot = order(dd[1,])
  plot(dd[1,ot], QQ[1,ot], type = "l", ylim = c(0,1))
  for(i in 1:N){
    ot = order(dd[i,])
    lines(dd[i,ot], QQ[i,ot]) 
  }
  ot = order(dd[1,])
  plot(dd[1,ot], cumsum(QQ[1,ot]), type = "l", ylim =c(0,1))
  for(i in 1:N){
    ot = order(dd[i,])
    lines(dd[i,ot], cumsum(QQ[i,ot])) 
  } 
}


plotSym = function(xy, Q, mag=1, cutt=0.95, xy.o=NULL, pset = "l", pcex = 1){
  
  ppch = 3
  ccol = "blue"
  ppch.o = 4
  ccol.o = "red"
  
  if(pset == "f") {
    ppch = 4
    ccol = "red"
    ppch.o = 3
    ccol.o = "blue"
  }
  
  rrng = range(xy[,c(1,2)], xy.o[,c(1,2)])
  
  plot(xy[,1], xy[,2], pch = ppch, col = ccol,  
       xlab = "", xaxt = "n", ylab = "", yaxt = "n", 
       xlim = rrng, ylim = rrng, cex = pcex)
  points(xy.o[,1], xy.o[,2], pch = ppch.o, col = ccol.o, cex = pcex)
  addAdjacency(xy, Q, mag=mag, cutt=cutt, ccol=ccol)
}

addAdjacency = function(xy, Q, mag=1, cutt = 0.95, ccol="black"){
  N = dim(Q)[1]
  S = pmin(Q, t(Q))
  qc = quantile(S, cutt)
  for(i in 1:N) for(j in i:N) if(S[i,j] > qc)
    segments(xy[i,1], xy[i,2], xy[j,1], xy[j,2], lwd = S[i,j]*mag, col = ccol)  
}

plotFlow = function(xy, Q, al=0.05, cutt=0.95, mag=1, xy.o=NULL, pset = "l", pcex = 1){
  if (pset == "l"){
    ppch = 3
    ccol = "blue"
    ppch.o = 4
    ccol.o = "red"
  } 
  if(pset == "f") {
    ppch = 4
    ccol = "red"
    ppch.o = 3
    ccol.o = "blue"
  }
  rrng = range(xy[,c(1,2)], xy.o[,c(1,2)])
  
  plot(xy[,1], xy[,2], pch = ppch, col = ccol,  
       xlab = "", xaxt = "n", ylab = "", yaxt = "n", 
       xlim = rrng, ylim = rrng, cex=pcex)
  points(xy.o[,1], xy.o[,2], pch = ppch.o, col = ccol.o, cex=pcex)
  
  addFlow(xy,Q,al,cutt,mag,ccol) 
}

addFlow = function(xy, Q, al=0.05, cutt=0.95, mag=1, ccol = "black"){
  N = dim(Q)[1]
  S = pmin(Q, t(Q))
  F = Q-S 
  qc = quantile(F, cutt)
  for(i in 1:N) for(j in 1:N) if(F[i,j]>qc)
    arrows(xy[i,1], xy[i,2], xy[j,1], xy[j,2], 
           lwd = F[i,j]*mag, length = al, col = ccol)
}

visPointSetDispersal = function(xy.f, xy.l, FF, LL, Smag=.01, cutt=0.95, kerp = 4, Fmag=0.01, al=0.05, pcex=0.2){
  par(mfrow = c(2,2), mar = c(0,0,0,0))
  plot(xy.l[,1],xy.l[,2], pch = 4, col = "blue", xlab= "", xaxt = "n", ylab= "", yaxt = "n")
  points(xy.f[,1],xy.f[,2], pch = 3, col = "red")
  plotSym(xy.l, LL, mag=Smag, cutt=cutt, xy.o=xy.f, pset = "l", pcex=pcex)
  addAdjacency(xy.f, FF, mag=Smag, cutt=cutt, ccol = "red")
  
  plotFlow(xy.l, LL, mag=Fmag, cutt=cutt, xy.o=xy.f, pset = "l", al=al, pcex=pcex)
  addFlow(xy.f, FF, mag=Fmag, cutt=cutt, ccol = "red")
  plotPointSetDispersion(dll, LL, 500)
  
}


#ans = plotPointSetDispersion(dll, Q,500)
#ker = smoothPointSetDispersal(ans$d, ans$w)