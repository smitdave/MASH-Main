
kerW = function(xy, XY, p=1){
  d = sqrt((xy[1] - XY[,1])^2 + (xy[2] - XY[,2])^2) 
  exp(-d*p)*XY[,3]
}

#xy = c(0,0)
#XY = cbind(1:20, 1:20, rep(1,20))
#plot(1:20, kerW(xy, XY))

kerW.i = function(i, xy, XY, w, p=1){
  kerW(xy[i,], XY, p)
}




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


plotSym = function(xy, Q, mag=1, xy.o=NULL, pset = "l"){
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
  if(pset == "n") {
    ppch = ""
    ccol = "white"
    ppch.o = ""
    ccol.o = "white"
  }
  rrng = range(xy, xy.o)
  
  plot(xy[,1], xy[,2], pch = ppch, col = ccol,  
       xlab = "", xaxt = "n", ylab = "", yaxt = "n", 
       xlim = rrng, ylim = rrng)
  points(xy.o[,1], xy.o[,2], pch = ppch.o, col = ccol.o)
  
  N = dim(Q)
  S = pmin(Q, t(Q))
  for(i in 1:N)
    for(j in i:N)
      segments(xy[i,1], xy[i,2], xy[j,1], xy[j,2], lwd = S[i,j]*mag)
}

plotFlow = function(xy, Q, al=0.05, mag=1, xy.o=NULL, pset = "l"){
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
  if(pset == "n") {
    ppch = 4
    ccol = "white"
    ppch.o = 3
    ccol.o = "white"
  }
  rrng = range(xy, xy.o)
  
  plot(xy[,1], xy[,2], pch = ppch, col = ccol,  
       xlab = "", xaxt = "n", ylab = "", yaxt = "n", 
       xlim = rrng, ylim = rrng)
  points(xy.o[,1], xy.o[,2], pch = ppch.o, col = ccol.o)
  
  N = dim(Q)
  S = pmin(Q, t(Q))
  F = Q-S 
  for(i in 1:N) for(j in 1:N) if(F[i,j]>0)
    arrows(xy[i,1], xy[i,2], xy[j,1], xy[j,2], 
           lwd = F[i,j]*mag, length = al)
}

#par(mfrow = c(1,1))
#plotByDistance(dll,LL)
#plotByDistance(dff,FF)
#plotSym(xy.l, LL, mag=3, xy.o=xy.f, pset = "n")
#plotSym(xy.f, FF, mag=3, xy.o=xy.l, pset = "n")
#plotFlow(xy.l, LL, mag=3, xy.o=xy.f, pset = "n",al=0.05)
#plotFlow(xy.f, FF, mag=3, xy.o=xy.l, pset = "n", al=0.05)
