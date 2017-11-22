
pointSetDiagnostic = function(xy.f, xy.l){
  N.f = dim(xy.f)[1]
  N.l = dim(xy.l)[1]
  
  FL = sapply(X=c(1:N.f), FUN=kerW.i, xy = xy.f, XY=xy.l, p=4, simplify = "array")
  LF = sapply(X=c(1:N.l), FUN=kerW.i, xy = xy.l, XY=xy.f, p=4, simplify = "array")
  
  LF = as.matrix(t(LF))  
  FL = as.matrix(t(FL)) 
  
  dff = as.matrix(dist(xy.f[,c(1,2)], diag = TRUE, upper = TRUE), N.f, N.f) 
  dll = as.matrix(dist(xy.l[,c(1,2)], diag = TRUE, upper = TRUE), N.l, N.l) 
  LL = LF%*%FL
  LL = LL/rowSums(LL)
  FF = FL%*%LF
  FF = FF/rowSums(FF)
  
  
  par(mfrow = c(2,2), mar = c(0,0,0,0))
  plot(xy.l[,1],xy.l[,2], pch = 4, col = "blue", xlab= "", xaxt = "n", ylab= "", yaxt = "n")
  points(xy.f[,1],xy.f[,2], pch = 3, col = "red")
  plotSym(xy.l, LL, mag=3, xy.o=xy.f, pset = "l", pcex = .2)
  addAdjacency(xy.f, FF, mag=3, ccol = "red")
  
  plotFlow(xy.l, LL, mag=0.5, xy.o=xy.f, pset = "l",al=0.05, pcex=0.2)
  addFlow(xy.f, FF, mag=0.5, ccol = "red")
  plotPointSetDispersion(dll, LL, 500)
}