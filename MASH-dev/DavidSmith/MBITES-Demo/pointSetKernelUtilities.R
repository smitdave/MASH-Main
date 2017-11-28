
#############################################
# Note :: These kernel functions are for 
# computation without MBITES
#############################################

kerW = function(xy, XY, p=1){
  d = sqrt((xy[1] - XY[,1])^2 + (xy[2] - XY[,2])^2) 
  exp(-d*p)*XY[,3]
}

kerW.i = function(i, xy, XY, w, p=1){
  kerW(xy[i,], XY, p)
}

kerDisperseFromPointSets=function(xy.f, xy.l, kerp =4){
  N.f = dim(xy.f)[1]
  N.l = dim(xy.l)[1]
  
  FL = sapply(X=c(1:N.f), FUN=kerW.i, xy = xy.f, XY=xy.l, p=kerp, simplify = "array")
  LF = sapply(X=c(1:N.l), FUN=kerW.i, xy = xy.l, XY=xy.f, p=kerp, simplify = "array")
  
  LF = as.matrix(t(LF))  
  FL = as.matrix(t(FL)) 
  
  dff = as.matrix(dist(xy.f[,c(1,2)], diag = TRUE, upper = TRUE), N.f, N.f) 
  dll = as.matrix(dist(xy.l[,c(1,2)], diag = TRUE, upper = TRUE), N.l, N.l) 
  LL = LF%*%FL
  LL = LL/rowSums(LL)
  FF = FL%*%LF
  FF = FF/rowSums(FF)
  list(FL=FL,LF=LF,LL=LL,FF=FF)
}




#par(mfrow = c(1,1))
#plotByDistance(dll,LL)
#plotByDistance(dff,FF)
#plotSym(xy.l, LL, mag=3, xy.o=xy.f, pset = "n")
#plotSym(xy.f, FF, mag=3, xy.o=xy.l, pset = "n")
#plotFlow(xy.l, LL, mag=3, xy.o=xy.f, pset = "n",al=0.05)
#plotFlow(xy.f, FF, mag=3, xy.o=xy.l, pset = "n", al=0.05)
