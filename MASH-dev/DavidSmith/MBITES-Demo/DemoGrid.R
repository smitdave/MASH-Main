############################################
# Make a set of clusters on a grid
############################################
source ("Demo.R")
source ("Demo.network.R")
source ("pointSetDispersion.R")

######################
# Set Grid Parameters
######################
g = -2:2
lg = length(g)
np = 4
sp = max(diff(g))/10

######################
# xy is a grid over g 
######################
x = matrix(rep(g, lg), lg, lg)
y = t(x) 
xy = cbind(as.vector(x),as.vector(y)) 

#3plot(x,y)

x=0; y=0; w=0
for(i in 1:dim(xy)[1]){
  xyw = makeCluster(xy[i,1], xy[i,2], np, sp)
  x = c(x,xyw$x)
  y = c(y,xyw$y)
  w = c(w,xyw$w)
}

xy.f = cbind(x=x[-1], y=y[-1], w=w[-1]) 

x=0; y=0; w=0
for(i in 1:dim(xy)[1]){
  xyw = makeCluster(xy[i,1], xy[i,2], np-1, sp)
  x = c(x,xyw$x)
  y = c(y,xyw$y)
  w = c(w,xyw$w)
}

xy.l = cbind(x=x[-1], y=y[-1], w=w[-1]) 

N.f = dim(xy.f)[1]
N.l = dim(xy.l)[1]

FL = sapply(X=c(1:N.f), FUN=kerW.i, xy = xy.f, XY=xy.l, p=5, simplify = "array")
LF = sapply(X=c(1:N.l), FUN=kerW.i, xy = xy.l, XY=xy.f, p=5, simplify = "array")

LF = as.matrix(t(LF))  
FL = as.matrix(t(FL)) 

dff = as.matrix(dist(xy.f[,c(1,2)], diag = TRUE, upper = TRUE), N.f, N.f) 
dll = as.matrix(dist(xy.l[,c(1,2)], diag = TRUE, upper = TRUE), N.l, N.l) 
LL = LF%*%FL
LL = LL/rowSums(LL)
FF = FL%*%LF
FF = FF/rowSums(FF)


par(mfrow = c(2,2), mar = c(0,0,0,0))
plot(x[-1],y[-1], xlab= "", xaxt = "n", ylab= "", yaxt = "n")
plotSym(xy.l, LL, mag=3, xy.o=xy.f, pset = "n")
plotFlow(xy.l, LL, mag=3, xy.o=xy.f, pset = "n",al=0.05)
ans = plotPointSetDispersion(dll, LL, 100)

