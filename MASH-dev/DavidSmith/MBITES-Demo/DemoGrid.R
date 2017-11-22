############################################
# Make a set of clusters on a grid
############################################
source ("Demo.R")

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

#plot(x,y)

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

par(mfrow = c(2,2), mar = c(0,0,0,0))
plot(x[-1],y[-1], xlab= "", xaxt = "n", ylab= "", yaxt = "n")

source ("Demo.network.R")
source("pointSetDispersion.R")
