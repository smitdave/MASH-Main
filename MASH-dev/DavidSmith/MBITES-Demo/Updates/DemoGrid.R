############################################
# Make a set of clusters on a grid
############################################
source ("Demo.R")
source ("Demo.network.R")
source ("pointSetDispersion.R")
##########################
# xy is a grid over -2:2
##########################
xy = gGrid(-2:2) 

np.f = 4
sp.f = max(diff(g))/10
np.l = 3
sp.l = sp.f


x=0; y=0; w=0
for(i in 1:dim(xy)[1]){
  xyw = makeCluster(xy[i,1], xy[i,2], np.f, sp.f)
  x = c(x,xyw$x)
  y = c(y,xyw$y)
  w = c(w,xyw$w)
}

xy.f = cbind(x=x[-1], y=y[-1], w=w[-1]) 

x=0; y=0; w=0
for(i in 1:dim(xy)[1]){
  xyw = makeCluster(xy[i,1], xy[i,2], np.l, sp)
  x = c(x,xyw$x)
  y = c(y,xyw$y)
  w = c(w,xyw$w)
}

xy.l = cbind(x=x[-1], y=y[-1], w=w[-1]) 

MM = kerDisperseFromPointSets(xy.f, xy.l, kerp=3)
#pdf("grid.pdf", height=10, width=10)

visPointSetDispersal(xy.f, xy.l, MM$FF, MM$LL, cutt=0)


