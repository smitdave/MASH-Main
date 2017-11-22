# Make a set of clusters on the grid
######################
# Set Grid Parameters
######################
g = -2:2
lg = length(g)
np = 4
sp = max(diff(g))/10

x = matrix(rep(g, lg), lg, lg)
y = t(x) 
xy = cbind(as.vector(x),as.vector(y)) 

plot(x,y)

x=0; y=0; w=0
for(i in 1:dim(xy)[1]){
  xyw = makeCluster(xy[i,1], xy[i,2], np, sp)
  x = c(x,xyw$x)
  y = c(y,xyw$y)
  w = c(w,xyw$w)
}

xy.f = cbind(x=x[-1], y=y[-1], w=w[-1]) 
xy.l = xy.f 
N.f = dim(xy.f)[1]
N.l = dim(xy.f)[1]

par(mfrow = c(2,2), mar = c(0,0,0,0))
plot(x[-1],y[-1], xlab= "", xaxt = "n", ylab= "", yaxt = "n")

