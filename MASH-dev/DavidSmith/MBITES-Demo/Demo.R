

getCenters = function(nCenters, rng){
  list(
    x = runif(nCenters, -rng, rng), 
    y = runif(nCenters, -rng, rng)
  ) 
}

makeCluster = function(x,y,n,s){
  X = x + rnorm(n,0,s)
  Y = y + rnorm(n,0,s)
  w = rgamma(n,1,1)
  list(x=X, y=Y, w=w)
}

getClusters = function(seed, nCenters,  rng, nn, spread=NULL){

  set.seed(seed)
  
  xy = getCenters(nCenters, rng)
  
  if(length(nn)==1)
    nn = rep(nn, nCenters)
  
  if(is.null(spread))
    spread = 0.3+rnorm(nCenters,rng/60,rng/60)
  
  clusters = list() 
  for(i in 1:nCenters)
    clusters[[i]] = makeCluster(xy$x[i], xy$y[i], nn[i], spread[i])

  clusters 
}

clusters2xy = function(clusters){
  x=0; y=0; w=0
  for(i in 1:length(clusters)){
    x = c(x, clusters[[i]]$x)
    y = c(y, clusters[[i]]$y)
    w = c(w, clusters[[i]]$w)
  }
  xyw = cbind(x=x[-1], y=y[-1], w=w[-1])
}

plot.fl = function(xy.f, xy.l){
  plot(xy.f[,1], xy.f[,2],  pch = 3, col = "red", xaxt = "n", yaxt = "n", xlab = "", ylab = "", xlim = range(xy.f, xy.l), ylim = range(xy.f, xy.l), cex = xy.f[,3])
  points(xy.l[,1], xy.l[,2], pch =4, col = "blue", cex = xy.l[,3])
}

fclusters = getClusters(21, nCenters=25, rng=10, nn = 10)
lclusters = getClusters(20, nCenters=25, rng=10, nn = 10)

xy.f = clusters2xy(fclusters)
pointsets = list()
pdf("overlap.pdf")
xy.l = clusters2xy(lclusters)
plot.fl(xy.f, xy.l)
pointsets[[1]] = list(xy.f =xy.f, xy.l=xy.l)

newlcluster = c(fclusters[1], lclusters[2:25])
xy.l = clusters2xy(newlcluster)
plot.fl(xy.f, xy.l)
pointsets[[2]] = list(xy.f =xy.f, xy.l=xy.l)

for(i in 2:24){
  newlcluster = c(fclusters[1:i], lclusters[(i+1):25])
  xy.l = clusters2xy(newlcluster)
  plot.fl(xy.f, xy.l)
  pointsets[[i+1]] = list(xy.f =xy.f, xy.l=xy.l)
}

xy.l = clusters2xy(fclusters)
plot.fl(xy.f, xy.l)
pointsets[[26]] = list(xy.f =xy.f, xy.l=xy.l)

dev.off(dev.cur()) 

