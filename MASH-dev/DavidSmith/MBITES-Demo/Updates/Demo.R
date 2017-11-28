


runifCenters = function(nCenters, rng){
  list(
    x = runif(nCenters, -rng, rng), 
    y = runif(nCenters, -rng, rng)
  ) 
}

rnormClusters = function(x,y,n,s,wt.type="gamma",wt.par=list(x=1)){
  X = x + rnorm(n,0,s)
  Y = y + rnorm(n,0,s)
  w = getSearchWeights(n,wt.type,wt.par) 
  list(x=X,y=Y,w=w)
}

getClusters = function(seed, nCenters,  rng, nn, spread=NULL){

  set.seed(seed)
  
  xy = runifCenters(nCenters, rng)
  
  if(length(nn)==1)
    nn = rep(nn, nCenters)
  
  if(is.null(spread))
    spread = 0.3+rnorm(nCenters,rng/60,rng/60)
  
  clusters = list() 
  for(i in 1:nCenters)
    clusters[[i]] = rnormClusters(xy$x[i], xy$y[i], nn[i], spread[i], 0)

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

plot.fl = function(xy.f, xy.l, pcex=1){
  plot(xy.f[,1], xy.f[,2],  pch = 3, col = "red", xaxt = "n", yaxt = "n", xlab = "", ylab = "", xlim = range(xy.f[,1], xy.l[,1]), ylim = range(xy.f[,2], xy.l[,2]), cex = pcex*xy.f[,3])
  points(xy.l[,1], xy.l[,2], pch = 4, col = "blue", cex = pcex*xy.l[,3])
}

#move = kerDisperseFromPointSets=function(xy.f, xy.l, kerp =4){

