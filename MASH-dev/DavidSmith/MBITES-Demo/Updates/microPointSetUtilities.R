#######################################################
#  Functions for generating and testing point sets
#######################################################

##########################################
# Use gGrid to make a grid over a mesh g 
# ----------------------------------------
# Example: 
# xy = gGrid(-2:2)
# plot(xy)
# ----------------------------------------
gGrid = function(g){
  lg = length(g)
  x = matrix(rep(g, lg), lg, lg)
  y = t(x) 
  cbind(as.vector(x),as.vector(y)) 
}

#############################################
# getSearchWeights is a function to generate 
# search weights for constructing landscsapes  
# -------------------------------------------
# Examples:
# a = getSearchWeights(10)
# a = getSearchWeights(1000, "gamma", list(x=2))
# hist(a)
# -------------------------------------------
getSearchWeights = function(N, wt.type="homogenous", wt.par=NULL){
  switch(wt.type,
         "gamma" = with(wt.par,rgamma(N,x,x)),
         "homogenous" = rep(1,N)
  )
}

#############################################
# generate n random centers
# -------------------------------------------
# Examples:
# plot(rCenters(10,10)) 
# -------------------------------------------
rCenters = function(n, rng){
  cbind(
    x = runif(n, -rng, rng), 
    y = runif(n, -rng, rng)
  ) 
}

#############################################
# generate a cluster of n points 
# around the point x,y 
# using rnorm, with variance s
# -------------------------------------------
# Examples:
# plot(rnormCluster(0,0,10,1)) 
# -------------------------------------------
rnormCluster = function(x,y,n,s){
  cbind(x=rnorm(n,x,s),y=rnorm(n,y,s))
}

#############################################
# Generate several clusters of points, returned
# in a list
#############################################
makeClusters = function(
  seed,            # Random number seed
  nClusters,       # The number of clusters
  rng,             # The cluster centers will be in (-rng, rng)
  nn,              # The number of points per cluster
  spr=60,          # See note below
  wt.type="gamma",
  wt.par=list(x=1)
){
  
  set.seed(seed)
  
  centers = rCenters(nClusters, rng)

  # If nn is set to a scalar, then each cluster has 
  # nn points. Otherwises, nn should have the same 
  # length as nClusters
  if(length(nn)==1)
    nn = rep(nn, nClusters)
  
  # If spr is set to a scalar, then the following rule
  # determines the spread of points around centers.
  # Otherwises, spr should have the same length as nClusters
  if(length(spr)==1)
    ss = rep(rng/spr, nClusters) 

  clusters = list()
  for(i in 1:nClusters)
    clusters[[i]]= cbind(
         rnormCluster(centers[i,1], centers[i,2], nn[i], ss[i]), 
         w=getSearchWeights(nn[i],wt.type,wt.par))

  clusters 
}

#############################################
# turn a list of clusters into a micro point set 
# -------------------------------------------
# Examples:
# plot(rnormCluster) 
# -------------------------------------------
clusters2xy = function(clusters){
  x=0; y=0; w=0
  for(i in 1:length(clusters)){
    x = c(x, clusters[[i]][,1])
    y = c(y, clusters[[i]][,2])
    w = c(w, clusters[[i]][,3])
  }
  xyw = cbind(x=x[-1], y=y[-1], w=w[-1])
}

plot.fl = function(xy.f, xy.l, pcex=1){
  plot(xy.f[,1], xy.f[,2],  pch = 3, col = "red", xaxt = "n", yaxt = "n", xlab = "", ylab = "", xlim = range(xy.f[,1], xy.l[,1]), ylim = range(xy.f[,2], xy.l[,2]), cex = pcex*xy.f[,3])
  points(xy.l[,1], xy.l[,2], pch = 4, col = "blue", cex = pcex*xy.l[,3])
}
