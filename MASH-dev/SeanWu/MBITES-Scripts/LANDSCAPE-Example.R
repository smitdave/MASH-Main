###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Example Landscapes
#     MBITES Team
#     March 2018
#
###############################################################################

rm(list=ls());gc()
library(spatstat)
library(truncdist)
library(viridis)

# number of sites
nSite = 200

# number of resources
nFeed = 100
nAqua = 120
nSugar = 40
nMate = 20

# assign resources to sites
ix_feed = sample(x=1:nSite,size=nFeed,replace=TRUE)
w_feed = rgamma(n=nFeed,shape=1,rate=1)
ix_aqua = sample(x=1:nSite,size=nAqua,replace=TRUE)
w_aqua = rgamma(n=nAqua,shape=1,rate=1)
ix_sugar = sample(x=1:nSite,size=nSugar,replace=TRUE)
w_sugar = rgamma(n=nSugar,shape=1,rate=1)
ix_mate = sample(x=1:nSite,size=nMate,replace=TRUE)
w_mate = rgamma(n=nMate,shape=1,rate=1)

# generate lambda
lambda_a = 100 # lambda summed across all sites
lambda_w = rgamma(nAqua, shape=1, scale = 1) # relative weights of sites
K = lambda_a*lambda_w / sum(lambda_w) # carrying capacity of each site
lambda = lapply(K,function(x){x*(1+sin(2*pi*(1:365)/365))})

# 2d plane
xy_plane = owin(xrange = c(0,1),yrange = c(0,1))

# poisson scatter, matérn clustering, overdispersed
xy_pois <- rpoispp(lambda = nSite,win = xy_plane)
xy_pois <- cbind(xy_pois$x,xy_pois$y)
xy_clust <- rMatClust(kappa = nSite/5,scale = 0.1,mu = nSite/10,win = xy_plane)
xy_clust <- cbind(xy_clust$x,xy_clust$y)
xy_overdisp <- rSSI(r = 0.05,n = nSite,win = xy_plane)
xy_overdisp <- cbind(xy_overdisp$x,xy_overdisp$y)

# sample search weights
wSite = rgamma(n = nSite,shape = 1,rate = 1)

# plots
wCol = viridis(n = nSite)
wCol = wCol[order(wSite,decreasing = TRUE)]
plot(xy_overdisp,pch=16,col=wCol,main="SSI",xlab="",ylab="")
plot(xy_clust,pch=16,col=wCol,main="Matérn Clustering",xlab="",ylab="")
plot(xy_pois,pch=16,col=wCol,main="Poisson Scatter",xlab="",ylab="")

# landscapes
landscape_clust <- vector(mode = "list",length = nSite)
landscape_pois <- vector(mode = "list",length = nSite)
landscape_overdisp <- vector(mode = "list",length = nSite)

# movement kernel
exp_fit <- function(d,q,up=1){
  f_opt = function(x){
    abs(d - qexp(p = q,rate = x))
  }
  sol = optimise(f = f_opt,lower = 0,upper = up,maximum = FALSE)
  return(sol$minimum)
}

exp_kern <- exp_fit(d = 0.1,q = 0.75)

# movement matrices
dist_overdisp <- as.matrix(dist(xy_overdisp,diag = TRUE,upper = TRUE))
move_overdisp <- apply(X = dist_overdisp,MARGIN = 1,FUN = function(x){dtrunc(x = x,spec = "exp",a = 1e-12,b = Inf, rate = 1/exp_kern)})
move_overdisp <- move_overdisp/rowSums(move_overdisp)

dist_pois <- as.matrix(dist(xy_pois,diag = TRUE,upper = TRUE))
move_pois <- apply(X = dist_pois,MARGIN = 1,FUN = function(x){dtrunc(x = x,spec = "exp",a = 1e-12,b = Inf, rate = 1/exp_kern)})
move_pois <- move_pois/rowSums(move_pois)

dist_clust <- as.matrix(dist(xy_clust,diag = TRUE,upper = TRUE))
move_clust <- apply(X = dist_clust,MARGIN = 1,FUN = function(x){dtrunc(x = x,spec = "exp",a = 1e-12,b = Inf, rate = 1/exp_kern)})
move_clust <- move_clust/rowSums(move_clust)

# make landscape
for(i in 1:nSite){
  # site characteristics
  landscape_overdisp[[i]]$id = i
  landscape_overdisp[[i]]$xy = xy_overdisp[i,]
  landscape_overdisp[[i]]$type = 1L
  landscape_overdisp[[i]]$move = move_overdisp[i,-i]
  landscape_overdisp[[i]]$move_id = as.integer(names(move_overdisp[i,-i]))
  # null resources
  landscape_overdisp[[i]]$sugar = NULL
  landscape_overdisp[[i]]$mate = NULL
  landscape_overdisp[[i]]$feed = NULL
  landscape_overdisp[[i]]$aqua = NULL
}

# assign feeding resources
for(i in 1:nFeed){
  res_feed = list(w=w_feed[i],enterP=1)
  if(is.null(landscape_overdisp[[ix_feed[i]]]$feed)){
    landscape_overdisp[[ix_feed[i]]]$feed[[1]] = res_feed
  } else {
    landscape_overdisp[[ix_feed[i]]]$feed[[length(landscape_overdisp[[ix_feed[i]]]$feed)+1]] = res_feed
  }
}

# assign aquatic resources
for(i in 1:nAqua){
  res_aqua = list(w=w_aqua[i],lambda=lambda[[i]])
  if(is.null(landscape_overdisp[[ix_aqua[i]]]$aqua)){
    landscape_overdisp[[ix_aqua[i]]]$aqua[[1]] = res_aqua
  } else {
    landscape_overdisp[[ix_aqua[i]]]$aqua[[length(landscape_overdisp[[ix_aqua[i]]]$aqua)+1]] = res_aqua
  }
}

# assign sugar resources
for(i in 1:nSugar){
  res_sugar = list(w=w_sugar[i])
  if(is.null(landscape_overdisp[[ix_sugar[i]]]$sugar)){
    landscape_overdisp[[ix_sugar[i]]]$sugar[[1]] = res_sugar
  } else {
    landscape_overdisp[[ix_sugar[i]]]$sugar[[length(landscape_overdisp[[ix_sugar[i]]]$sugar)+1]] = res_sugar
  }
}

# assign mating resources
for(i in 1:nMate){
  res_mate = list(w=w_mate[i])
  if(is.null(landscape_overdisp[[ix_mate[i]]]$mate)){
    landscape_overdisp[[ix_mate[i]]]$mate[[1]] = res_mate
  } else {
    landscape_overdisp[[ix_mate[i]]]$mate[[length(landscape_overdisp[[ix_mate[i]]]$mate)+1]] = res_mate
  }
}
