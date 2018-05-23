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

# load libraries
rm(list=ls());gc()
library(spatstat)
library(truncdist)
library(viridis)


###############################################################################
# Set up points
###############################################################################

# number of sites
nSite = 20

# number of resources
nFeed = 15
nAqua = 15

# source: https://github.com/slwu89/PlosCompBio_2013/blob/master/fig2/code/functionsModel.R
points.clustered = function(n, meanParents = 10, clusteredness = .25, ...){
  meanDist = clusteredness / sqrt(meanParents)
  meanChildren = n / meanParents
  ps = rMatClust(meanParents, meanDist, meanChildren, ...)
  while(ps$n != n){
    ps = rMatClust(meanParents, meanDist, meanChildren, ...)
  }
  return(ps)
}

# assign resources to sites
ix_feed = 1:nFeed
w_feed = rgamma(n=nFeed,shape=1,rate=1)
ix_aqua = (nSite-nAqua):nSite
w_aqua = rgamma(n=nAqua,shape=1,rate=1)

# generate lambda
lambda_a = 5 # lambda summed across all sites
lambda_w = rgamma(nAqua, shape=1, scale = 1) # relative weights of sites
K = lambda_a*lambda_w / sum(lambda_w) # carrying capacity of each site
lambda = lapply(K,function(x){x*(1+sin(2*pi*(1:365)/365))})

# 2d plane
xy_plane = owin(xrange = c(0,1),yrange = c(0,1))

# poisson scatter, matérn clustering, overdispersed
points = points.clustered(n = nSite,meanParents = 5, clusteredness = 0.25, win = xy_plane)
xy_points = cbind(points$x,points$y)

# sample search weights
wSite = rgamma(n = nSite,shape = 1,rate = 1)

# landscapes
landscape <- vector(mode = "list",length = nSite)

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
dist <- as.matrix(dist(xy_points,diag = TRUE,upper = TRUE))
movement <- apply(X = dist,MARGIN = 1,FUN = function(x){dtrunc(x = x,spec = "exp",a = 1e-12,b = Inf, rate = 1/exp_kern)})
movement <- movement/rowSums(movement)


###############################################################################
# Plot landscape
###############################################################################

movement_quantile = cut(as.vector(movement),breaks=quantile(as.vector(movement),probs=seq(0, 1, 0.2)),include.lowest = TRUE,labels = FALSE)
movement_color <- matrix(data = plasma(n = length(unique(movement_quantile)),alpha=0.5)[movement_quantile],nrow = nrow(movement),ncol = ncol(movement))

par(bg = grey(0.15))
plot.new()
for(i in 1:ncol(movement)){
  for(j in 1:nrow(movement)){
    segments(x0 = xy_points[i,1],y0 = xy_points[i,2],
             x1 = xy_points[j,1],y1 = xy_points[j,2],
             col = movement_color[i,j],lty = 1.15,lwd = 1.15)
  }
}
points(xy_points,pch=21,cex=5,bg=grey(level = 0.95,alpha = 0.85),col="white")
text(xy_points,labels=as.character(1:20),col="black")
par(bg = "white")


###############################################################################
# Make landscape initialization object
###############################################################################

for(i in 1:nSite){
  # site characteristics
  landscape[[i]]$id = i
  landscape[[i]]$xy = xy_points[i,]
  landscape[[i]]$type = 1L
  landscape[[i]]$tileID = 1L
  landscape[[i]]$move = movement[i,-i]
  landscape[[i]]$move_id = as.integer(names(movement[i,-i]))
  landscape[[i]]$haz = 0.001
  # null resources
  landscape[[i]]$feed = NULL
  landscape[[i]]$aqua = NULL
}

# assign feeding resources
for(i in 1:nFeed){
  res_feed = list(w=w_feed[i],enterP=1)
  if(is.null(landscape[[ix_feed[i]]]$feed)){
    landscape[[ix_feed[i]]]$feed[[1]] = res_feed
  } else {
    landscape[[ix_feed[i]]]$feed[[length(landscape[[ix_feed[i]]]$feed)+1]] = res_feed
  }
}

# assign aquatic resources
for(i in 1:nAqua){
  res_aqua = list(w=w_aqua[i],lambda=lambda[[i]])
  if(is.null(landscape[[ix_aqua[i]]]$aqua)){
    landscape[[ix_aqua[i]]]$aqua[[1]] = res_aqua
  } else {
    landscape[[ix_aqua[i]]]$aqua[[length(landscape[[ix_aqua[i]]]$aqua)+1]] = res_aqua
  }
}


###############################################################################
# Make human initialization object
###############################################################################

nHumans = 80

humans = data.frame(
  tileID = rep(1,nHumans),
  siteID = sample(x = 1:nFeed,size = nHumans,replace=TRUE),
  feedingID = rep(1,nHumans),
  w = rep(1,nHumans)
)


###############################################################################
# Make mosquito initialization object
###############################################################################

nMosquitos = 50

mosquitos = data.frame(
  tileID = rep(1,nMosquitos),
  siteID = sample(x = ix_aqua,size=nMosquitos,replace=TRUE),
  female = rep(T,nMosquitos)
)


###############################################################################
# Run MBITES
###############################################################################

library(MBITES)

# initialize methods
MBITES_Setup_Timing(timing_model = 1,
                    wait_b = 1,wait_o = 1,wait_m = 1,wait_s = 1,
                    wait_bs = 1,wait_os = 1,wait_ms = 1,wait_ss = 1,
                    ppr_model = 1,wait_ppr = 0.5/24)

MBITES_Setup_BloodMeal(overfeeding = FALSE)

MBITES_Setup_Oogenesis(oogenesis_model = 1,eggMaturationTime = FALSE,eggsize_model = 2,refeeding = FALSE)

MBITES_Setup_Energetics(sugar = FALSE)

MBITES_Setup_Oviposition(aqua_model = 1)

MBITES_Setup_Survival(tattering = FALSE,senescence = FALSE)

PATHOGEN_Setup(pathogen_model = "null")

# we want detailed output of blood hosts from the mosquito
trackBloodHost()

# set parameters
MBITES:::Parameters$set_parameters(disperse = 0.05)

# initialize a tile
Tile_Initialize(landscape)

Human_NULL_Initialize(humans)

MBITES_Initialize(mosquitos)

# run simulation
set_output(directory = "/Users/slwu89/Desktop/mbites/",runID = 1)
simulation(tMax = 365)

reset(directory = "/Users/slwu89/Desktop/mbites/",runID = 2)
Human_NULL_Initialize(humans)
MBITES_Initialize(mosquitos)
simulation(tMax = 365)
