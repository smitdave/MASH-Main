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
library(MBITES)

set.seed(42)

directory <- "/Users/slwu89/Desktop/mbites/"

###############################################################################
# Set up points
###############################################################################

# number of sites
nSite = 25

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

# get an allocation of resources to sites with minimal overlap
get_minimalOverlap <- function(nSite,nFeed,nAqua){
  feed <- sample(x = 1:nSite,size = nFeed)
  notFeed <- setdiff(x = 1:nSite,y = feed)
  aqua <- notFeed
  if(length(aqua)<nAqua){
    aqua <- append(aqua,sample(feed,nAqua-length(aqua)))
  }
  if(length(aqua)>nAqua){
    aqua <- aqua[1:nAqua]
  }
  return(list(feed=feed,aqua=aqua))
}

# assign resources to sites
resources <- get_minimalOverlap(nSite,nFeed,nAqua)
ix_feed = resources$feed
w_feed = rgamma(n=nFeed,shape=1,rate=1)
ix_aqua = resources$aqua
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
exp_fit <- function(d,q,up=1e3){
  f_opt = function(x){
    abs(d - qexp(p = q,rate = x))
  }
  sol = optimise(f = f_opt,lower = 0,upper = up,maximum = FALSE)
  return(sol$minimum)
}

exp_kern <- exp_fit(d = 0.1,q = 0.75)

# movement matrices
dist <- as.matrix(dist(xy_points,diag = TRUE,upper = TRUE))
movement <- apply(X = dist,MARGIN = 1,FUN = function(x){dtrunc(x = x,spec = "exp",a = 1e-12,b = Inf, rate = exp_kern)})
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
# write landscape
###############################################################################

write.table(x = dist,
            file = paste0(directory,"sites_dist.csv"),
            sep = ",",row.names = F,col.names = F)

write.table(x = t(sapply(landscape,function(x){x$xy})),
          file = paste0(directory,"sites_xy.csv"),
          sep = ",",row.names = F,col.names = F)


sites_movement <- matrix(data = 0,nrow = length(landscape),ncol = length(landscape))
sites_movement[1,] <- c(0,landscape[[1]]$move)
for(i in 2:(length(landscape)-1)){
  sites_movement[i,] <- c(landscape[[i]]$move[1:(i-1)], 0, landscape[[i]]$move[i:(length(landscape)-1)])
}
sites_movement[length(landscape),] <- c(landscape[[length(landscape)]]$move,0)

write.table(x = sites_movement,
          file = paste0(directory,"sites_movement.csv"),
          sep = ",",row.names = F,col.names = F)

###############################################################################
# Make human initialization object
###############################################################################

nHumans = 200

humans = data.frame(
  tileID = rep(1,nHumans),
  siteID = sample(x = ix_feed,size = nHumans,replace=TRUE),
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

# library(MBITES)

# initialize methods
MBITES_Setup_Timing(timing_model = 2,
                    rate_b = 1/(1/2/24),tmin_b = 0,
                    rate_bs = 1/(6/24),tmin_bs = 0,
                    rate_o = 1/(1/2/24),tmin_o = 0,
                    rate_os = 1/(6/24),tmin_os = 0,
                    ppr_model = 2,rate_ppr = 1/(18/24),tmin_ppr = 0
)

MBITES_Setup_BloodMeal(overfeeding = FALSE)

MBITES_Setup_Oogenesis(oogenesis_model = 1,eggMaturationTime = FALSE,eggsize_model = 2,refeeding = 3)

MBITES_Setup_Energetics(sugar = FALSE)

MBITES_Setup_Oviposition(aqua_model = 1)

MBITES_Setup_Survival(tattering = FALSE,senescence = FALSE)

PATHOGEN_Setup(pathogen_model = "null")

# we want detailed output of blood hosts from the mosquito
trackBloodHost()
trackOviposition()

# set parameters
MBITES:::Parameters$set_parameters(disperse = 0.25,Bs_surv = 0.95,Os_surv = 0.95,B_surv = 0.98,O_surv = 0.98,
                                   Bs_succeed = 0.99,Os_succeed = 0.99,B_succeed = 0.95,O_succeed = 0.99,
                                   S_u = 0,energyFromBlood_b = 2e16)

# initialize a tile
Tile_Initialize(landscape)

Human_NULL_Initialize(humans)

MBITES_Initialize(mosquitos)


# run simulation
set_output(directory = directory,runID = 1)
simulation(tMax = 365,pretty = TRUE)
hardreset()


###############################################################################
# Analysis
###############################################################################

library(jsonlite)
library(ggplot2)

# where the files can be found
output_dir <- paste0(directory,"run1")

mosquitos_df <- fromJSON(paste0(output_dir,"/mosquito_F_1.json"), flatten = TRUE)
mosquitos_df <- mosquitos_df[-which(sapply(mosquitos_df$id,is.null)),]
humans_df <- fromJSON(paste0(output_dir,"/human_1.json"), flatten = TRUE)
humans_df <- humans_df[-which(sapply(humans_df$id,is.null)),]


###############################################################################
# basic bionomics
###############################################################################

# lifespan
lf <- Bionomics_lifespan(mosquitos_df)
mean(lf$lifespan)
sd(lf$lifespan)

ggplot() + geom_histogram(data = lf, aes(lifespan), fill = "steelblue", bins = 20) +
  ggtitle("Mosquito Lifespans") + xlab("Days") + ylab("Frequency") + theme_bw()

# human blood hosts
bh <- Bionomics_humanBloodHost(mosquitos_df,who = "human")
mean(bh$humanHost)

ggplot() + geom_histogram(data = bh, aes(humanHost), fill = "steelblue", bins = 20) +
  ggtitle("Number of Human Blood Hosts per mosquito") + xlab("Num Hosts") + ylab("Frequency") + theme_bw()

# blood meal intervals
bmi <- Bionomics_bloodIntervals(mosquitos_df,who = "human")
mean(bmi$bmIntervals)

ggplot() + geom_histogram(data = bmi, aes(bmIntervals), fill = "steelblue", bins = 20) +
  ggtitle("Human Blood Meal Interval") + xlab("Duration") + ylab("Frequency") + theme_bw()

# vectorial capacity
vc <- Bionomics_vectorialCapacity(mosquitos = mosquitos_df,humans = humans_df,EIP = 5,spatial = T)
vc_df <- data.frame(vc=sapply(vc,function(x){x$VC}))

ggplot() + geom_histogram(data = vc_df, aes(vc), fill = "steelblue", bins = 20) +
  ggtitle("Vectorial Capacity") + xlab("Secondary Bites") + ylab("Frequency") + theme_bw()


###############################################################################
# spatial bionomics
###############################################################################

# spatial vectorial capacity
vc_pairs <- lapply(vc,function(x){x$spatialVC})
vc_pairs <- Filter(function(x){length(x)>0},vc_pairs)
vc_pairs <- do.call(c,vc_pairs)
# needs the distance matrix between sites to be called 'dist'
vc_dist <- lapply(vc_pairs,function(x){
  out <- NULL
  i <- x$origin
  for(j in 1:length(x$dest)){
    out <- append(out,dist[i,x$dest[j]])
  }
  return(out)
})
vc_dist <- do.call(c,vc_dist)


distV <- as.vector(dist)
probV <- as.vector(kernel)

# get rid of diagonal of dist matrix (mosy has no self loops)
zeroD <- which(fequal(distV,0))

distV <- distV[-zeroD]
probV <- probV[-zeroD]

# sort in increasing distance
ordD <- order(distV)

distV <- distV[ordD]
probV <- probV[ordD]

# make distance bins
distBins <- unique(distV)

# get empirical PDF by summing stuff in the distance bins (takes awhile, use parallel if you can)
PDF_emp <- mclapply(X = distBins,FUN = function(x,probV,distV){
  sum(probV[which(fequal(distV,x))])
},probV=probV,distV=distV,mc.cores = detectCores()-2)
PDF_emp <- unlist(PDF_emp) # mclapply outputs lists; coerce to vector

# technically its a PMF so we normalize it
PDF_emp <- PDF_emp/sum(PDF_emp)

# get empirical CDF by preforming a cumulative sum over data points in distance bins
CDF_emp <- cumsum(PDF_emp)

# smoothed CDF
CDF_sth <- smooth.spline(x = distBins,y = CDF_emp,all.knots = TRUE,cv = NA,keep.data = FALSE)
CDF_sth$y <- CDF_sth$y / max(CDF_sth$y)
# force the smoothed CDF to be an increasing function
CDF_sth$y <- sort(CDF_sth$y,decreasing = FALSE)
# cdf_err <- which(diff(CDF_sth$y) < 0)
# if(length(cdf_err)>0){
#   CDF_sth$y[cdf_err[1]:length(CDF_sth$y)] <- sort(CDF_sth$y[cdf_err[1]:length(CDF_sth$y)],decreasing = FALSE)
# }

# differentiate smoothed CDF at bins to get smooth PDF (PMF really)
PDF_sth <- predict(object = CDF_sth,x = distBins,deriv = 1)
pdf_err <-which(PDF_sth$y<0)
if(length(pdf_err)>0){
  PDF_sth$y[pdf_err] = 0
}
PDF_sth$y <- PDF_sth$y/sum(PDF_sth$y) # normalize
