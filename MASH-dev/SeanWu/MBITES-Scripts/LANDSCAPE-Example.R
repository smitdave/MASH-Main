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
nSite = 60

# number of resources
nFeed = 30
nAqua = 30

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
# lambda_a = 5 # lambda summed across all sites
# lambda_w = rgamma(nAqua, shape=1, scale = 1) # relative weights of sites
# K = lambda_a*lambda_w / sum(lambda_w) # carrying capacity of each site
# lambda = lapply(K,function(x){x*(1+sin(2*pi*(1:365)/365))})
lambda <- rep(1,nAqua)

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

library(MBITES)

# initialize methods
MBITES_Setup_Timing(timing_model = 2,
                    rate_b = 1/(3/24),tmin_b = 0,
                    rate_bs = 1/(8/24),tmin_bs = 0,
                    rate_o = 1/(3/24),tmin_o = 0,
                    rate_os = 1/(8/24),tmin_os = 0,
                    ppr_model = 2,rate_ppr = 1/(30/24),tmin_ppr = 0
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
MBITES:::Parameters$set_parameters(disperse = 0.05,Bs_surv = 0.95,Os_surv = 0.95,B_surv = 0.98,O_surv = 0.98,
                                   Bs_succeed = 0.99,Os_succeed = 0.99,B_succeed = 0.95,O_succeed = 0.99,
                                   S_u = 0)

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

dist <- as.matrix(read.csv(file = paste0(directory,"sites_movement.csv"),
                  sep = ",",header = F,stringsAsFactors = F,
                  colClasses = "numeric"))

###############################################################################
# basic bionomics
###############################################################################

# lifespan
lf <- Bionomics_lifespan(mosquitos_df)
mean_lf <- mean(lf$lifespan)
sd(lf$lifespan)

ggplot() + geom_histogram(data = lf, aes(lifespan), fill = "steelblue", bins = 20) +
  geom_vline(xintercept = mean_lf,col="firebrick3",size=1.15) +
  ggtitle(paste0("Mosquito Lifespans (mean: ",round(mean_lf,3),")")) + xlab("Days") + ylab("Frequency") + theme_bw()

# human blood hosts
bh <- Bionomics_humanBloodHost(mosquitos_df,who = "human")
mean_bh <- mean(bh$humanHost)

ggplot() + geom_histogram(data = bh, aes(humanHost), fill = "steelblue", bins = 20) +
  geom_vline(xintercept = mean_bh,col="firebrick3",size=1.15) +
  ggtitle(paste0("Number of Human Blood Hosts per mosquito (mean: ",round(mean_bh,3),")")) + xlab("Num Hosts") + ylab("Frequency") + theme_bw()

# blood meal intervals
bmi <- Bionomics_bloodIntervals(mosquitos_df,who = "human")
mean_bmi <- mean(bmi$bmIntervals)

ggplot() + geom_histogram(data = data.frame(bmi), aes(bmIntervals), fill = "steelblue", bins = 20) +
  geom_vline(xintercept = mean_bmi,col="firebrick3",size=1.15) +
  ggtitle(paste0("Human Blood Meal Interval (mean: ",round(mean_bmi,3),")")) + xlab("Time") + ylab("Frequency") + theme_bw()

# Bionomics_bloodfeedingRate
bfr <- Bionomics_bloodfeedingRate(mosquitos_df)
bfr_m <- mean(bfr)

# vectorial capacity
vc <- Bionomics_vectorialCapacity(mosquitos = mosquitos_df,humans = humans_df,EIP = 10,spatial = T)
vc_df <- data.frame(vc=sapply(vc,function(x){x$VC}))
mean_vc <- mean(vc_df$vc)

ggplot() + geom_histogram(data = vc_df, aes(vc), fill = "steelblue", bins = 20) +
  geom_vline(xintercept = mean_vc,col="firebrick3",size=1.15) +
  ggtitle(paste0("Vectorial Capacity (mean: ",round(mean_vc,3),")")) + xlab("Secondary Bites") + ylab("Frequency") + theme_bw()

# lifetime egg production
egg <- Bionomics_lifetimeOviposition(mosquitos_df,TRUE)
egg_df <- data.frame(egg=egg$lifetime)
mean_egg <- mean(egg_df$egg)

ggplot() + geom_histogram(data = egg_df, aes(egg), fill = "steelblue", bins = 20) +
  geom_vline(xintercept = mean_egg,col="firebrick3",size=1.15) +
  ggtitle(paste0("Lifetime Egg Production (mean: ",round(mean_egg,3),")")) + xlab("Eggs") + ylab("Frequency") + theme_bw()


###############################################################################
# dispersion of mosquitos
###############################################################################

cum_disperse <- Bionomics_cumulativeDisperse(mosquitos_df)
cum_disperse <- sapply(X = cum_disperse,FUN = function(x){
  if(length(x)>1){
    d <- 0
    for(i in 1:(length(x)-1)){
      d <- d + dist[x[i],x[i+1]]
    }
    return(d)
  } else {
    return(0)
  }
},USE.NAMES = FALSE)

cum_disperseC <- Bionomics_cumulativeDisperseCpp(mosquitos_df,dist)

abs_disperse <- Bionomics_absoluteDisperse(mosquitos_df)
abs_disperse <- sapply(X = abs_disperse,FUN = function(x){
  if(length(x)>1){
    return(dist[x[1],x[2]])
  } else {
    return(0)
  }
})

abs_disperseC <- Bionomics_absoluteDisperseCpp(mosquitos_df,dist)

###############################################################################
# spatial bionomics: vectorial capacity
###############################################################################

library(parallel)
library(lokern)

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
vc_dist <- sort(vc_dist,decreasing = FALSE)

vc_bins <- unique(vc_dist)

# comparisons of floats
fequal <- function(x,y){
  abs(x-y) <= .Machine$double.eps
}

# vcecdf <- ecdf(vc_dist)
# vc_x <- knots(vcecdf)
# vc_y <- sapply(vc_x,function(x){
#   vcecdf(x+(.Machine$double.eps^0.5)) - vcecdf(x-(.Machine$double.eps^0.5))
# })
# plot(vc_x,vc_y,pch=16)

# get empirical PDF by summing stuff in the distance bins (takes awhile, use parallel if you can)
PDF_emp <- mclapply(X = vc_bins,FUN = function(x,vc_dist){
  length(vc_dist[which(fequal(vc_dist,x))])
},vc_dist=vc_dist,mc.cores = detectCores()-2)
PDF_emp <- unlist(PDF_emp) # mclapply outputs lists; coerce to vector
# technically its a PMF so we normalize it
PDF_emp <- PDF_emp/sum(PDF_emp)

# get empirical CDF by preforming a cumulative sum over data points in distance bins
CDF_emp <- cumsum(PDF_emp)

# smoothed CDF and PDF
CDF_sth <- glkerns(vc_bins,CDF_emp,deriv = 0,korder = 4,x.out=vc_bins)
PDF_sth <- glkerns(vc_bins,CDF_emp,deriv = 1,korder = 3,x.out=vc_bins)
PDF_mean <- round(weighted.mean(PDF_sth$x.out,PDF_sth$est),3)

# plot
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(CDF_sth$x.out, CDF_sth$est,type="l",col="firebrick3",lwd=3,
     ylab="CDF",xlab="Distance",main=paste0("Spatial Dispersion of Vectorial Capacity\n mean: ",PDF_mean))
par(new = TRUE)
plot(PDF_sth$x.out, PDF_sth$est, type = "l",col="mediumblue",lwd=3,
     axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = pretty(range(PDF_sth$est)))
mtext("PDF", side=4, line=3)


# ###############################################################################
# # spatial bionomics: egg production
# ###############################################################################
#
# egg_pairs <- Filter(f = function(x){
#   !(is.nan(x$natal) && is.nan(x$dest))
# },x = egg$dispersion)
#
# # spatial egg dispersion
# egg_pairs <- lapply(egg_pairs,function(x){
#   out <- NULL
#   i <- x$natal
#   for(j in 1:length(x$dest)){
#     out <- append(out,dist[i,x$dest[j]])
#   }
#   return(out)
# })
# egg_dist <- do.call(c,egg_pairs)
# egg_dist <- sort(egg_dist,decreasing = FALSE)
#
# egg_bins <- unique(egg_dist)
#
# # get empirical PDF by summing stuff in the distance bins (takes awhile, use parallel if you can)
# PDF_emp <- mclapply(X = egg_bins,FUN = function(x,egg_dist){
#   length(egg_dist[which(fequal(egg_dist,x))])
# },egg_dist=egg_dist,mc.cores = detectCores()-2)
# PDF_emp <- unlist(PDF_emp) # mclapply outputs lists; coerce to vector
# # technically its a PMF so we normalize it
# PDF_emp <- PDF_emp/sum(PDF_emp)
#
# # get empirical CDF by preforming a cumulative sum over data points in distance bins
# CDF_emp <- cumsum(PDF_emp)
#
# # smoothed CDF and PDF
# CDF_sth <- glkerns(egg_bins,CDF_emp,deriv = 0,korder = 4,x.out=vc_bins)
# PDF_sth <- glkerns(egg_bins,CDF_emp,deriv = 1,korder = 3,x.out=vc_bins)
#
# # plot
# par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
# plot(CDF_sth$x.out, CDF_sth$est,type="l",col="firebrick3",lwd=3,
#      ylab="CDF",xlab="Distance",main="Spatial Dispersion of Egg Batches")
# par(new = TRUE)
# plot(PDF_sth$x.out, PDF_sth$est, type = "l",col="mediumblue",lwd=3,
#      axes = FALSE, bty = "n", xlab = "", ylab = "")
# axis(side=4, at = pretty(range(PDF_sth$est)))
# mtext("PDF", side=4, line=3)
