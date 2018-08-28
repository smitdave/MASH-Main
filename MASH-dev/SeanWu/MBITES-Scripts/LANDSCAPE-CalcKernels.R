###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Plot Movement Kernels
#     MBITES Team
#     July 2018
#
###############################################################################


###############################################################################
# load data and libraries, define helper functions
###############################################################################

rm(list=ls());gc()

library(parallel)
library(grDevices)

# get a landscape
dir_dev <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/"
dist <- as.matrix(read.csv(paste0(dir_dev,"DavidSmith/MBITES-Demo/dist_5.csv"), header = FALSE))
kernel <- as.matrix(read.csv(paste0(dir_dev,"DavidSmith/MBITES-Demo/movement_5.csv"), header = FALSE))
kernel <- kernel/rowSums(kernel)

# comparisons of floats
fequal <- function(x,y){
  abs(x-y) <= .Machine$double.eps
}


###############################################################################
# plot movement for entire landscape
###############################################################################

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


# plot smoothed PDFs
par(mar = c(5, 5, 3, 5))
alpha <- 0.5
maxy <- max(PDF_sth$y)
plot(distBins,PDF_sth$y,type="l",col=adjustcolor("mediumblue",alpha),lwd=3,ylim=c(.Machine$double.eps,maxy),xlim=c(0,4),
     xlab="Distance",ylab="Density",main="Smoothed Distance Kernel CDF and PDF") # PDF
par(new = TRUE)

# plot smoothed CDFs
cdf_final <- predict(object = CDF_sth,x = distBins,deriv = 0)
plot(distBins,cdf_final$y,type="l",col=adjustcolor("firebrick3",alpha),lwd=3,xlim=c(0,4), xaxt = "n", yaxt = "n",ylab = "", xlab = "")
axis(side=4, at = pretty(range(cdf_final$y)))
mtext("Cumulative Probability", side = 4, line = 3)
par(mar = c(5,4,2,2)) # defaults


###############################################################################
# calculate distributions for each site
###############################################################################

nsite <- nrow(dist)

sitesKern <- vector(mode = "list",length = nsite)

# notation Vi: means vector for site i
pb <- txtProgressBar(min = 1,max = nsite)

# go over all sites
for(i in 1:nsite){

  # distance and probabilities for site i
  distVi <- as.vector(dist[i,])
  probVi <- as.vector(kernel[i,])

  # get rid of zero distance elements (have probability 0 anyway)
  zeroDi <- which(fequal(distVi,0))

  distVi <- distVi[-zeroDi]
  probVi <- probVi[-zeroDi]

  # sort by increasing distance
  ordDi <- order(distVi)

  distVi <- distVi[ordDi]
  probVi <- probVi[ordDi]

  # get empirical PDF by summing stuff in the distance bins (takes awhile, use parallel if you can)
  PDF_empi <- mclapply(X = distBins,FUN = function(x,probVi,distVi){
    # sum(probVi[which(fequal(distVi,x))])
    sum(probVi[which(distVi %in% x)])
  },probVi=probVi,distVi=distVi,mc.cores = detectCores()-2)
  PDF_empi <- unlist(PDF_empi) # mclapply outputs lists; coerce to vector

  # technically its a PMF so we normalize it
  PDF_empi <- PDF_empi/sum(PDF_empi)

  # get empirical CDF by preforming a cumulative sum over data points in distance bins
  CDF_empi <- cumsum(PDF_empi)

  # smoothed CDF
  CDF_sthi <- smooth.spline(x = distBins,y = CDF_empi,all.knots = TRUE,cv = NA,keep.data = FALSE)
  CDF_sthi$y <- CDF_sthi$y / max(CDF_sthi$y)
  # force the smoothed CDF to be an increasing function
  CDF_sthi$y <- sort(CDF_sthi$y,decreasing = FALSE)
  # cdf_erri <- which(diff(CDF_sthi$y) < 0)
  # if(length(cdf_erri)>0){
  #   CDF_sthi$y[cdf_erri[1]:length(CDF_sthi$y)] <- sort(CDF_sthi$y[cdf_erri[1]:length(CDF_sthi$y)],decreasing = FALSE)
  # }

  # differentiate smoothed CDF at bins to get smooth PDF (PMF really)
  PDF_sthi <- predict(object = CDF_sthi,x = distBins,deriv = 1)
  if(length(which(PDF_sthi$y<0))>0){
    PDF_sthi$y[which(PDF_sthi$y<0)] = 0
  }
  PDF_sthi$y <- PDF_sthi$y/sum(PDF_sthi$y) # normalize

  # put it in the container for this site
  sitesKern[[i]]$PDF_emp <- PDF_empi
  sitesKern[[i]]$CDF_emp <- CDF_empi
  sitesKern[[i]]$CDF_sth <- CDF_sthi
  sitesKern[[i]]$PDF_sth <- PDF_sthi

  setTxtProgressBar(pb,i)
}

# save or read back in the object
# saveRDS(object = sitesKern,file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticKernel.rds"))
# sitesKern <- readRDS(file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticKernel.rds"))

# calculate mean of all the smooth CDFs and PDFs
cdfs <- sapply(X = sitesKern,function(x){x$CDF_sth$y})
meanCDF <- rowMeans(cdfs)
pdfs <- sapply(X = sitesKern,function(x){x$PDF_sth$y})
meanPDF <- rowMeans(pdfs)

# plot smoothed PDFs
alpha <- 0.5
maxy <- max(sapply(sitesKern,function(x){max(x$PDF_sth$y)}))
maxy <- round(maxy,digits = 2)
plot(distBins,sitesKern[[1]]$PDF_sth$y,type="l",col=adjustcolor("mediumblue",alpha),lwd=3,ylim=c(.Machine$double.eps,maxy),xlim=c(0,4))
for(i in 2:nsite){
  lines(distBins,sitesKern[[i]]$PDF_sth$y,col=adjustcolor("mediumblue",alpha),lwd=3)
}
lines(distBins,meanPDF,col=grey(level = 0.1),lwd=3)

# plot smoothed CDFs
plot(sitesKern[[1]]$CDF_sth$x,sitesKern[[1]]$CDF_sth$y,type="l",col=adjustcolor("firebrick3",alpha),lwd=3,ylim=c(0,1),xlim=c(0,4))
for(i in 2:nsite){
  lines(sitesKern[[i]]$CDF_sth$x,sitesKern[[i]]$CDF_sth$y,col=adjustcolor("firebrick3",alpha),lwd=3)
}
lines(sitesKern[[i]]$CDF_sth$x,meanCDF,col=grey(level = 0.1),lwd=3)






###############################################################################
# Calculate & process movement kernels for all sites and landscapes
###############################################################################

library(parallel)
library(lokern)

# comparisons of floats
fequal <- function(x,y){
  abs(x-y) <= .Machine$double.eps
}

# where the files can be found
directory <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/"
out_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/finals/"

for(i in 1:26){

  KERN <- list()

  cat("calculating movement distributions for landscape ",i,"\n")

  run <- as.character(i)

  dist_mat <- as.matrix(read.csv(paste0(directory,"DavidSmith/MBITES-Demo/dist_",run,".csv"), header = FALSE))
  kernel <- as.matrix(read.csv(paste0(directory,"DavidSmith/MBITES-Demo/movement_",run,".csv"), header = FALSE))
  kernel <- kernel/rowSums(kernel)

  KERN$dist <- as.vector(dist_mat)
  KERN$prob <- as.vector(kernel)

  # get rid of diagonal of dist matrix (mosy has no self loops)
  zeros <- which(fequal(KERN$dist,0))

  KERN$dist <- KERN$dist[-zeros]
  KERN$prob <- KERN$prob[-zeros]

  # sort in increasing distance
  ord <- order(KERN$dist)

  KERN$dist <- KERN$dist[ord]
  KERN$prob <- KERN$prob[ord]

  # make distance bins
  KERN$distance_bins <- unique(KERN$dist)

  # get empirical PDF by summing stuff in the distance bins (takes awhile, use parallel if you can)
  KERN$PDF_emp <- mclapply(X = KERN$distance_bins,FUN = function(x,prob,dist){
    sum(prob[which(fequal(dist,x))])
  },prob=KERN$prob,dist=KERN$dist,mc.cores = detectCores()-2)
  KERN$PDF_emp <- unlist(KERN$PDF_emp) # mclapply outputs lists; coerce to vector

  # technically its a PMF so we normalize it
  KERN$PDF_emp <- KERN$PDF_emp/sum(KERN$PDF_emp)

  # get empirical CDF by preforming a cumulative sum over data points in distance bins
  KERN$CDF_emp <- cumsum(KERN$PDF_emp)

  # smoothed CDF and PDF
  KERN$CDF_sth <- glkerns(KERN$distance_bins,KERN$PDF_emp,deriv = 0,korder = 4,x.out=KERN$distance_bins)
  KERN$PDF_sth <- glkerns(KERN$distance_bins,KERN$CDF_emp,deriv = 1,korder = 3,x.out=KERN$distance_bins)

  cat("calculating movement distributions for all sites\n")

  n <- nrow(dist_mat)

  KERN$sites <- vector("list",n)

  pb <- txtProgressBar(1,n)
  for(j in 1:n){

    # distance and probabilities for site i
    dist <- as.vector(dist_mat[j,])
    prob <- as.vector(kernel[j,])

    # get rid of zero distance elements (have probability 0 anyway)
    zeros <- which(fequal(dist,0))

    dist <- dist[-zeros]
    prob <- prob[-zeros]

    # sort by increasing distance
    ord <- order(dist)

    dist <- dist[ord]
    prob <- prob[ord]

    # get empirical PDF by summing stuff in the distance bins (takes awhile, use parallel if you can)
    PDF_emp <- mclapply(X = KERN$distance_bins,FUN = function(x,prob,dist){
      sum(prob[which(dist %in% x)])
    },prob=prob,dist=dist,mc.cores = detectCores()-2)
    PDF_emp <- unlist(PDF_emp) # mclapply outputs lists; coerce to vector

    # technically its a PMF so we normalize it
    PDF_emp <- PDF_emp/sum(PDF_emp)

    # get empirical CDF by preforming a cumulative sum over data points in distance bins
    CDF_emp <- cumsum(PDF_emp)

    # smoothed CDF and PDF
    CDF_sth <- glkerns(KERN$distance_bins,PDF_emp,deriv = 0,korder = 4,x.out=KERN$distance_bins)
    PDF_sth <- glkerns(KERN$distance_bins,CDF_emp,deriv = 1,korder = 3,x.out=KERN$distance_bins)

    # put it in the container for this site
    KERN$sites[[j]]$PDF_emp <- PDF_emp
    KERN$sites[[j]]$CDF_emp <- CDF_emp
    KERN$sites[[j]]$CDF_sth <- CDF_sth
    KERN$sites[[j]]$PDF_sth <- PDF_sth

    setTxtProgressBar(pb,j)
  }
  setTxtProgressBar(pb,j+1)

  outfile <- paste0(out_directory,"processed_kernels_",run,".rds")
  cat("\n")
  cat("writing out to: ",outfile,"\n")
  saveRDS(object = KERN,file = outfile,compress = TRUE)

  rm(KERN);gc()
  cat("\n")
}
