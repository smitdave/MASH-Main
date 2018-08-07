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
