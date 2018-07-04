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
library(cobs)

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
# CDF_emp <- mclapply(X = distBins,FUN = function(x,probV,distV){
#   sum(probV[which(distV <= x)])
# },probV=probV,distV=distV,mc.cores = 4)
# CDF_emp <- unlist(CDF_emp)
#
# CDF_emp <- CDF_emp/max(CDF_emp)

# smoothed CDF

CDF_sth <- smooth.spline(x = distBins,y = CDF_emp,all.knots = TRUE,cv = NA,keep.data = FALSE)
CDF_sth$y <- CDF_sth$y / max(CDF_sth$y)
# force the smoothed CDF to be an increasing function
cdf_err <- which(diff(CDF_sth$y) < 0)
CDF_sth$y[cdf_err[1]:length(CDF_sth$y)] <- sort(CDF_sth$y[cdf_err[1]:length(CDF_sth$y)],decreasing = FALSE)
# # take the ones that still wont behave and force the function to increase
# cdf_err <- which(diff(CDF_sth$y) < 0)
# if(length(cdf_err)>1){
#   warning("after initial forcing of CDF to increase there are still > 1 values that are broken")
# }
#
# CDF_sth$y[cdf_err] <- (CDF_sth$y[cdf_err-1]+CDF_sth$y[cdf_err+1]) / 2



# i=1
# while(any(diff(CDF_sth$y) < 0)){
#   cat("i: ",i,"\n");i=i+1
#   cdf_err <- which(diff(CDF_sth$y) < 0)
#   CDF_sth$y[cdf_err] <- sort(CDF_sth$y[cdf_err],decreasing = FALSE)
# }

# CDF_sth <- cobs(x = distBins,y = CDF_emp,constraint = "increase")
# CDF_sth <- scam(CDF_emp~s(distBins,k=500,bs="mpi"),family = gaussian(link="identity"),not.exp = T)
# CDF_sth <- ksmooth(distBins,CDF_emp,kernel="normal",bandwidth = 5 * dpill(distBins,CDF_emp))


# differentiate smoothed CDF at bins to get smooth PDF (PMF really)
PDF_sth <- predict(object = CDF_sth,x = distBins,deriv = 1)
PDF_sth$y[which(PDF_sth$y<0)] = 0
PDF_sth$y <- PDF_sth$y/sum(PDF_sth$y) # normalize


###############################################################################
# calculate distributions for each site
###############################################################################

# distance bins to use for each site
distV <- as.vector(dist)
distV <- distV[-which(fequal(distV,0))]
distBins <- unique(distV)

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
    sum(probVi[which(fequal(distVi,x))])
  },probVi=probVi,distVi=distVi,mc.cores = detectCores()-2)
  PDF_empi <- unlist(PDF_empi) # mclapply outputs lists; coerce to vector

  # technically its a PMF so we normalize it
  PDF_empi <- PDF_empi/sum(PDF_empi)

  # get empirical CDF by preforming a cumulative sum over data points in distance bins
  CDF_empi <- mclapply(X = distBins,FUN = function(x,probVi,distVi){
    sum(probVi[which(distVi <= x)])
  },probVi=probVi,distVi=distVi,mc.cores = detectCores()-2)
  CDF_empi <- unlist(CDF_empi)

  CDF_empi <- CDF_empi/max(CDF_empi)

  # smoothed CDF
  CDF_sthi <- smooth.spline(x = distBins,y = CDF_empi,all.knots = TRUE,cv = NA,keep.data = FALSE)

  # differentiate smoothed CDF at bins to get smooth PDF (PMF really)
  PDF_sthi <- predict(object = CDF_sthi,x = distBins,deriv = 1)
  PDF_sthi$y <- PDF_sthi$y / sum(PDF_sthi$y) # normalize)

  sitesKern[[i]]$PDF_emp <- PDF_empi
  sitesKern[[i]]$CDF_emp <- CDF_empi
  sitesKern[[i]]$CDF_sth <- CDF_sthi
  sitesKern[[i]]$PDF_sth <- PDF_sthi

  setTxtProgressBar(pb,i)
}

sum(sitesKern[[1]]$PDF_sth)






###############################################################################
# biyonka code
###############################################################################

setwd("~/Downloads/Mission01")
library(ggplot2)
library(parallel)

dist = as.matrix(read.csv("lscape_dist_5.csv", header = FALSE))
kernel = as.matrix(read.csv("lscape_kernel_5.csv", header = FALSE))

kernel_list = unlist(as.data.frame(t(as.matrix(kernel))))
dist_list = unlist(as.data.frame(t(as.matrix(dist))))
d = data.frame(Distance = dist_list, Probability = kernel_list)

#Data Points plot
ggplot(d, aes(x=Distance, y=Probability)) +
  geom_point(size=0.40, alpha = 0.1, color = 'firebrick3') +
  scale_y_continuous(breaks = seq(0, 1, by=0.2), limits=c(0,1)) +
  ggtitle("Data Points") +
  theme(plot.title = element_text(hjust = 0.5))


#PDF plot
x = ggplot()
#I've only gone up to 395 because for some reason, the axes start to disappear
#and other weird things begin to happen past 400 (reached elasped time limit error)
for (i in seq(1, 460*459, 459)) {
  #grab subset of data corresponding to each site
  small = d[c(i:(i+459)), ]

  #order by distance
  small = small[order(small$Distance), ]

  #add geom_step plot to our ggplot
  x = x + geom_step(data = small,
                    mapping = aes(x=Distance, y=Probability),
                    direction="vh",
                    color = 'mediumblue',
                    alpha = 0.1, size = 0.2)
}
#make everything pretty
x = x +  scale_y_continuous(breaks = seq(0, 1, by=0.2), limits=c(0,1)) +
  ggtitle("PDF") +
  theme(plot.title = element_text(hjust = 0.5))

#get median line
dist.sorted = data.frame((apply(dist,1,sort)))
dist_medians = apply(dist.sorted, 1, median)

#sort probs based on distances
B = as.matrix(kernel)
A = as.matrix(dist)
prob.sorted = as.data.frame((sapply(1:NROW(A), function(i) B[i,][order(A[i,])])))
prob_medians = apply(prob.sorted, 1, median)

#plot median
x = x + geom_step(data = data.frame(x = dist_medians, y = prob_medians),
                  mapping = aes(x=x, y=y),
                  direction="vh",
                  color = 'black',
                  alpha = 0.9, size = 1)
x



#CDF plot
x = ggplot()
#I've only gone up to 395 because for some reason, the axes start to disappear
#and other weird things begin to happen past 400 (reached elasped time limit error)
for (i in seq(1, 460*459, 459)) {
  #grab subset of data corresponding to each site
  small = d[c(i:(i+459)), ]

  #order by distance
  small = small[order(small$Distance), ]
  small['cumsum'] = cumsum(small$Probability)

  #precision errors in summing make the cumsum larger or smaller than 1 sometimes, so I need to
  #add this condition
  #if (small$cumsum[460] <= 1 & small$cumsum[460] > 0.99){
  x = x + geom_step(data = small,
                    mapping = aes(x=Distance, y=cumsum),
                    direction="vh",
                    color = 'purple',
                    alpha = 0.15, size = 0.2)
  #}
  #add geom_step plot to our ggplot

}
#make everything pretty
x = x + scale_y_continuous(breaks = seq(0, 1, by=0.2), limits=c(0,1)) +
  ggtitle("CDF") + ylab("Probability")+
  theme(plot.title = element_text(hjust = 0.5))

#get median line
dist.sorted = data.frame((apply(dist,1,sort)))
dist_medians = apply(dist.sorted, 1, mean)

#sort probs based on distances
B = as.matrix(kernel)
A = as.matrix(dist)
prob.sorted = as.data.frame((sapply(1:NROW(A), function(i) B[i,][order(A[i,])])))
prob_medians = apply(prob.sorted, 1, mean)
w = cumsum(prob_medians)
#plot median
x + geom_step(data = data.frame(x = dist_medians, y = w),
              mapping = aes(x=x, y=y),
              direction="vh",
              color = 'black',
              alpha = 0.9, size = 1)
w

