###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Process Movement Kernels
#     MBITES Team
#     August 2018
#
###############################################################################


###############################################################################
# Calculate & process movement kernels for all sites and landscapes
###############################################################################

rm(list=ls());gc()
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
  KERN$CDF_sth <- glkerns(c(0,KERN$distance_bins),c(0,KERN$CDF_emp),deriv = 0,korder = 4,x.out=c(0,KERN$distance_bins))
  KERN$CDF_sth$est <- KERN$CDF_sth$est / max(KERN$CDF_sth$est)
  KERN$CDF_sth$est <- sort(KERN$CDF_sth$est,decreasing = FALSE)
  KERN$PDF_sth <- predict(KERN$CDF_sth,deriv=1)
  # KERN$PDF_sth <- glkerns(c(0,KERN$distance_bins),c(0,KERN$CDF_emp),deriv = 1,korder = 3,x.out=c(0,KERN$distance_bins))

  # # old method using splines
  # CDF_sth <- smooth.spline(x = distBins,y = CDF_emp,all.knots = TRUE,cv = NA,keep.data = FALSE)
  # CDF_sth$y <- CDF_sth$y / max(CDF_sth$y)
  # # force the smoothed CDF to be an increasing function
  # CDF_sth$y <- sort(CDF_sth$y,decreasing = FALSE)

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
    CDF_sth <- glkerns(c(0,KERN$distance_bins),c(0,CDF_emp),deriv = 0,korder = 4,x.out=c(0,KERN$distance_bins))
    CDF_sth$est <- CDF_sth$est / max(KERN$CDF_sth$est)
    CDF_sth$est <- sort(CDF_sth$est,decreasing = FALSE)
    PDF_sth <- predict(CDF_sth,deriv=1)
    # CDF_sth <- glkerns(KERN$distance_bins,CDF_emp,deriv = 0,korder = 4,x.out=KERN$distance_bins)
    # PDF_sth <- glkerns(KERN$distance_bins,CDF_emp,deriv = 1,korder = 3,x.out=KERN$distance_bins)

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

  rm(KERN,dist_mat,kernel,dist,prob,ord,zeros,PDF_emp,CDF_emp,PDF_sth,CDF_sth,n);gc()
  cat("\n")
}
