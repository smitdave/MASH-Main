###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Process Bionomics for peri-domestic experiments
#     MBITES Team
#     October 2018
#
###############################################################################


###############################################################################
# initialization bits
###############################################################################

rm(list=ls());gc()

# set up
script_dir <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/SeanWu/MBITES-Scripts/"
out_dir <- "/Users/slwu89/Desktop/mbites/peridomIHME/finals/"
analysis_dir <- "/Users/slwu89/Desktop/mbites/peridomIHME/analyzed/"
lscape_dir <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/DavidSmith/MBITES-Demo/"

library(Rcpp)
library(lokern)
library(MBITES)
library(jsonlite)

Rcpp::sourceCpp(paste0(script_dir,"bionomics.cpp"))

# take a vector of distances and make ECDF,PMF,smoothed CDF, smoothed PDF
# tol: tolerance when computing PMF
smooth_kernels <- function(distances, tol = .Machine$double.eps^0.75){

  d_ecdf <- stats::ecdf(distances)
  d_knots <- stats::knots(d_ecdf)

  d_pmf <- vapply(d_knots, function(x,tol){
    d_ecdf(x+.Machine$double.eps^0.75) - d_ecdf(x-.Machine$double.eps^0.75)
  }, numeric(1), tol = tol)

  d_cdf <- lokern::glkerns(d_knots,d_ecdf(d_knots),deriv = 0,korder = 4)
  d_pdf <- lokern::glkerns(d_knots,d_ecdf(d_knots),deriv = 1,korder = 3)

  return(list(
    ecdf=d_ecdf,
    knots=d_knots,
    pmf=d_pmf,
    cdf=d_cdf,
    pdf=d_pdf
  ))
}


###############################################################################
# process run(s)
###############################################################################

n <- 26
sumstat <- vector("list",n)

verbose <- TRUE # if TRUE print out progress  bars in the fn's; o/w just have a global bar
if(!verbose){
  pb <- txtProgressBar(1,n)
}

for(i in 1:n){

  # read in data
  mosquitos <- fromJSON(paste0(out_dir,"/mosquito_F_",i,".json"), flatten = TRUE)
  mosquitos <- mosquitos[-which(sapply(mosquitos$id,is.null)),]
  humans <- fromJSON(paste0(out_dir,"/human_",i,".json"), flatten = TRUE)
  humans <- humans[-which(sapply(humans$id,is.null)),]
  dist <- as.matrix(read.csv(paste0(lscape_dir,"dist_",i,".csv"), header = FALSE))

  # summary statistics

  # state transitions
  sumstat[[i]]$transitions <- Bionomics_StateTransitionCpp(mosquitos,verbose = verbose)

  # lifespan
  sumstat[[i]]$lifespan <- Bionomics_lifespanCpp(mosquitos,verbose = verbose)

  # number of blood hosts
  sumstat[[i]]$blood_hosts <- Bionomics_humanBloodHostCpp(mosquitos,who = "human",verbose = verbose)

  # feeding interval & resting interval
  sumstat[[i]]$blood_interval <- Bionomics_bloodIntervalsCpp(mosquitos,who = "human",verbose = verbose)
  sumstat[[i]]$blood_interval <- unlist(sumstat[[i]]$blood_interval)
  sumstat[[i]]$rest_interval <- Bionomics_restIntervalsCpp(mosquitos,verbose = verbose)
  sumstat[[i]]$rest_interval <- unlist(sumstat[[i]]$rest_interval)

  # hbr (human biting proportion)
  sumstat[[i]]$human_biterate <- Bionomics_humanBitingProportionCpp(mosquitos,verbose = verbose)

  # blood feeding rate
  sumstat[[i]]$blood_rate <- Bionomics_bloodfeedingRateCpp(mosquitos,verbose = verbose)
  sumstat[[i]]$blood_rate <- unlist(sumstat[[i]]$blood_rate)

  # lifetime egg production
  sumstat[[i]]$life_egg <- Bionomics_lifetimeOvipositionCpp(mosquitos,dist,verbose = verbose)

  # oviposition interval
  sumstat[[i]]$egg_interval <- Bionomics_ovipositionIntervalCpp(mosquitos,verbose = verbose)

  # oviposition rate
  sumstat[[i]]$egg_rate <- Bionomics_ovipositionRateCpp(mosquitos,verbose = verbose)

  # vectorial capacity
  sumstat[[i]]$VC <- Bionomics_vectorialCapacityCpp(mosquitos,dist,nrow(humans),EIP = 10,unique = F,verbose = verbose)

  # vectorial capacity (unique secondary hosts)
  sumstat[[i]]$VC_unique <- Bionomics_vectorialCapacityCpp(mosquitos,dist,nrow(humans),EIP = 10,unique = T,verbose = verbose)

  # dispersion of vc and eggs
  sumstat[[i]]$vc_dispersion <- smooth_kernels(distances = sumstat[[i]]$VC$dispersion)
  sumstat[[i]]$vc_dispersion_unique <- smooth_kernels(distances = sumstat[[i]]$VC_unique$dispersion)
  sumstat[[i]]$egg_dispersion <- smooth_kernels(distances = sumstat[[i]]$life_egg$dispersion)

  # cumulative and absolute dispersal of mosquitos
  sumstat[[i]]$disperse_cum <- Bionomics_cumulativeDisperseCpp(mosquitos = mosquitos,dist = dist,verbose = verbose)
  sumstat[[i]]$disperse_abs <- Bionomics_absoluteDisperseCpp(mosquitos = mosquitos,dist = dist,verbose = verbose)

  # smoothed mosquito dispersion
  sumstat[[i]]$disperse_cum_smooth <- smooth_kernels(distances = sumstat[[i]]$disperse_cum)
  sumstat[[i]]$disperse_abs_smooth <- smooth_kernels(distances = sumstat[[i]]$disperse_abs)

  # remove data
  rm(mosquitos,humans,dist);gc()

  if(!verbose){
    setTxtProgressBar(pb,i)
  } else {
    cat("completed run: ",i," of ",n,"\n")
  }
}

saveRDS(object = sumstat,file = paste0(analysis_dir,"summary.rds"),compress = TRUE)


###############################################################################
# process landscapes
###############################################################################


# dist_mat <- as.matrix(read.csv(paste0(directory,"DavidSmith/MBITES-Demo/dist_",run,".csv"), header = FALSE))
# kernel <- as.matrix(read.csv(paste0(directory,"DavidSmith/MBITES-Demo/movement_",run,".csv"), header = FALSE))
