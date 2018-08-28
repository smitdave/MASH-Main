###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Processing of peri-domestic simulation experiments
#     MBITES Team
#     August 2018
#
###############################################################################

rm(list = ls());gc()
library(MBITES)


###############################################################################
# load data & calculate bionomics for all runs
###############################################################################

library(jsonlite)
library(parallel)
library(lokern)

# where the files can be found
directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/finals/"
lscape_dir <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/"

# iterate over all runs
for(i in 1:26){

  cat("processing run ",i,"\n")

  run <- as.character(i)

  MBITES <- list()

  mosquitos_df <- fromJSON(paste0(directory,"/mosquito_F_",run,".json"), flatten = TRUE)
  null_m <- which(sapply(mosquitos_df$id,is.null))
  if(length(null_m)>0){
    mosquitos_df <- mosquitos_df[-null_m,]
  }

  humans_df <- fromJSON(paste0(directory,"/human_",run,".json"),flatten = TRUE)
  null_h <- which(sapply(humans_df$id,is.null))
  if(length(null_h)>0){
    humans_df <- humans_df[-null_h,]
  }

  # lifespan and survival
  MBITES$surv <- Bionomics_lifespan(mosquitos_df)
  MBITES$surv_cdf <- ecdf(MBITES$surv$lifespan)
  MBITES$surv_tt <- seq(from=0,to=max(MBITES$surv$lifespan),by=0.25)
  MBITES$surv_s <- 1 - MBITES$surv_cdf(MBITES$surv_tt)
  MBITES$surv_mean <- weighted.mean(MBITES$surv_tt,MBITES$surv_s)

  MBITES$life_mean <- mean(MBITES$surv$lifespan)

  # number of blood hosts
  MBITES$blood_hosts <- Bionomics_humanBloodHost(mosquitos_df,who = "all")
  MBITES$blood_hosts_mean <- mean(MBITES$blood_hosts$humanHost)

  # feeding interval
  MBITES$blood_interval <- Bionomics_bloodIntervals(mosquitos_df,who = "all")
  MBITES$blood_interval_mean <- mean(MBITES$blood_interval$rest_intervals)

  # vectorial capacity
  MBITES$vc <- Bionomics_vectorialCapacity(mosquitos = mosquitos_df,humans = humans_df,EIP = 10,spatial = T)
  MBITES$vc_df <- data.frame(vc=sapply(MBITES$vc,function(x){x$VC}))
  MBITES$vc_mean <- mean(MBITES$vc_df$vc)

  # egg production
  MBITES$lifetime_egg <- Bionomics_lifetimeOviposition(mosquitos_df,TRUE)
  MBITES$lifetime_egg_mean <- mean(MBITES$lifetime_egg$lifetime)

  # egg laying rate
  MBITES$egg_rate <- Bionomics_ovipositionRate(mosquitos_df)
  MBITES$egg_rate_mean <- mean(MBITES$egg_rate$ages)

  # blood feeding rate
  MBITES$blood_rate <- Bionomics_bloodfeedingRate(mosquitos_df)
  MBITES$blood_rate_mean <- mean(MBITES$blood_rate)

  MBITES$dmat <- as.matrix(read.csv(paste0(lscape_dir,"DavidSmith/MBITES-Demo/dist_",run,".csv"), header = FALSE))

  # spatial dispersion of vectorial capacity
  with(MBITES,{

    cat("getting pairs of bites ... \n")

    # get pairs of bites
    spatial_vc_pairs <- lapply(vc,function(x){x$spatialVC})
    spatial_vc_pairs <- Filter(function(x){length(x)>0},spatial_vc_pairs)
    spatial_vc_pairs <- do.call(c,spatial_vc_pairs)

    cat("calculating distance matrix of bite pairs ... \n")

    # get distance matrix between sites
    spatial_vc_dist <- lapply(spatial_vc_pairs,function(x){
      out <- NULL
      i <- x$origin
      for(j in 1:length(x$dest)){
        out <- append(out,dmat[i,x$dest[j]])
      }
      return(out)
    })
    spatial_vc_dist <- do.call(c,spatial_vc_dist)
    spatial_vc_dist <- sort(spatial_vc_dist,decreasing = FALSE)

    cat("binning distance matrix ... \n")

    # discretize into distance bins
    spatial_vc_bins <- unique(spatial_vc_dist)

    # comparisons of floats
    fequal <- function(x,y){
      abs(x-y) <= .Machine$double.eps
    }

    cat("calculating empirical PMF and CDF ... \n")

    # get empirical PDF by summing stuff in the distance bins (takes awhile, use parallel if you can)
    spatial_vc_PDF_emp <- mclapply(X = spatial_vc_bins,FUN = function(x,spatial_vc_dist){
      length(spatial_vc_dist[which(fequal(spatial_vc_dist,x))])
    },spatial_vc_dist=spatial_vc_dist,mc.cores = detectCores()-2)
    spatial_vc_PDF_emp <- unlist(spatial_vc_PDF_emp) # mclapply outputs lists; coerce to vector
    # technically its a PMF so we normalize it
    spatial_vc_PDF_emp <- spatial_vc_PDF_emp/sum(spatial_vc_PDF_emp)

    # get empirical CDF by preforming a cumulative sum over data points in distance bins
    spatial_vc_CDF_emp <- cumsum(spatial_vc_PDF_emp)

    cat("smoothing empirical PMF and CDF ... \n")

    # smoothed CDF and PDF
    MBITES$spatial_vc_CDF_sth <<- spatial_vc_CDF_sth <- glkerns(spatial_vc_bins,spatial_vc_CDF_emp,deriv = 0,korder = 4,x.out=spatial_vc_bins)
    MBITES$spatial_vc_PDF_sth <<- spatial_vc_PDF_sth <- glkerns(spatial_vc_bins,spatial_vc_CDF_emp,deriv = 1,korder = 3,x.out=spatial_vc_bins)

    MBITES$spatial_vc_CDF_emp <<- spatial_vc_CDF_emp
    MBITES$spatial_vc_PDF_emp <<- spatial_vc_PDF_emp
    cat("done! \n")
  })

  # spatial dispersion of egg batches
  # spatial egg dispersion
  with(MBITES,{

    cat("getting pairs of oviposition events ... \n")

    # get pairs of eggs
    spatial_egg_pairs <- Filter(f = function(x){
      !(is.nan(x$natal) && is.nan(x$dest))
    },x = lifetime_egg$dispersion)

    cat("calculating distance matrix of oviposition pairs ... \n")

    # spatial egg dispersion
    spatial_egg_pairs <- lapply(spatial_egg_pairs,function(x){
      out <- NULL
      i <- x$natal
      for(j in 1:length(x$dest)){
        out <- append(out,dmat[i,x$dest[j]])
      }
      return(out)
    })
    spatial_egg_dist <- do.call(c,spatial_egg_pairs)
    spatial_egg_dist <- sort(spatial_egg_dist,decreasing = FALSE)

    spatial_egg_bins <- unique(spatial_egg_dist)

    cat("calculating empirical PMF and CDF ... \n")

    # get empirical PDF by summing stuff in the distance bins (takes awhile, use parallel if you can)
    spatial_egg_PDF_emp <- mclapply(X = spatial_egg_bins,FUN = function(x,spatial_egg_dist){
      length(spatial_egg_dist[which(fequal(spatial_egg_dist,x))])
    },spatial_egg_dist=spatial_egg_dist,mc.cores = detectCores()-2)
    spatial_egg_PDF_emp <- unlist(spatial_egg_PDF_emp) # mclapply outputs lists; coerce to vector
    # technically its a PMF so we normalize it
    spatial_egg_PDF_emp <- spatial_egg_PDF_emp/sum(spatial_egg_PDF_emp)

    # get empirical CDF by preforming a cumulative sum over data points in distance bins
    spatial_egg_CDF_emp <- cumsum(spatial_egg_PDF_emp)

    cat("smoothing empirical PMF and CDF ... \n")

    # smoothed CDF and PDF
    MBITES$spatial_egg_CDF_sth <- glkerns(spatial_egg_bins,spatial_egg_CDF_emp,deriv = 0,korder = 4,x.out=spatial_egg_bins)
    MBITES$spatial_egg_PDF_sth <- glkerns(spatial_egg_bins,spatial_egg_CDF_emp,deriv = 1,korder = 3,x.out=spatial_egg_bins)

    MBITES$spatial_egg_CDF_emp <<- spatial_egg_CDF_emp
    MBITES$spatial_egg_PDF_emp <<- spatial_egg_PDF_emp
    cat("done! \n")
  })

  outfile <- paste0(directory,"/analysis",run,".rds")
  cat("writing out to: ",outfile,"\n")
  saveRDS(object = MBITES,file = outfile,compress = TRUE)

  rm(MBITES,mosquitos_df,humans_df);gc()
  cat("\n")
}

