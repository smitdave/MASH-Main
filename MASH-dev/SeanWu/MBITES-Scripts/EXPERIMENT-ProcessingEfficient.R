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
# Libraries
.libPaths("/ihme/malaria_modeling/dtcitron/Rlibs")
library(lokern)
library(MBITES)

###############################################################################
# make cluster
###############################################################################

# directories for files
JSON_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/finals/"
landscape_directory <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/"
# output_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/finals/analysis/"
output_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/test/"

# list of jobs to parallelize over
jobs <- lapply(1:26,function(i){
  list(
    id = i,
    # distance matrix for spatial dispersion computations
    dmat = as.matrix(read.csv(paste0(landscape_directory,"DavidSmith/MBITES-Demo/dist_",i,".csv"), header = FALSE)),
    # paths to JSON
    mosy_json = paste0(JSON_directory,"/mosquito_F_",i,".json"),
    human_json = paste0(JSON_directory,"/human_",i,".json"),
    # path to output
    outfile = paste0(output_directory,"/analysis",i)
  )
})


# process one run
process <- function(job, verbose = FALSE){

  if(verbose){cat("processing job ",job$id,"\n")}

  run <- as.character(job$id)

  MBITES <- list()

  mosquitos_df <- jsonlite::fromJSON(job$mosy_json, flatten = TRUE)
  null_m <- which(sapply(mosquitos_df$id,is.null))
  if(length(null_m)>0){
    mosquitos_df <- mosquitos_df[-null_m,]
  }

  humans_df <- jsonlite::fromJSON(job$human_json,flatten = TRUE)
  null_h <- which(sapply(humans_df$id,is.null))
  if(length(null_h)>0){
    humans_df <- humans_df[-null_h,]
  }

  # lifespan and survival
  if(verbose){cat("calculating lifespan and survival\n")}
  MBITES$surv <- Bionomics_lifespan(mosquitos_df)
  MBITES$surv_cdf <- ecdf(MBITES$surv$lifespan)
  MBITES$surv_tt <- seq(from=0,to=max(MBITES$surv$lifespan),by=0.25)
  MBITES$surv_s <- 1 - MBITES$surv_cdf(MBITES$surv_tt)
  MBITES$surv_mean <- weighted.mean(MBITES$surv_tt,MBITES$surv_s)

  MBITES$life_mean <- mean(MBITES$surv$lifespan)

  # number of blood hosts
  if(verbose){cat("calculating number of blood hosts\n")}
  MBITES$blood_hosts <- Bionomics_humanBloodHost(mosquitos_df,who = "human")
  MBITES$blood_hosts_mean <- mean(MBITES$blood_hosts$humanHost)

  # feeding interval
  if(verbose){cat("calculating feeding interval\n")}
  MBITES$blood_interval <- Bionomics_bloodIntervals(mosquitos_df,who = "human")
  MBITES$blood_interval_mean <- mean(MBITES$blood_interval$rest_intervals)

  # vectorial capacity
  if(verbose){cat("calculating human-centric vectorial capacity\n")}
  MBITES$vc <- vc <- Bionomics_vectorialCapacity(mosquitos = mosquitos_df,humans = humans_df,EIP = 10,spatial = T)
  MBITES$vc_df <- data.frame(vc=sapply(MBITES$vc,function(x){x$VC}))
  MBITES$vc_mean <- mean(MBITES$vc_df$vc)

  # vectorial capacity for mosquitos
  if(verbose){cat("calculating mosquito-centric vectorial capacity\n")}
  MBITES$vc_mosy <- vc_mosy <- Bionomics_vectorialCapacityMosquito(mosquitos = mosquitos_df,EIP = 10,spatial = T)
  MBITES$vc_mosy_df <- data.frame(vc=sapply(MBITES$vc_mosy,function(x){x$VC}))
  MBITES$vc_mosy_mean <- mean(MBITES$vc_mosy_df$vc)

  # egg production
  if(verbose){cat("calculating lifetime egg production\n")}
  MBITES$lifetime_egg <- lifetime_egg <- Bionomics_lifetimeOviposition(mosquitos_df,TRUE)
  MBITES$lifetime_egg_mean <- mean(MBITES$lifetime_egg$lifetime)

  # egg laying rate
    if(verbose){cat("calculating oviposition rate\n")}
  MBITES$egg_rate <- Bionomics_ovipositionRate(mosquitos_df)
  MBITES$egg_rate_mean <- mean(MBITES$egg_rate$ages)

  # blood feeding rate
  if(verbose){cat("calculating blood feeding rate\n")}
  MBITES$blood_rate <- Bionomics_bloodfeedingRate(mosquitos_df)
  MBITES$blood_rate_mean <- mean(MBITES$blood_rate)

  # clear out and write
  if(verbose){cat("writing basic bionomics out to: ",paste0(job$outfile,"_basic.rds"),"\n")}
  saveRDS(object = MBITES,file = paste0(job$outfile,"_basic.rds"),compress = TRUE)
  rm(MBITES);gc()

  # new object to write out
  MBITES <- list()

  MBITES$dmat <- job$dmat

  # spatial dispersion of vectorial capacity
  if(verbose){cat("calculating spatial dispersion of vectorial capacity\n")}
  # with(MBITES,{

    if(verbose){cat("getting pairs of bites ... \n")}

    # get pairs of bites
    spatial_vc_pairs <- lapply(vc,function(x){x$spatialVC})
    spatial_vc_pairs <- Filter(function(x){length(x)>0},spatial_vc_pairs)
    spatial_vc_pairs <- do.call(c,spatial_vc_pairs)

    if(verbose){cat("calculating distance matrix of bite pairs ... \n")}

    # get distance matrix between sites
    spatial_vc_dist <- lapply(spatial_vc_pairs,function(x){
      out <- NULL
      i <- x$origin
      for(j in 1:length(x$dest)){
        out <- append(out,MBITES$dmat[i,x$dest[j]])
      }
      return(out)
    })
    spatial_vc_dist <- do.call(c,spatial_vc_dist)
    spatial_vc_dist <- sort(spatial_vc_dist,decreasing = FALSE)

    if(verbose){cat("binning distance matrix ... \n")}

    # discretize into distance bins
    spatial_vc_bins <- unique(spatial_vc_dist)

    # comparisons of floats
    fequal <- function(x,y){
      abs(x-y) <= .Machine$double.eps
    }

    if(verbose){cat("calculating empirical PMF and CDF ... \n")}

    # get empirical PDF by summing stuff in the distance bins (takes awhile, use parallel if you can)
    spatial_vc_PDF_emp <- lapply(spatial_vc_bins,FUN = function(x,spatial_vc_dist){
      length(spatial_vc_dist[which(fequal(spatial_vc_dist,x))])
    },spatial_vc_dist=spatial_vc_dist)
    spatial_vc_PDF_emp <- unlist(spatial_vc_PDF_emp) # mclapply outputs lists; coerce to vector
    # technically its a PMF so we normalize it
    spatial_vc_PDF_emp <- spatial_vc_PDF_emp/sum(spatial_vc_PDF_emp)

    # get empirical CDF by preforming a cumulative sum over data points in distance bins
    spatial_vc_CDF_emp <- cumsum(spatial_vc_PDF_emp)

    if(verbose){cat("smoothing empirical PMF and CDF ... \n")}

    # smoothed CDF and PDF
    MBITES$spatial_vc_CDF_sth <- lokern::glkerns(spatial_vc_bins,spatial_vc_CDF_emp,deriv = 0,korder = 4,x.out=spatial_vc_bins)
    MBITES$spatial_vc_PDF_sth <- lokern::glkerns(spatial_vc_bins,spatial_vc_CDF_emp,deriv = 1,korder = 3,x.out=spatial_vc_bins)

    MBITES$spatial_vc_CDF_emp <- spatial_vc_CDF_emp
    MBITES$spatial_vc_PDF_emp <- spatial_vc_PDF_emp
    if(verbose){cat("done! \n")}
  # })

  # clear out and write
  if(verbose){cat("writing spatial dispersion of vectorial capacity out to: ",paste0(job$outfile,"_spatialVC.rds"),"\n")}
  saveRDS(object = MBITES,file = paste0(job$outfile,"_spatialVC.rds"),compress = TRUE)
  rm(MBITES,vc);gc()

  # new object to write out
  MBITES <- list()

  MBITES$dmat <- job$dmat

  # spatial dispersion of mosquito-centric vectorial capacity
  if(verbose){cat("calculating spatial dispersion of (mosy-centric) vectorial capacity\n")}
  # with(MBITES,{

    if(verbose){cat("getting pairs of bites ... \n")}

    # get pairs of bites
    spatial_vc_mosy_pairs <- lapply(vc_mosy,function(x){x$spatialVC})
    spatial_vc_mosy_pairs <- Filter(function(x){length(x)>0},spatial_vc_mosy_pairs)
    spatial_vc_mosy_pairs <- do.call(c,spatial_vc_mosy_pairs)

    if(verbose){cat("calculating distance matrix of bite pairs ... \n")}

    # get distance matrix between sites
    spatial_vc_mosy_dist <- lapply(spatial_vc_mosy_pairs,function(x){
      out <- NULL
      i <- x$origin
      for(j in 1:length(x$dest)){
        out <- append(out,MBITES$dmat[i,x$dest[j]])
      }
      return(out)
    })
    spatial_vc_mosy_dist <- do.call(c,spatial_vc_mosy_dist)
    spatial_vc_mosy_dist <- sort(spatial_vc_mosy_dist,decreasing = FALSE)

    if(verbose){cat("binning distance matrix ... \n")}

    # discretize into distance bins
    spatial_vc_mosy_bins <- unique(spatial_vc_mosy_dist)

    # comparisons of floats
    fequal <- function(x,y){
      abs(x-y) <= .Machine$double.eps
    }

    if(verbose){cat("calculating empirical PMF and CDF ... \n")}

    # get empirical PDF by summing stuff in the distance bins (takes awhile, use parallel if you can)
    spatial_vc_mosy_PDF_emp <- lapply(spatial_vc_mosy_bins,FUN = function(x,spatial_vc_mosy_dist){
      length(spatial_vc_mosy_dist[which(fequal(spatial_vc_mosy_dist,x))])
    },spatial_vc_mosy_dist=spatial_vc_mosy_dist)
    spatial_vc_mosy_PDF_emp <- unlist(spatial_vc_mosy_PDF_emp) # mclapply outputs lists; coerce to vector
    # technically its a PMF so we normalize it
    spatial_vc_mosy_PDF_emp <- spatial_vc_mosy_PDF_emp/sum(spatial_vc_mosy_PDF_emp)

    # get empirical CDF by preforming a cumulative sum over data points in distance bins
    spatial_vc_mosy_CDF_emp <- cumsum(spatial_vc_mosy_PDF_emp)

    if(verbose){cat("smoothing empirical PMF and CDF ... \n")}

    # smoothed CDF and PDF
    MBITES$spatial_vc_mosy_CDF_sth <- lokern::glkerns(spatial_vc_mosy_bins,spatial_vc_mosy_CDF_emp,deriv = 0,korder = 4,x.out=spatial_vc_mosy_bins)
    MBITES$spatial_vc_mosy_PDF_sth <- lokern::glkerns(spatial_vc_mosy_bins,spatial_vc_mosy_CDF_emp,deriv = 1,korder = 3,x.out=spatial_vc_mosy_bins)

    MBITES$spatial_vc_mosy_CDF_emp <- spatial_vc_mosy_CDF_emp
    MBITES$spatial_vc_mosy_PDF_emp <- spatial_vc_mosy_PDF_emp
    if(verbose){cat("done! \n")}
  # })

  # clear out and write
  if(verbose){cat("writing spatial dispersion of (mosy-centric) vectorial capacity out to: ",paste0(job$outfile,"_spatialMosyVC.rds"),"\n")}
  saveRDS(object = MBITES,file = paste0(job$outfile,"_spatialMosyVC.rds"),compress = TRUE)
  rm(MBITES,vc_mosy);gc()

  # new object to write out
  MBITES <- list()

  MBITES$dmat <- job$dmat

  # spatial dispersion of egg batches
  # spatial egg dispersion
  if(verbose){cat("calculating spatial dispersion of egg batches\n")}
  # with(MBITES,{

    if(verbose){cat("getting pairs of oviposition events ... \n")}

    # get pairs of eggs
    spatial_egg_pairs <- Filter(f = function(x){
      !(is.nan(x$natal) && is.nan(x$dest))
    },x = lifetime_egg$dispersion)

    if(verbose){cat("calculating distance matrix of oviposition pairs ... \n")}

    # spatial egg dispersion
    spatial_egg_pairs <- lapply(spatial_egg_pairs,function(x){
      out <- NULL
      i <- x$natal
      for(j in 1:length(x$dest)){
        out <- append(out,MBITES$dmat[i,x$dest[j]])
      }
      return(out)
    })
    spatial_egg_dist <- do.call(c,spatial_egg_pairs)
    spatial_egg_dist <- sort(spatial_egg_dist,decreasing = FALSE)

    spatial_egg_bins <- unique(spatial_egg_dist)

    if(verbose){cat("calculating empirical PMF and CDF ... \n")}

    # get empirical PDF by summing stuff in the distance bins (takes awhile, use parallel if you can)
    spatial_egg_PDF_emp <- lapply(spatial_egg_bins,FUN = function(x,spatial_egg_dist){
      length(spatial_egg_dist[which(fequal(spatial_egg_dist,x))])
    },spatial_egg_dist=spatial_egg_dist)
    spatial_egg_PDF_emp <- unlist(spatial_egg_PDF_emp) # mclapply outputs lists; coerce to vector
    # technically its a PMF so we normalize it
    spatial_egg_PDF_emp <- spatial_egg_PDF_emp/sum(spatial_egg_PDF_emp)

    # get empirical CDF by preforming a cumulative sum over data points in distance bins
    spatial_egg_CDF_emp <- cumsum(spatial_egg_PDF_emp)

    if(verbose){cat("smoothing empirical PMF and CDF ... \n")}

    # smoothed CDF and PDF
    MBITES$spatial_egg_CDF_sth <- lokern::glkerns(spatial_egg_bins,spatial_egg_CDF_emp,deriv = 0,korder = 4,x.out=spatial_egg_bins)
    MBITES$spatial_egg_PDF_sth <- lokern::glkerns(spatial_egg_bins,spatial_egg_CDF_emp,deriv = 1,korder = 3,x.out=spatial_egg_bins)

    MBITES$spatial_egg_CDF_emp <- spatial_egg_CDF_emp
    MBITES$spatial_egg_PDF_emp <- spatial_egg_PDF_emp
    cat("done! \n")
  # })

  # clear out and write
  if(verbose){cat("writing spatial dispersion of egg batches out to: ",paste0(job$outfile,"_spatialEgg.rds"),"\n")}
  saveRDS(object = MBITES,file = paste0(job$outfile,"_spatialEgg.rds"),compress = TRUE)

  rm(MBITES,lifetime_egg,mosquitos_df,humans_df);gc()
  if(verbose){cat("\ndone processing job ",job$id,"\n")}
}








# process one run
process_chunks <- function(job, nchunk, verbose = FALSE){

  if(verbose){cat("processing job ",job$id,"\n")}

  run <- as.character(job$id)

  mosquitos_df <- jsonlite::fromJSON(job$mosy_json, flatten = TRUE)
  null_m <- which(sapply(mosquitos_df$id,is.null))
  if(length(null_m)>0){
    mosquitos_df <- mosquitos_df[-null_m,]
  }

  humans_df <- jsonlite::fromJSON(job$human_json,flatten = TRUE)
  null_h <- which(sapply(humans_df$id,is.null))
  if(length(null_h)>0){
    humans_df <- humans_df[-null_h,]
  }

  # get indices to chunk over
  chunks <- parallel::splitIndices(nrow(mosquitos_df),nchunk)

  # iterate over chunks
  for(i in 1:length(chunks)){

    cat("begin iterating over chunk ",i," of ",length(chunks),"\n")

    MBITES <- list()

    # lifespan and survival
    if(verbose){cat("calculating lifespan and survival\n")}
    MBITES$surv <- Bionomics_lifespan(mosquitos_df[chunks[[i]],])
    MBITES$surv_cdf <- ecdf(MBITES$surv$lifespan)
    MBITES$surv_tt <- seq(from=0,to=max(MBITES$surv$lifespan),by=0.25)
    MBITES$surv_s <- 1 - MBITES$surv_cdf(MBITES$surv_tt)
    MBITES$surv_mean <- weighted.mean(MBITES$surv_tt,MBITES$surv_s)

    MBITES$life_mean <- mean(MBITES$surv$lifespan)

    # number of blood hosts
    if(verbose){cat("calculating number of blood hosts\n")}
    MBITES$blood_hosts <- Bionomics_humanBloodHost(mosquitos_df[chunks[[i]],],who = "human")
    MBITES$blood_hosts_mean <- mean(MBITES$blood_hosts$humanHost)

    # feeding interval
    if(verbose){cat("calculating feeding interval\n")}
    MBITES$blood_interval <- Bionomics_bloodIntervals(mosquitos_df[chunks[[i]],],who = "human")
    MBITES$blood_interval_mean <- mean(MBITES$blood_interval$rest_intervals)

    # vectorial capacity
    if(verbose){cat("calculating human-centric vectorial capacity\n")}
    MBITES$vc <- vc <- Bionomics_vectorialCapacity(mosquitos = mosquitos_df[chunks[[i]],],humans = humans_df,EIP = 10,spatial = T)
    MBITES$vc_df <- data.frame(vc=sapply(MBITES$vc,function(x){x$VC}))
    MBITES$vc_mean <- mean(MBITES$vc_df$vc)

    # vectorial capacity for mosquitos
    if(verbose){cat("calculating mosquito-centric vectorial capacity\n")}
    MBITES$vc_mosy <- vc_mosy <- Bionomics_vectorialCapacityMosquito(mosquitos = mosquitos_df[chunks[[i]],],EIP = 10,spatial = T)
    MBITES$vc_mosy_df <- data.frame(vc=sapply(MBITES$vc_mosy,function(x){x$VC}))
    MBITES$vc_mosy_mean <- mean(MBITES$vc_mosy_df$vc)

    # egg production
    if(verbose){cat("calculating lifetime egg production\n")}
    MBITES$lifetime_egg <- lifetime_egg <- Bionomics_lifetimeOviposition(mosquitos_df[chunks[[i]],],TRUE)
    MBITES$lifetime_egg_mean <- mean(MBITES$lifetime_egg$lifetime)

    # egg laying rate
      if(verbose){cat("calculating oviposition rate\n")}
    MBITES$egg_rate <- Bionomics_ovipositionRate(mosquitos_df[chunks[[i]],])
    MBITES$egg_rate_mean <- mean(MBITES$egg_rate$ages)

    # blood feeding rate
    if(verbose){cat("calculating blood feeding rate\n")}
    MBITES$blood_rate <- Bionomics_bloodfeedingRate(mosquitos_df[chunks[[i]],])
    MBITES$blood_rate_mean <- mean(MBITES$blood_rate)

    # clear out and write
    chunkfile <- paste0(job$outfile,"_chunk_",i,"_basic.rds")
    if(verbose){cat("writing basic bionomics out to: ",chunkfile,"\n")}
    saveRDS(object = MBITES,file = chunkfile,compress = TRUE)
    rm(MBITES);gc()

    # new object to write out
    MBITES <- list()

    MBITES$dmat <- job$dmat

    # spatial dispersion of vectorial capacity
    if(verbose){cat("calculating spatial dispersion of vectorial capacity\n")}
    # with(MBITES,{

      if(verbose){cat("getting pairs of bites ... \n")}

      # get pairs of bites
      spatial_vc_pairs <- lapply(vc,function(x){x$spatialVC})
      spatial_vc_pairs <- Filter(function(x){length(x)>0},spatial_vc_pairs)
      spatial_vc_pairs <- do.call(c,spatial_vc_pairs)

      if(verbose){cat("calculating distance matrix of bite pairs ... \n")}

      # get distance matrix between sites
      spatial_vc_dist <- lapply(spatial_vc_pairs,function(x){
        out <- NULL
        i <- x$origin
        for(j in 1:length(x$dest)){
          out <- append(out,MBITES$dmat[i,x$dest[j]])
        }
        return(out)
      })
      spatial_vc_dist <- do.call(c,spatial_vc_dist)
      spatial_vc_dist <- sort(spatial_vc_dist,decreasing = FALSE)

      if(verbose){cat("binning distance matrix ... \n")}

      # discretize into distance bins
      spatial_vc_bins <- unique(spatial_vc_dist)

      # comparisons of floats
      fequal <- function(x,y){
        abs(x-y) <= .Machine$double.eps
      }

      if(verbose){cat("calculating empirical PMF and CDF ... \n")}

      # get empirical PDF by summing stuff in the distance bins (takes awhile, use parallel if you can)
      spatial_vc_PDF_emp <- lapply(spatial_vc_bins,FUN = function(x,spatial_vc_dist){
        length(spatial_vc_dist[which(fequal(spatial_vc_dist,x))])
      },spatial_vc_dist=spatial_vc_dist)
      spatial_vc_PDF_emp <- unlist(spatial_vc_PDF_emp) # mclapply outputs lists; coerce to vector
      # technically its a PMF so we normalize it
      spatial_vc_PDF_emp <- spatial_vc_PDF_emp/sum(spatial_vc_PDF_emp)

      # get empirical CDF by preforming a cumulative sum over data points in distance bins
      spatial_vc_CDF_emp <- cumsum(spatial_vc_PDF_emp)

      if(verbose){cat("smoothing empirical PMF and CDF ... \n")}

      # smoothed CDF and PDF
      MBITES$spatial_vc_CDF_sth <- lokern::glkerns(spatial_vc_bins,spatial_vc_CDF_emp,deriv = 0,korder = 4,x.out=spatial_vc_bins)
      MBITES$spatial_vc_PDF_sth <- lokern::glkerns(spatial_vc_bins,spatial_vc_CDF_emp,deriv = 1,korder = 3,x.out=spatial_vc_bins)

      MBITES$spatial_vc_CDF_emp <- spatial_vc_CDF_emp
      MBITES$spatial_vc_PDF_emp <- spatial_vc_PDF_emp
      if(verbose){cat("done! \n")}
    # })

    # clear out and write
    chunkfile <- paste0(job$outfile,"_chunk_",i,"_spatialVC.rds")
    if(verbose){cat("writing spatial dispersion of vectorial capacity out to: ",chunkfile,"\n")}
    saveRDS(object = MBITES,file = chunkfile,compress = TRUE)
    rm(MBITES,vc);gc()

    # new object to write out
    MBITES <- list()

    MBITES$dmat <- job$dmat

    # spatial dispersion of mosquito-centric vectorial capacity
    if(verbose){cat("calculating spatial dispersion of (mosy-centric) vectorial capacity\n")}
    # with(MBITES,{

      if(verbose){cat("getting pairs of bites ... \n")}

      # get pairs of bites
      spatial_vc_mosy_pairs <- lapply(vc_mosy,function(x){x$spatialVC})
      spatial_vc_mosy_pairs <- Filter(function(x){length(x)>0},spatial_vc_mosy_pairs)
      spatial_vc_mosy_pairs <- do.call(c,spatial_vc_mosy_pairs)

      if(verbose){cat("calculating distance matrix of bite pairs ... \n")}

      # get distance matrix between sites
      spatial_vc_mosy_dist <- lapply(spatial_vc_mosy_pairs,function(x){
        out <- NULL
        i <- x$origin
        for(j in 1:length(x$dest)){
          out <- append(out,MBITES$dmat[i,x$dest[j]])
        }
        return(out)
      })
      spatial_vc_mosy_dist <- do.call(c,spatial_vc_mosy_dist)
      spatial_vc_mosy_dist <- sort(spatial_vc_mosy_dist,decreasing = FALSE)

      if(verbose){cat("binning distance matrix ... \n")}

      # discretize into distance bins
      spatial_vc_mosy_bins <- unique(spatial_vc_mosy_dist)

      # comparisons of floats
      fequal <- function(x,y){
        abs(x-y) <= .Machine$double.eps
      }

      if(verbose){cat("calculating empirical PMF and CDF ... \n")}

      # get empirical PDF by summing stuff in the distance bins (takes awhile, use parallel if you can)
      spatial_vc_mosy_PDF_emp <- lapply(spatial_vc_mosy_bins,FUN = function(x,spatial_vc_mosy_dist){
        length(spatial_vc_mosy_dist[which(fequal(spatial_vc_mosy_dist,x))])
      },spatial_vc_mosy_dist=spatial_vc_mosy_dist)
      spatial_vc_mosy_PDF_emp <- unlist(spatial_vc_mosy_PDF_emp) # mclapply outputs lists; coerce to vector
      # technically its a PMF so we normalize it
      spatial_vc_mosy_PDF_emp <- spatial_vc_mosy_PDF_emp/sum(spatial_vc_mosy_PDF_emp)

      # get empirical CDF by preforming a cumulative sum over data points in distance bins
      spatial_vc_mosy_CDF_emp <- cumsum(spatial_vc_mosy_PDF_emp)

      if(verbose){cat("smoothing empirical PMF and CDF ... \n")}

      # smoothed CDF and PDF
      MBITES$spatial_vc_mosy_CDF_sth <- lokern::glkerns(spatial_vc_mosy_bins,spatial_vc_mosy_CDF_emp,deriv = 0,korder = 4,x.out=spatial_vc_mosy_bins)
      MBITES$spatial_vc_mosy_PDF_sth <- lokern::glkerns(spatial_vc_mosy_bins,spatial_vc_mosy_CDF_emp,deriv = 1,korder = 3,x.out=spatial_vc_mosy_bins)

      MBITES$spatial_vc_mosy_CDF_emp <- spatial_vc_mosy_CDF_emp
      MBITES$spatial_vc_mosy_PDF_emp <- spatial_vc_mosy_PDF_emp
      if(verbose){cat("done! \n")}
    # })

    # clear out and write
    chunkfile <- paste0(job$outfile,"_chunk_",i,"_spatialMosyVC.rds")
    if(verbose){cat("writing spatial dispersion of (mosy-centric) vectorial capacity out to: ",chunkfile,"\n")}
    saveRDS(object = MBITES,file = chunkfile,compress = TRUE)
    rm(MBITES,vc_mosy);gc()

    # new object to write out
    MBITES <- list()

    MBITES$dmat <- job$dmat

    # spatial dispersion of egg batches
    # spatial egg dispersion
    if(verbose){cat("calculating spatial dispersion of egg batches\n")}
    # with(MBITES,{

      if(verbose){cat("getting pairs of oviposition events ... \n")}

      # get pairs of eggs
      spatial_egg_pairs <- Filter(f = function(x){
        !(is.nan(x$natal) && is.nan(x$dest))
      },x = lifetime_egg$dispersion)

      if(verbose){cat("calculating distance matrix of oviposition pairs ... \n")}

      # spatial egg dispersion
      spatial_egg_pairs <- lapply(spatial_egg_pairs,function(x){
        out <- NULL
        i <- x$natal
        for(j in 1:length(x$dest)){
          out <- append(out,MBITES$dmat[i,x$dest[j]])
        }
        return(out)
      })
      spatial_egg_dist <- do.call(c,spatial_egg_pairs)
      spatial_egg_dist <- sort(spatial_egg_dist,decreasing = FALSE)

      spatial_egg_bins <- unique(spatial_egg_dist)

      if(verbose){cat("calculating empirical PMF and CDF ... \n")}

      # get empirical PDF by summing stuff in the distance bins (takes awhile, use parallel if you can)
      spatial_egg_PDF_emp <- lapply(spatial_egg_bins,FUN = function(x,spatial_egg_dist){
        length(spatial_egg_dist[which(fequal(spatial_egg_dist,x))])
      },spatial_egg_dist=spatial_egg_dist)
      spatial_egg_PDF_emp <- unlist(spatial_egg_PDF_emp) # mclapply outputs lists; coerce to vector
      # technically its a PMF so we normalize it
      spatial_egg_PDF_emp <- spatial_egg_PDF_emp/sum(spatial_egg_PDF_emp)

      # get empirical CDF by preforming a cumulative sum over data points in distance bins
      spatial_egg_CDF_emp <- cumsum(spatial_egg_PDF_emp)

      if(verbose){cat("smoothing empirical PMF and CDF ... \n")}

      # smoothed CDF and PDF
      MBITES$spatial_egg_CDF_sth <- lokern::glkerns(spatial_egg_bins,spatial_egg_CDF_emp,deriv = 0,korder = 4,x.out=spatial_egg_bins)
      MBITES$spatial_egg_PDF_sth <- lokern::glkerns(spatial_egg_bins,spatial_egg_CDF_emp,deriv = 1,korder = 3,x.out=spatial_egg_bins)

      MBITES$spatial_egg_CDF_emp <- spatial_egg_CDF_emp
      MBITES$spatial_egg_PDF_emp <- spatial_egg_PDF_emp
      cat("done! \n")
    # })

    # clear out and write
    chunkfile <- paste0(job$outfile,"_chunk_",i,"_spatialEgg.rds")
    if(verbose){cat("writing spatial dispersion of egg batches out to: ",chunkfile,"\n")}
    saveRDS(object = MBITES,file = chunkfile,compress = TRUE)

    rm(MBITES,lifetime_egg);gc()
  } # end iteration over chunks

  rm(mosquitos_df,humans_df);gc()
  if(verbose){cat("\ndone processing job ",job$id,"\n")}
}
