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
lscapes <- c(1,13,26)
jobs <- lapply(lscapes,function(i){
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

  # vectorial capacity
  if(verbose){cat("calculating human-centric vectorial capacity\n")}
  MBITES$vc2 <- vc2 <- Bionomics_vectorialCapacity(mosquitos = mosquitos_df,humans = humans_df,EIP = 2,spatial = T)
  MBITES$vc_df2 <- data.frame(vc=sapply(MBITES$vc2,function(x){x$VC}))
  MBITES$vc_mean2 <- mean(MBITES$vc_df2$vc)

  # # clear out and write
  # if(verbose){cat("writing basic bionomics out to: ",paste0(job$outfile,"_basic.rds"),"\n")}
  # saveRDS(object = MBITES,file = paste0(job$outfile,"_basic.rds"),compress = TRUE)
  # rm(MBITES);gc()
  #
  # # new object to write out
  # MBITES <- list()

  MBITES$dmat <- job$dmat

  # spatial dispersion of vectorial capacity
  if(verbose){cat("calculating spatial dispersion of vectorial capacity\n")}
  # with(MBITES,{

    if(verbose){cat("getting pairs of bites ... \n")}

    # get pairs of bites
    spatial_vc_pairs <- lapply(vc2,function(x){x$spatialVC})
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
  if(verbose){cat("writing spatial dispersion of vectorial capacity out to: ",paste0(job$outfile,"_VC2.rds"),"\n")}
  saveRDS(object = MBITES,file = paste0(job$outfile,"_VC2.rds"),compress = TRUE)
  rm(MBITES,vc);gc()
}
