###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Test Bionomics
#     MBITES Team
#     August 2018
#
###############################################################################

rm(list = ls());gc()
library(MBITES)


###############################################################################
# load data & calculate bionomics
###############################################################################

library(jsonlite)

# where the files can be found
directory <- "/Users/slwu89/Desktop/mbites/peridom/"
run <- "1"
output_dir <- paste0(directory,"run",run)

mosquitos_df <- fromJSON(paste0(output_dir,"/mosquito_F_",run,".json"), flatten = TRUE)
mosquitos_df <- mosquitos_df[-which(sapply(mosquitos_df$id,is.null)),]

humans_df <- fromJSON(paste0(output_dir,"/human_",run,".json"), flatten = TRUE)
humans_df <- humans_df[-which(sapply(humans_df$id,is.null)),]


###############################################################################
# BEGIN: MBITES BIONOMICS ONLY
###############################################################################

MBITES <- new.env()

###############################################################################
# lifespan & survival
###############################################################################

MBITES$surv <- Bionomics_lifespan(mosquitos_df)
MBITES$surv_cdf <- ecdf(MBITES$surv$lifespan)
MBITES$surv_tt <- seq(from=0,to=max(MBITES$surv$lifespan),by=0.25)
MBITES$surv_s <- 1 - MBITES$surv_cdf(MBITES$surv_tt)
MBITES$surv_mean <- weighted.mean(MBITES$surv_tt,MBITES$surv_s)

MBITES$life_mean <- mean(MBITES$surv$lifespan)

par(mfrow=c(4,2))

with(MBITES,{
  plot(surv_tt, surv_s,
       type = "l", xlab = "Age (Days)", ylab = "Density",
       main=paste0("MBITES Survival Function (mean: ",round(surv_mean,2),")"),
       lwd=2,col="firebrick3",xlim=c(0,30))
  polygon(c(0,surv_tt), c(0,surv_s),
          border=NA, col=adjustcolor("firebrick3",alpha.f = 0.5))
  abline(v = surv_mean,lwd=2.5,lty=2,col="firebrick3")
})

with(MBITES,{
  hist(surv$lifespan,probability = TRUE,breaks = 100,
       col = adjustcolor("firebrick3",alpha.f = 0.5),
       xlab = "Age (days)", ylab = "Density",
       main = paste0("MBITES Lifespan (mean: ",round(life_mean,2),")"),
       xlim = c(0,30))
  abline(v = life_mean,lwd=2.5,lty=2,col="firebrick3")
})

###############################################################################
# number of human blood hosts
###############################################################################

MBITES$blood_hosts <- Bionomics_humanBloodHost(mosquitos_df,who = "all")
MBITES$blood_hosts_mean <- mean(MBITES$blood_hosts$humanHost)

with(MBITES,{
  hist(blood_hosts$humanHost,probability = TRUE,breaks = 100,
       col = adjustcolor("firebrick3",alpha.f = 0.5),
       xlab = "Number of Blood Hosts", ylab = "Density",
       main = paste0("MBITES Human Blood Hosts (mean: ",round(blood_hosts_mean,2),")"))
  abline(v = blood_hosts_mean,lwd=2.5,lty=2,col="firebrick3")
})


###############################################################################
# feeding interval
###############################################################################

MBITES$blood_interval <- Bionomics_bloodIntervals(mosquitos_df,who = "all")
MBITES$blood_interval_mean <- mean(MBITES$blood_interval$rest_intervals)

with(MBITES,{
  hist(blood_interval$rest_intervals,probability = TRUE,breaks = 100,
       col = adjustcolor("firebrick3",alpha.f = 0.5),
       xlab = "Time (days)", ylab = "Density",
       main = paste0("MBITES Feeding Cycle Duration (mean: ",round(blood_interval_mean,2),")"))
  abline(v = blood_interval_mean,lwd=2.5,lty=2,col="firebrick3")
})


###############################################################################
# vectorial capacity
###############################################################################

MBITES$vc <- Bionomics_vectorialCapacity(mosquitos = mosquitos_df,humans = humans_df,EIP = 10,spatial = T)
MBITES$vc_df <- data.frame(vc=sapply(MBITES$vc,function(x){x$VC}))
MBITES$vc_mean <- mean(MBITES$vc_df$vc)

with(MBITES,{
  hist(MBITES$vc_df$vc,probability = TRUE,breaks = 100,
       col = adjustcolor("firebrick3",alpha.f = 0.5),
       xlab = "Secondary Bites", ylab = "Density",
       main = paste0("MBITES Vectorial Capacity (mean: ",round(MBITES$vc_mean,2),")"))
  abline(v = MBITES$vc_mean,lwd=2.5,lty=2,col="firebrick3")
})


###############################################################################
# egg production
###############################################################################

MBITES$lifetime_egg <- Bionomics_lifetimeOviposition(mosquitos_df,TRUE)
MBITES$lifetime_egg_mean <- mean(MBITES$lifetime_egg$lifetime)

with(MBITES,{
  hist(lifetime_egg$lifetime,probability = TRUE,breaks = 100,
       col = adjustcolor("firebrick3",alpha.f = 0.5),
       xlab = "Eggs", ylab = "Density",
       main = paste0("MBITES Lifetime Egg Production (mean: ",round(lifetime_egg_mean,2),")"))
  abline(v = lifetime_egg_mean,lwd=2.5,lty=2,col="firebrick3")
})


###############################################################################
# egg laying rate
###############################################################################

MBITES$egg_rate <- Bionomics_ovipositionRate(mosquitos_df)
MBITES$egg_rate_mean <- mean(MBITES$egg_rate$ages)

with(MBITES,{
  hist(egg_rate$ages,probability = TRUE,breaks = 100,
       col = adjustcolor("firebrick3",alpha.f = 0.5),
       xlab = "Age (Days)", ylab = "Density",
       main = paste0("MBITES Egg Laying Rate (mean: ",round(egg_rate_mean,2),")"))
  abline(v = egg_rate_mean,lwd=2.5,lty=2,col="firebrick3")
})


###############################################################################
# blood feeding rate
###############################################################################

MBITES$blood_rate <- Bionomics_bloodfeedingRate(mosquitos_df)
MBITES$blood_rate_mean <- mean(MBITES$blood_rate)

with(MBITES,{
  hist(blood_rate,probability = TRUE,breaks = 100,
       col = adjustcolor("firebrick3",alpha.f = 0.5),
       xlab = "Age (Days)", ylab = "Density",
       main = paste0("MBITES Blood Feeding Rate (mean: ",round(blood_rate_mean,2),")"))
  abline(v = blood_rate_mean,lwd=2.5,lty=2,col="firebrick3")
})

par(mfrow=c(1,1))

###############################################################################
# spatial bionomics: distance matrix between sites
###############################################################################

lscape_dir <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/"
MBITES$dmat <- as.matrix(read.csv(paste0(lscape_dir,"DavidSmith/MBITES-Demo/dist_",run,".csv"), header = FALSE))

###############################################################################
# spatial bionomics: vectorial capacity
###############################################################################

library(parallel)
library(lokern)

# spatial vectorial capacity
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
  spatial_vc_CDF_sth <- glkerns(spatial_vc_bins,spatial_vc_CDF_emp,deriv = 0,korder = 4,x.out=spatial_vc_bins)
  spatial_vc_PDF_sth <- glkerns(spatial_vc_bins,spatial_vc_CDF_emp,deriv = 1,korder = 3,x.out=spatial_vc_bins)

  cat("done! \n")
})

# plot
with(MBITES,{
  par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
  plot(spatial_vc_CDF_sth$x.out, spatial_vc_CDF_sth$est,type="l",col="firebrick3",lwd=3,
       ylab="CDF",xlab="Distance",main="Spatial Dispersion of Vectorial Capacity")
  polygon(x=c(0,spatial_vc_CDF_sth$x.out,tail(spatial_vc_CDF_sth$x.out,1)),
          y= c(head(spatial_vc_CDF_sth$est,1),spatial_vc_CDF_sth$est,head(spatial_vc_CDF_sth$est,1)),
       border=NA, col=adjustcolor("firebrick3",alpha.f = 0.5))
  par(new = TRUE)
  plot(spatial_vc_PDF_sth$x.out, spatial_vc_PDF_sth$est, type = "l",col="steelblue",lwd=3,
       axes = FALSE, bty = "n", xlab = "", ylab = "")
  polygon(c(0,spatial_vc_PDF_sth$x.out), c(0,spatial_vc_PDF_sth$est),
       border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
  axis(side=4, at = pretty(range(spatial_vc_PDF_sth$est)))
  mtext("PDF", side=4, line=3)
})


###############################################################################
# spatial bionomics: egg production
###############################################################################

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
  spatial_egg_CDF_sth <- glkerns(spatial_egg_bins,spatial_egg_CDF_emp,deriv = 0,korder = 4,x.out=spatial_egg_bins)
  spatial_egg_PDF_sth <- glkerns(spatial_egg_bins,spatial_egg_CDF_emp,deriv = 1,korder = 3,x.out=spatial_egg_bins)

  cat("done! \n")
})

# plot
with(MBITES,{
  par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
  plot(spatial_egg_CDF_sth$x.out, spatial_egg_CDF_sth$est,type="l",col="firebrick3",lwd=3,
       ylab="CDF",xlab="Distance",main="Spatial Dispersion of Egg Batches")
  polygon(x=c(0,spatial_egg_CDF_sth$x.out,tail(spatial_egg_CDF_sth$x.out,1)),
          y= c(head(spatial_egg_CDF_sth$est,1),spatial_egg_CDF_sth$est,head(spatial_egg_CDF_sth$est,1)),
       border=NA, col=adjustcolor("firebrick3",alpha.f = 0.5))
  par(new = TRUE)
  plot(spatial_egg_PDF_sth$x.out, spatial_egg_PDF_sth$est, type = "l",col="steelblue",lwd=3,
       axes = FALSE, bty = "n", xlab = "", ylab = "")
  polygon(c(0,spatial_egg_PDF_sth$x.out), c(0,spatial_egg_PDF_sth$est),
       border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
  axis(side=4, at = pretty(range(spatial_egg_PDF_sth$est)))
  mtext("PDF", side=4, line=3)
})
