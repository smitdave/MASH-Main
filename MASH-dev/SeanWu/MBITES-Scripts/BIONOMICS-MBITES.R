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

par(mfrow=c(1,2))

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

par(mfrow=c(1,1))


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

# plot
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(CDF_sth$x.out, CDF_sth$est,type="l",col="firebrick3",lwd=3,
     ylab="CDF",xlab="Distance",main="Spatial Dispersion of Vectorial Capacity")
par(new = TRUE)
plot(PDF_sth$x.out, PDF_sth$est, type = "l",col="mediumblue",lwd=3,
     axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = pretty(range(PDF_sth$est)))
mtext("PDF", side=4, line=3)


###############################################################################
# spatial bionomics: egg production
###############################################################################

egg_pairs <- Filter(f = function(x){
  !(is.nan(x$natal) && is.nan(x$dest))
},x = egg$dispersion)

# spatial egg dispersion
egg_pairs <- lapply(egg_pairs,function(x){
  out <- NULL
  i <- x$natal
  for(j in 1:length(x$dest)){
    out <- append(out,dist[i,x$dest[j]])
  }
  return(out)
})
egg_dist <- do.call(c,egg_pairs)
egg_dist <- sort(egg_dist,decreasing = FALSE)

egg_bins <- unique(egg_dist)

# get empirical PDF by summing stuff in the distance bins (takes awhile, use parallel if you can)
PDF_emp <- mclapply(X = egg_bins,FUN = function(x,egg_dist){
  length(egg_dist[which(fequal(egg_dist,x))])
},egg_dist=egg_dist,mc.cores = detectCores()-2)
PDF_emp <- unlist(PDF_emp) # mclapply outputs lists; coerce to vector
# technically its a PMF so we normalize it
PDF_emp <- PDF_emp/sum(PDF_emp)

# get empirical CDF by preforming a cumulative sum over data points in distance bins
CDF_emp <- cumsum(PDF_emp)

# smoothed CDF and PDF
CDF_sth <- glkerns(egg_bins,CDF_emp,deriv = 0,korder = 4,x.out=vc_bins)
PDF_sth <- glkerns(egg_bins,CDF_emp,deriv = 1,korder = 3,x.out=vc_bins)

# plot
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(CDF_sth$x.out, CDF_sth$est,type="l",col="firebrick3",lwd=3,
     ylab="CDF",xlab="Distance",main="Spatial Dispersion of Egg Batches")
par(new = TRUE)
plot(PDF_sth$x.out, PDF_sth$est, type = "l",col="mediumblue",lwd=3,
     axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = pretty(range(PDF_sth$est)))
mtext("PDF", side=4, line=3)
