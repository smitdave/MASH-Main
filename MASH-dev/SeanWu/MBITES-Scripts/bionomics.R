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

i <- 1

sumstat <- list()
MBITES <- list()

mosquitos <- fromJSON(paste0(out_dir,"/mosquito_F_",i,".json"), flatten = TRUE)
mosquitos <- mosquitos[-which(sapply(mosquitos$id,is.null)),]
humans <- fromJSON(paste0(out_dir,"/human_",i,".json"), flatten = TRUE)
humans <- humans[-which(sapply(humans$id,is.null)),]
dist <- as.matrix(read.csv(paste0(lscape_dir,"dist_",i,".csv"), header = FALSE))

# state transitions
sumstat$transitions <- Bionomics_StateTransitionCpp(mosquitos)
# MBITES$transitions <- Bionomics_StateTransition(mosquitos)

# lifespan
sumstat$lifespan <- Bionomics_lifespanCpp(mosquitos)
# MBITES$lifespan <- Bionomics_lifespan(mosquitos)

# number of blood hosts
sumstat$blood_hosts <- Bionomics_humanBloodHostCpp(mosquitos,who = "human")
# MBITES$blood_hosts <- Bionomics_humanBloodHost(mosquitos,who = "human")

# feeding interval
sumstat$blood_interval <- Bionomics_bloodIntervalsCpp(mosquitos,who = "human")
sumstat$blood_interval <- unlist(sumstat$blood_interval)
sumstat$rest_interval <- Bionomics_restIntervalsCpp(mosquitos)
sumstat$rest_interval <- unlist(sumstat$rest_interval)
# MBITES$blood_interval <- Bionomics_bloodIntervals(mosquitos,who = "human")

# hbr (human biting proportion)
sumstat$human_biterate <- Bionomics_humanBitingProportionCpp(mosquitos)
# MBITES$human_biterate <- Bionomics_humanBitingProportion(mosquitos)

# blood feeding rate
sumstat$blood_rate <- Bionomics_bloodfeedingRateCpp(mosquitos)
sumstat$blood_rate <- unlist(sumstat$blood_rate)
# MBITES$blood_rate <- Bionomics_bloodfeedingRate(mosquitos) # R version seems wrong (confirmed)

# lifetime egg production
sumstat$life_egg <- Bionomics_lifetimeOvipositionCpp(mosquitos,dist)
# MBITES$life_egg <- Bionomics_lifetimeOviposition(mosquitos)

# oviposition interval
sumstat$egg_interval <- Bionomics_ovipositionIntervalCpp(mosquitos)
# MBITES$egg_interval <- Bionomics_ovipositionInterval(mosquitos)

# oviposition rate
sumstat$egg_rate <- Bionomics_ovipositionRateCpp(mosquitos)
# MBITES$egg_rate <- Bionomics_ovipositionRate(mosquitos) # R version seems wrong (confirmed)

# vectorial capacity
sumstat$VC <- Bionomics_vectorialCapacityCpp(mosquitos,dist,nrow(humans),EIP = 10,unique = F)
# MBITES$VC <- Bionomics_vectorialCapacity(mosquitos,humans,EIP = 10)

# vectorial capacity (unique secondary hosts)
sumstat$VC_unique <- Bionomics_vectorialCapacityCpp(mosquitos,dist,nrow(humans),EIP = 10,unique = T)
# MBITES$VC_unique <- Bionomics_UniqueBites(mosquitos,humans,EIP = 10)

# dispersion plots
vc_dispersion <- smooth_kernels(distances = sumstat$VC$dispersion)
vc_dispersion_unique <- smooth_kernels(distances = sumstat$VC_unique$dispersion)
egg_dispersion <- smooth_kernels(distances = sumstat$life_egg$dispersion)

# VC dispersion

# mar <- par()$mar
#
# # plot
# red <- "firebrick3"
# blue <- "steelblue"
# redb <- adjustcolor(red,alpha.f = 0.75)
# redf <- adjustcolor(red,alpha.f = 0.5)
# blueb <- adjustcolor(blue,alpha.f = 0.75)
# bluef <- adjustcolor(blue,alpha.f = 0.5)
# with(vc_dispersion,{
#
#   par(mfrow = c(1,2))  # Leave space for z axis
#
#   # ECDF/PMF
#   plot(knots, ecdf(knots),type="l",col=redb,lwd=3,
#        ylab="CDF",xlab="Distance",main="Secondary Bites Dispersion (empirical)")
#   polygon(x = c(knots,max(knots),0),y = c(ecdf(knots),min(ecdf(knots)),min(ecdf(knots))),border = NA,col = redf)
#   par(new = TRUE)
#   plot(knots, pmf, type = "h",col=blueb,lwd=3,
#        axes = FALSE, bty = "n", xlab = "", ylab = "")
#   polygon(x = c(0,knots,max(knots)),y = c(0,pmf,0),border = NA,col = blueb)
#   axis(side=4, at = pretty(range(pmf)))
#   mtext("PDF", side=4, line=3)
#
#   # CDF/PDF
#   plot(knots, cdf$est,type="l",col=redb,lwd=3,
#        ylab="CDF",xlab="Distance",main="Secondary Bites Dispersion (smoothed)")
#   polygon(x = c(knots,max(knots),0),y = c(cdf$est,min(cdf$est),min(cdf$est)),border = NA,col = redf)
#   par(new = TRUE)
#   plot(knots, pdf$est, type = "l",col=blueb,lwd=3,
#        axes = FALSE, bty = "n", xlab = "", ylab = "")
#   polygon(x = c(0,knots,max(knots)),y = c(0,pdf$est,0),border = NA,col = blueb)
#   axis(side=4, at = pretty(range(pdf$est)))
#   mtext("PDF", side=4, line=3)
#
#   par(mfrow=c(1,1))
# })
