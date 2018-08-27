###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Analysis of peri-domestic simulation experiments
#     MBITES Team
#     August 2018
#
###############################################################################

rm(list = ls());gc()
library(MBITES)
library(viridis)

###############################################################################
# load processed bionomics
###############################################################################

# where the files can be found
directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/finals/"

run <- "26"
MBITES <- readRDS(paste0(directory,"/analysis",run,".rds"))





par(mfrow=c(1,1))


with(MBITES,{
  plot(surv_tt, surv_s,
       type = "l", xlab = "Age (Days)", ylab = "Density",
       main=paste0("MBITES Survival Function (mean: ",round(surv_mean,2),")"),
       lwd=2,col="firebrick3",xlim=c(0,30))
  polygon(c(0,surv_tt), c(0,surv_s),
          border=NA, col=adjustcolor("firebrick3",alpha.f = 0.5))
  abline(v = surv_mean,lwd=2.5,lty=2,col="firebrick3")
})


colors <- viridis(n = 26,alpha = 0.8)

run <- "1"
MBITES <- readRDS(paste0(directory,"/analysis",run,".rds"))

with(MBITES,{
  plot(surv_tt, surv_s,
       type = "l", xlab = "Age (Days)", ylab = "Density",
       main=paste0("MBITES Survival Function"),
       lwd=2,col=colors[1],xlim=c(0,30))
  abline(v = surv_mean,lwd=2.5,lty=2,col=colors[1])
})

for(i in 2:26){
  cat("i: ",i,"\n")
  run <- as.character(i)
  MBITES <- readRDS(paste0(directory,"/analysis",run,".rds"))
  with(MBITES,{
    lines(surv_tt, surv_s,
         lwd=2,col=colors[as.integer(i)])
    abline(v = surv_mean,lwd=2.5,lty=2,col=colors[as.integer(i)])
  })

}
