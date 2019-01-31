###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Figure - Stability Index & VC at 0,50,100% peri-domestic breeding
#     MBITES Team
#     February 2019
#
###############################################################################


###############################################################################
# load data
###############################################################################

rm(list=ls());gc()
library(here)

gpars <- par()

sumstat <- readRDS(file = "/Users/slwu89/Desktop/mbites/peridomIHME/analyzed/summary.rds")
simtime <- 5*365 # simulation time (used to normalize VC metric to a daily rate)


###############################################################################
# generate plots
###############################################################################

# output file
pdf(file = here("figures/StabilityVC.pdf"),width = 14,height = 8)

lscapes <- c(1,13,26)
titles <- paste0(c(0,50,100),"% Peri-domestic Breeding")

# helper functions

# xlims for all plots should scale to largest: VC plot
xup_vc <- max(sapply(sumstat[lscapes],function(x){
  vc <- x$VC$VC/length(x$VC$VC)
  vc <- vc / simtime
  max(vc)
}))

# xlims for all plots should scale to largest: human blood hosts plot
xup_bh <- max(sapply(sumstat[lscapes],function(x){
  max(x$blood_hosts)
}))

par(mfrow=c(2,3))

# VC plots
for(i in 1:length(lscapes)){
  
  vc_normalized <- sumstat[[lscapes[i]]]$VC$VC / length(sumstat[[lscapes[i]]]$VC$VC)
  vc_normalized <- vc_normalized / simtime # normalize by time
  vc_max <- max(vc_normalized)
  vc_mean_norm <- mean(vc_normalized)
  vc_mean_norm_r <- round(vc_mean_norm,2)
  if(abs(vc_mean_norm_r) < .Machine$double.eps^0.5){
    vc_mean_norm_r <- round(vc_mean_norm,3)
  }
  vc <- hist(vc_normalized,probability = TRUE,breaks = 100,
             col = adjustcolor("firebrick3",alpha.f = 0.5),
             xlab = "Secondary Bites", ylab = "Density",
             main = paste0(titles[i],"\nVectorial Capacity (mean: ",vc_mean_norm_r,")"),
             xlim = c(0,xup_vc),
             cex.lab = 1.15)
  abline(v = vc_mean_norm,lwd=2.5,lty=2,col="firebrick3")
  abline(v = vc_max,lwd=2.5,lty=2,col=adjustcolor("steelblue",alpha.f = 0.5))
  # text(x = vc_max,y=max(vc$density)*0.1,paste0("max: ",round(vc_max,2)),
  #      col=adjustcolor("steelblue",alpha.f = 0.75),adj=1.15)
  
}

# human blood host plots
for(i in 1:length(lscapes)){
  
  blood_hosts_mean <- mean(sumstat[[lscapes[i]]]$blood_hosts)
  hbh <- hist(sumstat[[lscapes[i]]]$blood_hosts,probability = TRUE,breaks = 100,
              col = adjustcolor("firebrick3",alpha.f = 0.5),
              xlab = "Number of Blood Hosts", ylab = "Density",
              main = paste0("Human Blood Hosts (mean: ",round(blood_hosts_mean,2),")"),
              xlim = c(0,xup_bh),
              cex.lab = 1.15)
  abline(v = blood_hosts_mean,lwd=2.5,lty=2,col="firebrick3")
  abline(v = max(sumstat[[lscapes[i]]]$blood_hosts),lwd=2.5,lty=2,col=adjustcolor("steelblue",alpha.f = 0.5))
  text(x = max(sumstat[[lscapes[i]]]$blood_hosts),y=max(hbh$density)*0.1,paste0("max: ",max(sumstat[[lscapes[i]]]$blood_hosts)),
       col=adjustcolor("steelblue",alpha.f = 0.75),adj=1.15)
  
}

dev.off()
par(gpars)