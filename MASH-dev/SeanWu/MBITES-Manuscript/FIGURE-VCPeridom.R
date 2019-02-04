###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Figure - VC & VC dispersion as fn. of peri-domestic breeding
#     MBITES Team
#     February 2019
#
###############################################################################


###############################################################################
# load data
###############################################################################

rm(list=ls());gc()
library(here)
library(Hmisc)

gpars <- par()

sumstat <- readRDS(file = "/Users/slwu89/Desktop/mbites/peridomIHME/analyzed/summary.rds")
simtime <- 5*365 # simulation time (used to normalize VC metric to a daily rate)

# generate a color scheme similar to ggplot2's default categorical color scheme
ggcol <- function(n, alpha = 1) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alpha)[1:n]
}


###############################################################################
# process quantiles
###############################################################################

# hold the means
max <- 26
vc_means <- rep(0,max)
spatialvc_means <- rep(0,max)
abs_d_means <- rep(0,max)
cum_d_means <- rep(0,max)

# hold the quantiles
vc_quant <- vector("list",max)
spatialvc_quant <- vector("list",max)
abs_d_quant <- vector("list",max)
cum_d_quant <- vector("list",max)

# probabilities for quantiles
q_probs <- c(0.25,0.5,0.75)

# iterate through all runs
pb <- txtProgressBar(1,max)
for(i in 1:max){

  run <- as.character(i)

  # vc correction
  vc_normalized <- sumstat[[i]]$VC$VC / length(sumstat[[i]]$VC$VC)
  vc_normalized <- vc_normalized / simtime # normalize by time
  vc_mean_norm <- mean(vc_normalized)
  vc_means[i] <- vc_mean_norm

  # spatial vc
  spatialvc_means[i] <- mean(sumstat[[i]]$VC$dispersion)
  
  # means of dispersion
  abs_d_means[i] <- mean(sumstat[[i]]$disperse_abs)
  cum_d_means[i] <- mean(sumstat[[i]]$disperse_cum)

  # vc
  vc_quant[[i]] <- quantile(vc_normalized,probs = q_probs)

  # spatial vc
  spatialvc_quant[[i]] <- quantile(sumstat[[i]]$VC$dispersion, probs = q_probs)
  
  # quantiles of dispersion
  abs_d_quant[[i]] <- quantile(sumstat[[i]]$disperse_abs,probs = q_probs)
  cum_d_quant[[i]] <- quantile(sumstat[[i]]$disperse_cum,probs = q_probs)

  setTxtProgressBar(pb,i)
}


###############################################################################
# make plot
###############################################################################

cols <- ggcol(n = 4)
pdf(file = here("figures/VCPeridom.pdf"),width = 12,height = 10)
par(mar = c(4.5, 4.5, 2, 4.5),mfrow=c(2,2))

xaxt <- floor(seq(from=1,to=26,length.out = 5))
xaxt_lab <- seq(from=0,to=1,length.out = 5)

# mean VC
vc_ylim <- c(min(c(unlist(vc_quant)),vc_means),max(c(vc_means,unlist(vc_quant))))
vc_25 <- sapply(vc_quant,function(x){x[["25%"]]})
vc_75 <- sapply(vc_quant,function(x){x[["75%"]]})
plot(vc_means,
     type="l", xaxt = "n", yaxt = "n",ylab = "", xlab = "",ylim = vc_ylim
)
polygon(
  x = c(1:26,26:1),
  y = c(vc_75,rev(vc_25)),
  border = NA,col = adjustcolor(cols[1],0.5)
)
lines(x = 1:26,
      y = vc_means,
      col = adjustcolor(cols[1],1),lwd=2)
lines(x = 1:26,
      y = sapply(vc_quant,function(x){x[["50%"]]}),
      col = adjustcolor(cols[1],1),lwd=2,lty=2)
axis(side=2, at = pretty(range(c(0,vc_means,vc_ylim)),n = 8))
axis(side = 1,at = xaxt,labels = xaxt_lab)
mtext("Vectorial Capacity", side = 3, line = 0.5,cex=1.25)
mtext("Percent peri-domestic", side = 1, line = 2.5)
mtext("Number of Secondary Bites", side = 2, line = 2.25)


# mean dispersion of VC
vc_s_ylim <- c(min(c(unlist(spatialvc_quant)),spatialvc_means),max(c(spatialvc_means,unlist(spatialvc_quant))))
vc_s_25 <- sapply(spatialvc_quant,function(x){x[["25%"]]})
vc_s_75 <- sapply(spatialvc_quant,function(x){x[["75%"]]})
plot(spatialvc_means,
     type="l", xaxt = "n", yaxt = "n",ylab = "", xlab = "",ylim = vc_s_ylim
)
polygon(
  x = c(1:26,26:1),
  y = c(vc_s_75,rev(vc_s_25)),
  border = NA,col = adjustcolor(cols[2],0.5)
)
lines(x = 1:26,
      y = spatialvc_means,
      col = adjustcolor(cols[2],1),lwd=2)
lines(x = 1:26,
      y = sapply(spatialvc_quant,function(x){x[["50%"]]}),
      col = adjustcolor(cols[2],1),lwd=2,lty=2)
axis(side=2, at = pretty(range(c(0,spatialvc_means,vc_s_ylim)),n = 8))
axis(side = 1,at = xaxt,labels = xaxt_lab)
mtext("Spatial Disperion of Vectorial Capacity", side = 3, line = 0.5,cex=1.25)
mtext("Percent peri-domestic", side = 1, line = 2.5)
mtext("Distance", side = 2, line = 2.25)

# absolute dispersion
abs_d_ylim <- c(min(c(unlist(abs_d_quant)),abs_d_means),max(c(abs_d_means,unlist(abs_d_quant))))
abs_d_25 <- sapply(abs_d_quant,function(x){x[["25%"]]})
abs_d_75 <- sapply(abs_d_quant,function(x){x[["75%"]]})
plot(abs_d_means,
     type="l", xaxt = "n", yaxt = "n",ylab = "", xlab = "",ylim = abs_d_ylim
)
polygon(
  x = c(1:26,26:1),
  y = c(abs_d_75,rev(abs_d_25)),
  border = NA,col = adjustcolor(cols[3],0.5)
)
lines(x = 1:26,
      y = abs_d_means,
      col = adjustcolor(cols[3],1),lwd=2)
lines(x = 1:26,
      y = sapply(abs_d_quant,function(x){x[["50%"]]}),
      col = adjustcolor(cols[3],1),lwd=2,lty=2)
axis(side=2, at = pretty(range(c(0,abs_d_means,abs_d_ylim)),n = 8))
axis(side = 1,at = xaxt,labels = xaxt_lab)
mtext("Lifetime Displacement", side = 3, line = 0.5,cex=1.25)
mtext("Percent peri-domestic", side = 1, line = 2.5)
mtext("Distance", side = 2, line = 2.25)


# cumulative dispersion
cum_d_ylim <- c(min(c(unlist(cum_d_quant)),cum_d_means),max(c(cum_d_means,unlist(cum_d_quant))))
cum_d_25 <- sapply(cum_d_quant,function(x){x[["25%"]]})
cum_d_75 <- sapply(cum_d_quant,function(x){x[["75%"]]})
plot(cum_d_means,
     type="l", xaxt = "n", yaxt = "n",ylab = "", xlab = "",ylim = cum_d_ylim
)
polygon(
  x = c(1:26,26:1),
  y = c(cum_d_75,rev(cum_d_25)),
  border = NA,col = adjustcolor(cols[4],0.5)
)
lines(x = 1:26,
      y = cum_d_means,
      col = adjustcolor(cols[4],1),lwd=2)
lines(x = 1:26,
      y = sapply(cum_d_quant,function(x){x[["50%"]]}),
      col = adjustcolor(cols[4],1),lwd=2,lty=2)
axis(side=2, at = pretty(range(c(0,cum_d_means,cum_d_ylim)),n = 8))
axis(side = 1,at = xaxt,labels = xaxt_lab)
mtext("Cumulative Movement", side = 3, line = 0.5,cex=1.25)
mtext("Percent peri-domestic", side = 1, line = 2.5)
mtext("Distance", side = 2, line = 2.25)

par(gpars)
dev.off()