###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Figure - Bionomics as fn. of peri-domestic breeding
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
lf_means <- rep(0,max)
nbh_means <- rep(0,max)
fc_means <- rep(0,max)
br_means <- rep(0,max)
# abs_d_means <- rep(0,max)
# cum_d_means <- rep(0,max)

# hold the quantiles
lf_quant <- vector("list",max)
nbh_quant <- vector("list",max)
fc_quant <- vector("list",max)
br_quant <- vector("list",max)
# abs_d_quant <- vector("list",max)
# cum_d_quant <- vector("list",max)

# probabilities for quantiles
q_probs <- c(0.2,0.25,0.5,0.75,0.8)

# iterate through all runs
pb <- txtProgressBar(1,max)
for(i in 1:max){
  
  run <- as.character(i)
  
  # get means
  lf_cdf <- ecdf(sumstat[[i]]$lifespan)
  lf_tt <- seq(from=0,to=max(sumstat[[i]]$lifespan),by=0.25)
  lf_surv <- 1 - lf_cdf(lf_tt)
  lf_means[i] <- weighted.mean(lf_tt,lf_surv)
  nbh_means[i] <- mean(sumstat[[i]]$blood_hosts)
  fc_means[i] <- mean(sumstat[[i]]$blood_interval)
  br_means[i] <- mean(sumstat[[i]]$blood_rate)
  
  # # means of dispersion
  # abs_d_means[i] <- mean(sumstat[[i]]$disperse_abs)
  # cum_d_means[i] <- mean(sumstat[[i]]$disperse_cum)
  
  # get quantiles
  lf_quant[[i]] <- wtd.quantile(lf_tt,lf_surv,probs = q_probs)
  nbh_quant[[i]] <- quantile(sumstat[[i]]$blood_hosts,probs = q_probs)
  fc_quant[[i]] <- quantile(sumstat[[i]]$blood_interval,probs = q_probs)
  br_quant[[i]] <- quantile(sumstat[[i]]$blood_rate,probs = q_probs)
  
  # # quantiles of dispersion
  # abs_d_quant[[i]] <- quantile(sumstat[[i]]$disperse_abs,probs = q_probs)
  # cum_d_quant[[i]] <- quantile(sumstat[[i]]$disperse_cum,probs = q_probs)
  
  setTxtProgressBar(pb,i)
}


###############################################################################
# make plot
###############################################################################

# plot parameters
cols <- ggcol(n = 4)
pdf(file = here("figures/BionomicsPeridom.pdf"),width = 12,height = 10)
par(mar = c(4.5, 4.5, 2, 4.5),mfrow=c(2,2))

xaxt <- floor(seq(from=1,to=26,length.out = 5))
xaxt_lab <- seq(from=0,to=1,length.out = 5)

# lifespans
lf_ylim <- c(min(unlist(lf_quant)),max(unlist(lf_quant)))
lf_25 <- sapply(lf_quant,function(x){x[["25%"]]})
lf_75 <- sapply(lf_quant,function(x){x[["75%"]]})
plot(lf_means,
     type="l", xaxt = "n", yaxt = "n",ylab = "", xlab = "",ylim = lf_ylim
)
polygon(
  x = c(1:26,26:1),
  y = c(lf_75,rev(lf_25)),
  border = NA,col = adjustcolor(cols[1],0.5)
)
lines(x = 1:26,
      y = lf_means,
      col = adjustcolor(cols[1],1),lwd=2)
lines(x = 1:26,
      y = sapply(lf_quant,function(x){x[["50%"]]}),
      col = adjustcolor(cols[1],1),lwd=2,lty=2)
axis(side=2, at = pretty(range(c(0,lf_means,lf_ylim)),n = 8))
axis(side = 1,at = xaxt,labels = xaxt_lab)
mtext("Lifespan", side = 3, line = 0.5,cex=1.25)
mtext("Percent peri-domestic", side = 1, line = 2.5)
mtext("Time (days)", side = 2, line = 2.25)


# number of blood hosts
nbh_ylim <- c(min(unlist(nbh_quant)),max(unlist(nbh_quant)))
nbh_25 <- sapply(nbh_quant,function(x){x[["25%"]]})
nbh_75 <- sapply(nbh_quant,function(x){x[["75%"]]})
plot(nbh_means,
     type="l", xaxt = "n", yaxt = "n",ylab = "", xlab = "",ylim = nbh_ylim
)
polygon(
  x = c(1:26,26:1),
  y = c(nbh_75,rev(nbh_25)),
  border = NA,col = adjustcolor(cols[2],0.5)
)
lines(x = 1:26,
      y = nbh_means,
      col = adjustcolor(cols[2],1),lwd=2)
lines(x = 1:26,
      y = sapply(nbh_quant,function(x){x[["50%"]]}),
      col = adjustcolor(cols[2],1),lwd=2,lty=2)
axis(side=2, at = pretty(range(c(0,nbh_means,nbh_ylim)),n = 8))
axis(side = 1,at = xaxt,labels = xaxt_lab)
mtext("Number Blood Hosts", side = 3, line = 0.5,cex=1.25)
mtext("Percent peri-domestic", side = 1, line = 2.5)
mtext("Host Count", side = 2, line = 2.25)


# feeding cycle
fc_ylim <- c(min(unlist(fc_quant)),max(unlist(fc_quant)))
fc_25 <- sapply(fc_quant,function(x){x[["25%"]]})
fc_75 <- sapply(fc_quant,function(x){x[["75%"]]})
plot(fc_means,
     type="l", xaxt = "n", yaxt = "n",ylab = "", xlab = "",ylim = fc_ylim
)
polygon(
  x = c(1:26,26:1),
  y = c(fc_75,rev(fc_25)),
  border = NA,col = adjustcolor(cols[3],0.5)
)
lines(x = 1:26,
      y = fc_means,
      col = adjustcolor(cols[3],1),lwd=2)
lines(x = 1:26,
      y = sapply(fc_quant,function(x){x[["50%"]]}),
      col = adjustcolor(cols[3],1),lwd=2,lty=2)
axis(side=2, at = pretty(range(c(0,fc_means,fc_ylim)),n = 8))
axis(side = 1,at = xaxt,labels = xaxt_lab)
mtext("Length of Feeding Cycle", side = 3, line = 0.5,cex=1.25)
mtext("Percent peri-domestic", side = 1, line = 2.5)
mtext("Time (days)", side = 2, line = 2.25)


# blood feeding rate
br_ylim <- c(min(unlist(br_quant)),max(unlist(br_quant)))
br_25 <- sapply(br_quant,function(x){x[["25%"]]})
br_75 <- sapply(br_quant,function(x){x[["75%"]]})
plot(br_means,
     type="l", xaxt = "n", yaxt = "n",ylab = "", xlab = "",ylim = br_ylim
)
polygon(
  x = c(1:26,26:1),
  y = c(br_75,rev(br_25)),
  border = NA,col = adjustcolor(cols[4],0.5)
)
lines(x = 1:26,
      y = br_means,
      col = adjustcolor(cols[4],1),lwd=2)
lines(x = 1:26,
      y = sapply(br_quant,function(x){x[["50%"]]}),
      col = adjustcolor(cols[4],1),lwd=2,lty=2)
axis(side=2, at = pretty(range(c(0,br_means,br_ylim)),n = 8))
axis(side = 1,at = xaxt,labels = xaxt_lab)
mtext("Blood Feeding by Age", side = 3, line = 0.5,cex=1.25)
mtext("Percent peri-domestic", side = 1, line = 2.5)
mtext("Age (days)", side = 2, line = 2.25)


# # absolute dispersion
# abs_d_ylim <- c(min(c(unlist(abs_d_quant)),abs_d_means),max(c(abs_d_means,unlist(abs_d_quant))))
# abs_d_25 <- sapply(abs_d_quant,function(x){x[["25%"]]})
# abs_d_75 <- sapply(abs_d_quant,function(x){x[["75%"]]})
# plot(abs_d_means,
#      type="l", xaxt = "n", yaxt = "n",ylab = "", xlab = "",ylim = abs_d_ylim
# )
# polygon(
#   x = c(1:26,26:1),
#   y = c(abs_d_75,rev(abs_d_25)),
#   border = NA,col = adjustcolor(cols[5],0.5)
# )
# lines(x = 1:26,
#       y = abs_d_means,
#       col = adjustcolor(cols[5],1),lwd=2)
# lines(x = 1:26,
#       y = sapply(abs_d_quant,function(x){x[["50%"]]}),
#       col = adjustcolor(cols[5],1),lwd=2,lty=2)
# axis(side=2, at = pretty(range(c(0,abs_d_means,abs_d_ylim)),n = 8))
# axis(side = 1,at = xaxt,labels = xaxt_lab)
# mtext("Lifetime Displacement", side = 3, line = 0.5,cex=1.25)
# mtext("Percent peri-domestic", side = 1, line = 2.5)
# mtext("Distance", side = 2, line = 2.25)
# 
# 
# # cumulative dispersion
# cum_d_ylim <- c(min(c(unlist(cum_d_quant)),cum_d_means),max(c(cum_d_means,unlist(cum_d_quant))))
# cum_d_25 <- sapply(cum_d_quant,function(x){x[["25%"]]})
# cum_d_75 <- sapply(cum_d_quant,function(x){x[["75%"]]})
# plot(cum_d_means,
#      type="l", xaxt = "n", yaxt = "n",ylab = "", xlab = "",ylim = cum_d_ylim
# )
# polygon(
#   x = c(1:26,26:1),
#   y = c(cum_d_75,rev(cum_d_25)),
#   border = NA,col = adjustcolor(cols[6],0.5)
# )
# lines(x = 1:26,
#       y = cum_d_means,
#       col = adjustcolor(cols[6],1),lwd=2)
# lines(x = 1:26,
#       y = sapply(cum_d_quant,function(x){x[["50%"]]}),
#       col = adjustcolor(cols[6],1),lwd=2,lty=2)
# axis(side=2, at = pretty(range(c(0,cum_d_means,cum_d_ylim)),n = 8))
# axis(side = 1,at = xaxt,labels = xaxt_lab)
# mtext("Cumulative Movement", side = 3, line = 0.5,cex=1.25)
# mtext("Percent peri-domestic", side = 1, line = 2.5)
# mtext("Distance", side = 2, line = 2.25)

par(gpars)
dev.off()