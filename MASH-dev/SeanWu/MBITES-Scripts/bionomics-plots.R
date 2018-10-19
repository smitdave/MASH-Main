###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Plot Bionomics for peri-domestic experiments
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
analysis_dir <- "/Users/slwu89/Desktop/mbites/peridomIHME/analyzed/"
out_dir <- "/Users/slwu89/Desktop/mbites/peridomIHME/finals/"
plots_dir <- "/Users/slwu89/Desktop/mbites/peridomIHME/plots/"

sumstat <- readRDS(file = paste0(analysis_dir,"summary.rds"))
simtime <- 5*365 # simulation time (used to normalize VC metric to a daily rate)

margins <- par()$mar # default margins

ggcol <- function(n, alpha = 1) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alpha)[1:n]
}

###############################################################################
# Figure 4 (landscape)
###############################################################################






###############################################################################
# Figure 6 (VC and num blood hosts for a landscape)
###############################################################################

lscapes <- c(1,13,26)

pdf(file = paste0(plots_dir,"MBITES_fig6.pdf"),width = 12,height = 8)
par(mfrow=c(3,2))
for(i in lscapes){

  run <- as.character(i)

  # how 2 plot vc
  with(sumstat[[i]],{
    vc_normalized <- VC$VC / length(VC$VC)
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
               main = paste0("MBITES Vectorial Capacity (mean: ",vc_mean_norm_r,")"))
    abline(v = vc_mean_norm,lwd=2.5,lty=2,col="firebrick3")
    abline(v = vc_max,lwd=2.5,lty=2,col=adjustcolor("steelblue",alpha.f = 0.5))
    text(x = vc_max,y=max(vc$density)*0.1,paste0("max: ",round(vc_max,2)),
         col=adjustcolor("steelblue",alpha.f = 0.75),adj=1.15)
  })

  # num blood hosts
  with(sumstat[[i]],{
    blood_hosts_mean <- mean(blood_hosts)
    hbh <- hist(blood_hosts,probability = TRUE,breaks = 100,
                col = adjustcolor("firebrick3",alpha.f = 0.5),
                xlab = "Number of Blood Hosts", ylab = "Density",
                main = paste0("MBITES Human Blood Hosts (mean: ",round(blood_hosts_mean,2),")"))
    abline(v = blood_hosts_mean,lwd=2.5,lty=2,col="firebrick3")
    abline(v = max(blood_hosts),lwd=2.5,lty=2,col=adjustcolor("steelblue",alpha.f = 0.5))
    text(x = max(blood_hosts),y=max(hbh$density)*0.1,paste0("max: ",max(blood_hosts)),
         col=adjustcolor("steelblue",alpha.f = 0.75),adj=1.15)
  })

}
dev.off()
par(mfrow=c(1,1))


###############################################################################
# Figure 7 (dispersion for landscape 13)
###############################################################################

l <- 13
mvmt <- readRDS(paste0(out_dir,"processed_kernels_",l,".rds"))


pdf(file = paste0(plots_dir,"MBITES_fig7.pdf"),width = 12,height = 10)

# Plot average movement for landscape

# PDF
avg_k_m <- weighted.mean(mvmt$CDF_sth$x.out,mvmt$PDF_sth$y)
xlim <- c(0,5)
par(mar = c(4.5, 4.5, 2.5, 4.5),mfrow=c(2,2))
plot(c(0,mvmt$CDF_sth$x.out,max(mvmt$CDF_sth$x.out),0),
     c(0,mvmt$PDF_sth$y,0,0),
     type="n",xlab="Distance",ylab="Density",xlim=xlim
)
polygon(x = c(0,mvmt$CDF_sth$x.out,max(mvmt$CDF_sth$x.out),0),
        y = c(0,mvmt$PDF_sth$y,0,0),
        border = NA,
        col = adjustcolor("mediumblue",alpha.f = 0.6))
mtext(paste0("Spatially Averaged Movement Kernel (mean: ",round(avg_k_m,3),")"),side = 3,line = 0.5,cex=1.25)
par(new = TRUE)

# CDF
plot(c(0,mvmt$CDF_sth$x.out,max(mvmt$CDF_sth$x.out),0),
     c(0,mvmt$CDF_sth$est,0,0),
     type="n", xaxt = "n", yaxt = "n",ylab = "", xlab = "",xlim=xlim
     )
lines(x = c(0,mvmt$CDF_sth$x.out),
      y = c(0,mvmt$CDF_sth$est),
      col = adjustcolor("firebrick3",1),lwd=2)
axis(side=4, at = pretty(range(c(0,mvmt$CDF_sth$est))))
mtext("Cumulative Probability", side = 4, line = 2.25)
# par(mar = c(5,4,2,2),mfrow=c(1,1)) # defaults

# plot mosquito dispersion

# cumulative dispersion
maxx <- max(sumstat[[l]]$disperse_cum_smooth$knots)+(0.001*max(sumstat[[l]]$disperse_cum_smooth$knots))
cum_disp_m <- weighted.mean(x = sumstat[[l]]$disperse_cum_smooth$knots,w = sumstat[[l]]$disperse_cum_smooth$pmf)
# par(mar = c(4.5, 4.5, 2.5, 4.5),mfrow=c(1,2))
plot(sumstat[[l]]$disperse_cum_smooth$knots,
     sumstat[[l]]$disperse_cum_smooth$pdf$est,
     type="n",xlab="Distance",ylab="Density"
     )
mtext(paste0("Cumulative Dispersion (mean: ",round(cum_disp_m,3),")"),side = 3,line = 0.5,cex=1.25)
# PDF
polygon(x = c(0,sumstat[[l]]$disperse_cum_smooth$knots,maxx),
        y = c(0,sumstat[[l]]$disperse_cum_smooth$pdf$est,0),border = NA,col = adjustcolor("mediumblue",alpha.f = 0.6))
par(new = TRUE)

# CDF
plot(sumstat[[l]]$disperse_cum_smooth$knots,
     sumstat[[l]]$disperse_cum_smooth$cdf$est,
     type="n", xaxt = "n", yaxt = "n",ylab = "", xlab = "")
lines(x = sumstat[[l]]$disperse_cum_smooth$knots,
        y = sumstat[[l]]$disperse_cum_smooth$cdf$est,
        col = adjustcolor("firebrick3",1),lwd=2)
axis(side=4, at = pretty(range(sumstat[[l]]$disperse_cum_smooth$cdf$est)))
mtext("Cumulative Probability", side = 4, line = 2.25)
# par(mar = c(5,4,2,2)) # defaults

# absolute dispersion
maxx <- max(sumstat[[l]]$disperse_abs_smooth$knots)+(0.001*max(sumstat[[l]]$disperse_abs_smooth$knots))
abs_disp_m <- weighted.mean(x = sumstat[[l]]$disperse_abs_smooth$knots,w = sumstat[[l]]$disperse_abs_smooth$pmf)
# par(mar = c(4.5, 4.5, 2.5, 4.5))
plot(c(0,sumstat[[l]]$disperse_abs_smooth$knots),
     c(0,ifelse(sumstat[[l]]$disperse_abs_smooth$pdf$est<0,0,sumstat[[l]]$disperse_abs_smooth$pdf$est)),
     type="n",xlab="Distance",ylab="Density"
)
mtext(paste0("Absolute Dispersion (mean: ",round(abs_disp_m,3),")"),side = 3,line = 0.5,cex=1.25)
# PDF
polygon(x = c(0,sumstat[[l]]$disperse_abs_smooth$knots,maxx),
        y = c(0,ifelse(sumstat[[l]]$disperse_abs_smooth$pdf$est<0,0,sumstat[[l]]$disperse_abs_smooth$pdf$est),0),border = NA,col = adjustcolor("mediumblue",alpha.f = 0.6))
par(new = TRUE)

# CDF
plot(sumstat[[l]]$disperse_abs_smooth$knots,
     sumstat[[l]]$disperse_abs_smooth$cdf$est,
     type="n", xaxt = "n", yaxt = "n",ylab = "", xlab = "")
lines(x = c(sumstat[[l]]$disperse_abs_smooth$knots),
      y = c(sumstat[[l]]$disperse_abs_smooth$cdf$est),
      col = adjustcolor("firebrick3",1),lwd=2)
axis(side=4, at = pretty(range(c(0,sumstat[[l]]$disperse_abs_smooth$cdf$est))))
mtext("Cumulative Probability", side = 4, line = 2.25)
# par(mar = c(5,4,2,2),mfrow=c(1,1)) # defaults

# plot VC dispersion
maxx <- max(sumstat[[l]]$vc_dispersion$knots)+(0.001*max(sumstat[[l]]$vc_dispersion$knots))
vc_disp_m <- weighted.mean(x = sumstat[[l]]$vc_dispersion$knots,w = sumstat[[l]]$vc_dispersion$pmf)
# par(mar = c(4.5, 4.5, 2.5, 4.5),mfrow=c(1,2))
plot(sumstat[[l]]$vc_dispersion$knots,
     sumstat[[l]]$vc_dispersion$pdf$est,
     type="n",xlab="Distance",ylab="Density"
)
mtext(paste0("VC Dispersion (mean: ",round(vc_disp_m,3),")"),side = 3,line = 0.5,cex=1.25)
# PDF
polygon(x = c(0,sumstat[[l]]$vc_dispersion$knots,maxx),
        y = c(0,sumstat[[l]]$vc_dispersion$pdf$est,0),border = NA,col = adjustcolor("mediumblue",alpha.f = 0.6))
par(new = TRUE)

# CDF
plot(sumstat[[l]]$vc_dispersion$knots,
     sumstat[[l]]$vc_dispersion$cdf$est,
     type="n", xaxt = "n", yaxt = "n",ylab = "", xlab = "")
lines(x = sumstat[[l]]$vc_dispersion$knots,
      y = sumstat[[l]]$vc_dispersion$cdf$est,
      col = adjustcolor("firebrick3",1),lwd=2)
axis(side=4, at = pretty(range(sumstat[[l]]$vc_dispersion$cdf$est)))
mtext("Cumulative Probability", side = 4, line = 2.25)
# par(mar = c(5,4,2,2)) # defaults

# # VC dispersion (unique bites)
# maxx <- max(sumstat[[l]]$vc_dispersion_unique$knots)+(0.001*max(sumstat[[l]]$vc_dispersion_unique$knots))
# vc_u_disp_m <- weighted.mean(x = sumstat[[l]]$vc_dispersion_unique$knots,w = sumstat[[l]]$vc_dispersion_unique$pmf)
# par(mar = c(4.5, 4.5, 2.5, 4.5))
# plot(sumstat[[l]]$vc_dispersion_unique$knots,
#      sumstat[[l]]$vc_dispersion_unique$pdf$est,
#      type="n",xlab="Distance",ylab="Density"
# )
# mtext(paste0("VC (Unique Hosts) Dispersion (mean: ",round(vc_u_disp_m,3),")"),side = 3,line = 0.5,cex=1.25)
# # PDF
# polygon(x = c(0,sumstat[[l]]$vc_dispersion_unique$knots,maxx),
#         y = c(0,sumstat[[l]]$vc_dispersion_unique$pdf$est,0),border = NA,col = adjustcolor("mediumblue",alpha.f = 0.6))
# par(new = TRUE)
#
# # CDF
# plot(sumstat[[l]]$vc_dispersion_unique$knots,
#      sumstat[[l]]$vc_dispersion_unique$cdf$est,
#      type="n", xaxt = "n", yaxt = "n",ylab = "", xlab = "")
# lines(x = sumstat[[l]]$vc_dispersion_unique$knots,
#       y = sumstat[[l]]$vc_dispersion_unique$cdf$est,
#       col = adjustcolor("firebrick3",1),lwd=2)
# axis(side=4, at = pretty(range(sumstat[[l]]$vc_dispersion_unique$cdf$est)))
# mtext("Cumulative Probability", side = 4, line = 2.25)
par(mar = c(5,4,2,2),mfrow=c(1,1)) # defaults
dev.off()


###############################################################################
# Figure 8 (chord diagrams)
###############################################################################

library(circlize)

# loop over all experiments
pdf(file = paste0(plots_dir,"MBITES_fig8.pdf"),width = 12,height = 8)
par(mfrow=c(4,7),mar = rep(0.001,4))

for(i in 1:26){

  cat("plotting run ",i," of 26\n")
  run <- as.character(i)

  # state transitions matrix
  M <- sumstat[[i]]$transitions

  # set up colors
  cols <- adjustcolor(c("firebrick1","firebrick4","mediumorchid3","steelblue1","steelblue4","grey30"),
                      alpha.f = 0.85)
  names(cols) <- c("F","B","R","L","O","D")
  cols_df <- expand.grid(s=rownames(M),d=colnames(M),stringsAsFactors = FALSE)
  cols_df$cols <- cols[cols_df[,"s"]]

  chordDiagramFromMatrix(M,
                         directional = 1,
                         direction.type = "arrows",
                         col = cols_df,
                         grid.col = cols,
                         grid.border = "black",
                         annotationTrack = c("name","grid"),
                         self.link = 2,
                         link.arr.length=0.05,
                         link.arr.lwd=0.5,
                         link.arr.width=0.05,
                         link.arr.col=grey(0.25))

}

par(mar = margins,mfrow=c(1,1))
dev.off()


###############################################################################
# Figure 9 (changes in bionomics over landscapes)
###############################################################################

library(Hmisc)

# hold the means
max <- 26
lf_means <- rep(0,max)
nbh_means <- rep(0,max)
fc_means <- rep(0,max)
br_means <- rep(0,max)
abs_d_means <- rep(0,max)
cum_d_means <- rep(0,max)

# hold the quantiles
lf_quant <- vector("list",max)
nbh_quant <- vector("list",max)
fc_quant <- vector("list",max)
br_quant <- vector("list",max)
abs_d_quant <- vector("list",max)
cum_d_quant <- vector("list",max)

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

  # means of dispersion
  abs_d_means[i] <- mean(sumstat[[i]]$disperse_abs)
  cum_d_means[i] <- mean(sumstat[[i]]$disperse_cum)

  # get quantiles
  lf_quant[[i]] <- wtd.quantile(lf_tt,lf_surv,probs = q_probs)
  nbh_quant[[i]] <- quantile(sumstat[[i]]$blood_hosts,probs = q_probs)
  fc_quant[[i]] <- quantile(sumstat[[i]]$blood_interval,probs = q_probs)
  br_quant[[i]] <- quantile(sumstat[[i]]$blood_rate,probs = q_probs)

  # quantiles of dispersion
  abs_d_quant[[i]] <- quantile(sumstat[[i]]$disperse_abs,probs = q_probs)
  cum_d_quant[[i]] <- quantile(sumstat[[i]]$disperse_cum,probs = q_probs)

  setTxtProgressBar(pb,i)
}


# plot parameters
cols <- ggcol(n = 6)
pdf(file = paste0(plots_dir,"MBITES_fig9.pdf"),width = 12,height = 12)
par(mar = c(4.5, 4.5, 2, 4.5),mfrow=c(3,2))

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
mtext("Lifespan", side = 3, line = 0.5,cex=1.25)
mtext("Percent peri-domestic", side = 1, line = 1.25)
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
mtext("Number Blood Hosts", side = 3, line = 0.5,cex=1.25)
mtext("Percent peri-domestic", side = 1, line = 1.25)
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
mtext("Length of Feeding Cycle", side = 3, line = 0.5,cex=1.25)
mtext("Percent peri-domestic", side = 1, line = 1.25)
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
mtext("Blood Feeding Rate", side = 3, line = 0.5,cex=1.25)
mtext("Percent peri-domestic", side = 1, line = 1.25)
mtext("Time (days)", side = 2, line = 2.25)


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
  border = NA,col = adjustcolor(cols[5],0.5)
)
lines(x = 1:26,
      y = abs_d_means,
      col = adjustcolor(cols[5],1),lwd=2)
lines(x = 1:26,
    y = sapply(abs_d_quant,function(x){x[["50%"]]}),
    col = adjustcolor(cols[5],1),lwd=2,lty=2)
axis(side=2, at = pretty(range(c(0,abs_d_means,abs_d_ylim)),n = 8))
mtext("Absolute Mosquito Dispersion", side = 3, line = 0.5,cex=1.25)
mtext("Percent peri-domestic", side = 1, line = 1.25)
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
  border = NA,col = adjustcolor(cols[6],0.5)
)
lines(x = 1:26,
      y = cum_d_means,
      col = adjustcolor(cols[6],1),lwd=2)
lines(x = 1:26,
    y = sapply(cum_d_quant,function(x){x[["50%"]]}),
    col = adjustcolor(cols[6],1),lwd=2,lty=2)
axis(side=2, at = pretty(range(c(0,cum_d_means,cum_d_ylim)),n = 8))
mtext("Cumulative Mosquito Dispersion", side = 3, line = 0.5,cex=1.25)
mtext("Percent peri-domestic", side = 1, line = 1.25)
mtext("Distance", side = 2, line = 2.25)

par(mar = margins,mfrow=c(1,1))
dev.off()


###############################################################################
# Figure 10 (changes in VC over landscapes)
###############################################################################

# hold the means
max <- 26
vc_means <- rep(0,max)
spatialvc_means <- rep(0,max)

# hold the quantiles
vc_quant <- vector("list",max)
spatialvc_quant <- vector("list",max)

# probabilities for quantiles
q_probs <- c(0.25,0.5,0.75)

# iterate through all runs
pb <- txtProgressBar(1,max)
for(i in 1:max){

  run <- as.character(i)

  # vc correction
  vc_normalized <- sumstat[[i]]$VC$VC / length(sumstat[[i]]$VC$VC)
  vc_normalized <- vc_normalized / (5*365) # normalize by time
  vc_mean_norm <- mean(vc_normalized)
  vc_means[i] <- vc_mean_norm

  # spatial vc
  spatialvc_means[i] <- mean(sumstat[[i]]$VC$dispersion)

  # vc
  vc_quant[[i]] <- quantile(vc_normalized,probs = q_probs)

  # spatial vc
  spatialvc_quant[[i]] <- quantile(sumstat[[i]]$VC$dispersion, probs = q_probs)

  setTxtProgressBar(pb,i)
}

pdf(file = paste0(plots_dir,"MBITES_fig10.pdf"),width = 12,height = 8)
par(mar = c(4.5, 4.5, 2, 4.5),mfrow=c(1,2))

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
  border = NA,col = adjustcolor("firebrick3",0.5)
)
lines(x = 1:26,
      y = vc_means,
      col = adjustcolor("firebrick3",1),lwd=2)
lines(x = 1:26,
      y = sapply(vc_quant,function(x){x[["50%"]]}),
      col = adjustcolor("firebrick3",1),lwd=2,lty=2)
axis(side=2, at = pretty(range(c(0,vc_means,vc_ylim)),n = 8))
mtext("Vectorial Capacity", side = 3, line = 0.5,cex=1.25)
mtext("Percent peri-domestic", side = 1, line = 1.25)
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
  border = NA,col = adjustcolor("mediumorchid4",0.5)
)
lines(x = 1:26,
      y = spatialvc_means,
      col = adjustcolor("mediumorchid4",1),lwd=2)
lines(x = 1:26,
      y = sapply(spatialvc_quant,function(x){x[["50%"]]}),
      col = adjustcolor("mediumorchid4",1),lwd=2,lty=2)
axis(side=2, at = pretty(range(c(0,spatialvc_means,vc_s_ylim)),n = 8))
mtext("Spatial Disperion of Vectorial Capacity", side = 3, line = 0.5,cex=1.25)
mtext("Percent peri-domestic", side = 1, line = 1.25)
mtext("Distance", side = 2, line = 2.25)


par(mar = margins,mfrow=c(1,1))
dev.off()
