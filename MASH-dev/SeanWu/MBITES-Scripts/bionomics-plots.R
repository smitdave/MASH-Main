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


# pdf(file = paste0(plots_dir,"MBITES_fig6.pdf"),width = 12,height = 10)

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
