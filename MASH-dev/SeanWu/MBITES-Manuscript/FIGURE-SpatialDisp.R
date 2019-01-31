###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Figure - Spatial Dispersion of Mosquitos/VC
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
rds_dir <- "/Users/slwu89/Desktop/mbites/peridomIHME/finals/"
analysis_dir <- "/Users/slwu89/Desktop/mbites/peridomIHME/analyzed/"

sumstat <- readRDS(file = paste0(analysis_dir,"summary.rds"))
simtime <- 5*365 # simulation time (used to normalize VC metric to a daily rate)


###############################################################################
# Plot Spatial Dispersion for single landscape
###############################################################################

# read in the processed movement kernel(s)
l <- 13
mvmt <- readRDS(paste0(rds_dir,"processed_kernels_",l,".rds"))

# output file
pdf(file = here("figures/",paste0("SpatialDisp_",l,".pdf")),width = 12,height = 10)

# make the plot

# movement kernel (averaged over all sites)

# PDF
avg_k_m <- weighted.mean(mvmt$CDF_sth$x.out,mvmt$PDF_sth$y)
xlim <- c(0,5)
par(mar = c(4.5, 4.5, 2.5, 4.5),mfrow=c(2,2))
plot(c(0,mvmt$CDF_sth$x.out,max(mvmt$CDF_sth$x.out),0),
     c(0,mvmt$PDF_sth$y,0,0),
     type="n",xlab="Distance",ylab="",xlim=xlim
)
mtext("Probability Density", side = 2, line = 2.25)
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


# cumulative (lifetime) dispersion

# PDF
maxx <- max(sumstat[[l]]$disperse_cum_smooth$knots)+(0.001*max(sumstat[[l]]$disperse_cum_smooth$knots))
cum_disp_m <- weighted.mean(x = sumstat[[l]]$disperse_cum_smooth$knots,w = sumstat[[l]]$disperse_cum_smooth$pmf)
plot(sumstat[[l]]$disperse_cum_smooth$knots,
     sumstat[[l]]$disperse_cum_smooth$pdf$est,
     type="n",xlab="Distance",ylab="",xlim=xlim
)
mtext("Probability Density", side = 2, line = 2.25)
mtext(paste0("Cumulative Movement (mean: ",round(cum_disp_m,3),")"),side = 3,line = 0.5,cex=1.25)
polygon(x = c(0,sumstat[[l]]$disperse_cum_smooth$knots,maxx),
        y = c(0,sumstat[[l]]$disperse_cum_smooth$pdf$est,0),border = NA,col = adjustcolor("mediumblue",alpha.f = 0.6))
par(new = TRUE)

# CDF
plot(sumstat[[l]]$disperse_cum_smooth$knots,
     sumstat[[l]]$disperse_cum_smooth$cdf$est,
     type="n", xaxt = "n", yaxt = "n",ylab = "", xlab = "",xlim=xlim)
lines(x = sumstat[[l]]$disperse_cum_smooth$knots,
      y = sumstat[[l]]$disperse_cum_smooth$cdf$est,
      col = adjustcolor("firebrick3",1),lwd=2)
axis(side=4, at = pretty(range(sumstat[[l]]$disperse_cum_smooth$cdf$est)))
mtext("Cumulative Probability", side = 4, line = 2.25)


# absolute dispersion (displacement)

# PDF
maxx <- max(sumstat[[l]]$disperse_abs_smooth$knots)+(0.001*max(sumstat[[l]]$disperse_abs_smooth$knots))
abs_disp_m <- weighted.mean(x = sumstat[[l]]$disperse_abs_smooth$knots,w = sumstat[[l]]$disperse_abs_smooth$pmf)
plot(c(0,sumstat[[l]]$disperse_abs_smooth$knots),
     c(0,ifelse(sumstat[[l]]$disperse_abs_smooth$pdf$est<0,0,sumstat[[l]]$disperse_abs_smooth$pdf$est)),
     type="n",xlab="Distance",ylab="",xlim=xlim
)
mtext("Probability Density", side = 2, line = 2.25)
mtext(paste0("Lifetime Displacement (mean: ",round(abs_disp_m,3),")"),side = 3,line = 0.5,cex=1.25)
polygon(x = c(0,sumstat[[l]]$disperse_abs_smooth$knots,maxx),
        y = c(0,ifelse(sumstat[[l]]$disperse_abs_smooth$pdf$est<0,0,sumstat[[l]]$disperse_abs_smooth$pdf$est),0),border = NA,col = adjustcolor("mediumblue",alpha.f = 0.6))
par(new = TRUE)

# CDF
plot(sumstat[[l]]$disperse_abs_smooth$knots,
     sumstat[[l]]$disperse_abs_smooth$cdf$est,
     type="n", xaxt = "n", yaxt = "n",ylab = "", xlab = "",xlim=xlim)
lines(x = c(sumstat[[l]]$disperse_abs_smooth$knots),
      y = c(sumstat[[l]]$disperse_abs_smooth$cdf$est),
      col = adjustcolor("firebrick3",1),lwd=2)
axis(side=4, at = pretty(range(c(0,sumstat[[l]]$disperse_abs_smooth$cdf$est))))
mtext("Cumulative Probability", side = 4, line = 2.25)


# VC dispersion

# PDF
maxx <- max(sumstat[[l]]$vc_dispersion$knots)+(0.001*max(sumstat[[l]]$vc_dispersion$knots))
vc_disp_m <- weighted.mean(x = sumstat[[l]]$vc_dispersion$knots,w = sumstat[[l]]$vc_dispersion$pmf)
plot(sumstat[[l]]$vc_dispersion$knots,
     sumstat[[l]]$vc_dispersion$pdf$est,
     type="n",xlab="Distance",ylab="",xlim=xlim
)
mtext("Probability Density", side = 2, line = 2.25)
mtext(paste0("VC Dispersion (mean: ",round(vc_disp_m,3),")"),side = 3,line = 0.5,cex=1.25)
polygon(x = c(0,sumstat[[l]]$vc_dispersion$knots,maxx),
        y = c(0,sumstat[[l]]$vc_dispersion$pdf$est,0),border = NA,col = adjustcolor("mediumblue",alpha.f = 0.6))
par(new = TRUE)

# CDF
plot(sumstat[[l]]$vc_dispersion$knots,
     sumstat[[l]]$vc_dispersion$cdf$est,
     type="n", xaxt = "n", yaxt = "n",ylab = "", xlab = "",xlim=xlim)
lines(x = sumstat[[l]]$vc_dispersion$knots,
      y = sumstat[[l]]$vc_dispersion$cdf$est,
      col = adjustcolor("firebrick3",1),lwd=2)
axis(side=4, at = pretty(range(sumstat[[l]]$vc_dispersion$cdf$est)))
mtext("Cumulative Probability", side = 4, line = 2.25)

dev.off()
do.call("par",gpars)