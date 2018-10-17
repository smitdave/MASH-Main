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

lscape <- 13
mvmt <- readRDS(paste0(out_dir,"processed_kernels_",run,".rds"))


pdf(file = paste0(plots_dir,"MBITES_fig6.pdf"),width = 12,height = 10)
par(mfrow=c(2,2))

# plot spatially-averaged movement kernel
with(mvmt,{

  # plot smoothed PDFs
  par(mar = c(5, 5, 3, 5))
  alpha <- 0.5
  maxy <- max(PDF_sth$y)
  plot(CDF_sth$x.out,PDF_sth$y,type="l",col=adjustcolor("mediumblue",alpha),lwd=3,ylim=c(.Machine$double.eps,maxy),xlim=c(0,4),
       xlab="Distance",ylab="Density",main="Smoothed Distance Kernel CDF and PDF") # PDF
  polygon(x = c(0,CDF_sth$x.out,max(CDF_sth$x.out),0),y = c(0,PDF_sth$y,0,0),
          border = NA,col = adjustcolor("mediumblue",alpha*0.5))
  par(new = TRUE)

  # plot smoothed CDFs
  plot(CDF_sth$x.out,CDF_sth$est,type="l",col=adjustcolor("firebrick3",alpha),lwd=3,xlim=c(0,4), xaxt = "n", yaxt = "n",ylab = "", xlab = "")
  polygon(x = c(0,CDF_sth$x.out,max(CDF_sth$x.out),0),y = c(0,CDF_sth$est,0,0),
          border = NA,col = adjustcolor("firebrick3",alpha*0.5))
  axis(side=4, at = pretty(range(CDF_sth$est)))
  mtext("Cumulative Probability", side = 4, line = 2.25)
  par(mar = c(5,4,2,2)) # defaults
})

# plot site-specific movement kernel(s)
with(mvmt,{

  nsite <- length(sites)

  # calculate mean of all the smooth CDFs and PDFs
  cdfs <- sapply(X = sites,function(x){x$CDF_sth$est})
  cdfs <- ifelse(cdfs<0,0,cdfs)
  meanCDF <- rowMeans(cdfs)
  pdfs <- sapply(X = sites,function(x){x$PDF_sth$y})
  pdfs <- ifelse(pdfs<0,0,pdfs)
  meanPDF <- rowMeans(pdfs)

  # plot smoothed PDFs
  alpha <- 0.5
  maxy <- max(sapply(sites,function(x){max(x$PDF_sth$y)}))
  maxy <- round(maxy,digits = 2)
  y <- ifelse(sites[[1]]$PDF_sth$y < 0,0,sites[[1]]$PDF_sth$y)
  plot(sites[[1]]$PDF_sth$x,y,
       type="l",col=adjustcolor("mediumblue",alpha),lwd=3,ylim=c(.Machine$double.eps,maxy),xlim=c(0,4),
       main = "Smoothed Movement PDFs",xlab="Distance",ylab="Density")
  for(i in 2:nsite){
    y <- ifelse(sites[[i]]$PDF_sth$y < 0,0,sites[[i]]$PDF_sth$y)
    lines(sites[[1]]$PDF_sth$x,y,col=adjustcolor("mediumblue",alpha),lwd=3)
  }
  lines(sites[[1]]$PDF_sth$x,meanPDF,col=grey(level = 0.1),lwd=3)

  # plot smoothed CDFs
  y <- ifelse(sites[[1]]$CDF_sth$est < 0,0,sites[[1]]$CDF_sth$est)
  plot(sites[[1]]$CDF_sth$x.out,y,
       type="l",col=adjustcolor("firebrick3",alpha),lwd=3,ylim=c(0,1),xlim=c(0,4),
       main = "Smoothed Movement CDFs",xlab="Distance",ylab="Cumulative Probability")
  for(i in 2:nsite){
    y <- ifelse(sites[[i]]$CDF_sth$est < 0,0,sites[[i]]$CDF_sth$est)
    lines(sites[[i]]$CDF_sth$x.out,y,col=adjustcolor("firebrick3",alpha),lwd=3)
  }
  lines(sites[[i]]$CDF_sth$x.out,meanCDF,col=grey(level = 0.1),lwd=3)

})

# plot mosquito dispersion
maxx <- max(sumstat[[1]]$disperse_cum_smooth$knots)+(0.001*max(sumstat[[1]]$disperse_cum_smooth$knots))
plot(sumstat[[1]]$disperse_cum_smooth$knots,
     sumstat[[1]]$disperse_cum_smooth$pmf,
     type="n"
     )
polygon(x = c(0,sumstat[[1]]$disperse_cum_smooth$knots,maxx),
        y = c(0,sumstat[[1]]$disperse_cum_smooth$pmf,0),border = NA,col = "steelblue")


# plot VC dispersion




dev.off()
par(mfrow=c(1,2))


