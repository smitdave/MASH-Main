###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Plot Movement Kernels
#     MBITES Team
#     August 2018
#
###############################################################################

###############################################################################
# load data
###############################################################################

rm(list=ls());gc()
out_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/finals/"
plot_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/plots/"

run <- "26"
KERN <- readRDS(paste0(out_directory,"processed_kernels_",run,".rds"))


###############################################################################
# plot global movement
###############################################################################

pdf(file = paste0(plot_directory,"mvmt_global_",run,".pdf"),width = 12,height = 8)
with(KERN,{

  # plot smoothed PDFs
  par(mar = c(5, 5, 3, 5))
  alpha <- 0.5
  maxy <- max(PDF_sth$y)
  plot(CDF_sth$x.out,PDF_sth$y,type="l",col=adjustcolor("mediumblue",alpha),lwd=3,ylim=c(.Machine$double.eps,maxy),xlim=c(0,4),
       xlab="Distance",ylab="Density",main="Smoothed Distance Kernel CDF and PDF") # PDF
  par(new = TRUE)

  # plot smoothed CDFs
  plot(CDF_sth$x.out,CDF_sth$est,type="l",col=adjustcolor("firebrick3",alpha),lwd=3,xlim=c(0,4), xaxt = "n", yaxt = "n",ylab = "", xlab = "")
  axis(side=4, at = pretty(range(CDF_sth$est)))
  mtext("Cumulative Probability", side = 4, line = 2.25)
  par(mar = c(5,4,2,2)) # defaults
})
dev.off()


###############################################################################
# plot site-specific movement
###############################################################################

pdf(file = paste0(plot_directory,"mvmt_specific_",run,".pdf"),width = 12,height = 8)
par(mfrow=c(1,2))

with(KERN,{

  nsite <- length(sites)

  # calculate mean of all the smooth CDFs and PDFs
  cdfs <- sapply(X = sites,function(x){x$CDF_sth$est})
  cdfs <- ifelse(cdfs<0,0,cdfs)
  meanCDF <- rowMeans(cdfs)
  pdfs <- sapply(X = sites,function(x){x$PDF_sth$y})
  pdfs <- ifelse(pdfs<0,0,pdfs)
  meanPDF <- rowMeans(pdfs)

  # # plot smoothed PDFs
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

par(mfrow=c(1,1))
dev.off()


###############################################################################
# make plots for all runs
###############################################################################

pb <- txtProgressBar(0,26)

for(i in 1:26){

  run <- as.character(i)
  KERN <- readRDS(paste0(out_directory,"processed_kernels_",run,".rds"))

  # global movement
  pdf(file = paste0(plot_directory,"mvmt_global_",run,".pdf"),width = 12,height = 8)
  with(KERN,{

    # plot smoothed PDFs
    par(mar = c(5, 5, 3, 5))
    alpha <- 0.5
    maxy <- max(PDF_sth$y)
    plot(CDF_sth$x.out,PDF_sth$y,type="l",col=adjustcolor("mediumblue",alpha),lwd=3,ylim=c(.Machine$double.eps,maxy),xlim=c(0,4),
         xlab="Distance",ylab="Density",main="Smoothed Distance Kernel CDF and PDF") # PDF
    par(new = TRUE)

    # plot smoothed CDFs
    plot(CDF_sth$x.out,CDF_sth$est,type="l",col=adjustcolor("firebrick3",alpha),lwd=3,xlim=c(0,4), xaxt = "n", yaxt = "n",ylab = "", xlab = "")
    axis(side=4, at = pretty(range(CDF_sth$est)))
    mtext("Cumulative Probability", side = 4, line = 2.25)
    par(mar = c(5,4,2,2)) # defaults
  })
  dev.off()

  # site specific movement
  pdf(file = paste0(plot_directory,"mvmt_specific_",run,".pdf"),width = 12,height = 8)
  par(mfrow=c(1,2))

  with(KERN,{

    nsite <- length(sites)

    # calculate mean of all the smooth CDFs and PDFs
    cdfs <- sapply(X = sites,function(x){x$CDF_sth$est})
    cdfs <- ifelse(cdfs<0,0,cdfs)
    meanCDF <- rowMeans(cdfs)
    pdfs <- sapply(X = sites,function(x){x$PDF_sth$y})
    pdfs <- ifelse(pdfs<0,0,pdfs)
    meanPDF <- rowMeans(pdfs)

    # # plot smoothed PDFs
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

  par(mfrow=c(1,1))
  dev.off()

  rm(KERN);gc()
  setTxtProgressBar(pb,i)
}

setTxtProgressBar(pb,i+1)
cat("\n")
