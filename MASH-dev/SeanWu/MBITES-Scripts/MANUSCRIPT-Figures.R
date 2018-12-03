###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Graphics for MBITES manuscript
#     MBITES Team
#     October 2018
#
###############################################################################


###############################################################################
# Figure 4 (landscape)
###############################################################################

rm(list=ls());gc()

# where the files can be found
directory <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/"
plot_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/plots/"

# lscapes <- readRDS(file = paste0(directory,"DavidSmith/MBITES-Demo/periDomesticRaw.rds"))

margins <- par()$mar

# dwellings, habitats
cols <- c("firebrick3","steelblue")
adjcols <- adjustcolor(cols,alpha.f = 0.5)
shapes <- c(21,24)

i <- 13

cat("plotting pointsets for landscape ",i," of 26\n")
if(i < 10){
  run <- paste0("0",i)
} else {
  run <- as.character(i)
}

dwelling_file <- paste0(directory,"DavidSmith/MBITES-Demo/peridom.f1",run,".xyw")
habitats_file <-paste0(directory,"DavidSmith/MBITES-Demo/peridom.l1",run,".xyw")

dwelling_xy <- read.csv2(file = dwelling_file,header = T,sep = ",",stringsAsFactors = FALSE)
habitat_xy <- read.csv2(file = habitats_file,header = T,sep = ",",stringsAsFactors = FALSE)

dwelling_n <- nrow(dwelling_xy)
habitat_n <- nrow(habitat_xy)

pdf(file = paste0(plot_directory,"MBITES_fig4.pdf"),width = 12,height = 8)
par(mar = rep(0.8,4))
plot(rbind(dwelling_xy[,1:2],habitat_xy[,1:2]),
     pch=c(rep(shapes[1],dwelling_n),rep(shapes[2],habitat_n)),
     bg=c(rep(adjcols[1],dwelling_n),rep(adjcols[2],habitat_n)),
     col=grey(0.05),
     axes = FALSE,ann=FALSE
)
box(which = "plot", lty = "solid")
par(mar = margins)
dev.off()


###############################################################################
# Figure 6 (VC and num blood hosts for a landscape)
###############################################################################

rm(list = ls());gc()

# directories
directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/analyzed/"
lscape_dir <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/"
plot_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/plots/"

lscapes <- c(1,13,26)

pdf(file = paste0(plot_directory,"MBITES_fig6.pdf"),width = 12,height = 8)
par(mfrow=c(3,2))
for(i in lscapes){

  run <- as.character(i)

  # read in data
  MBITES_basic <- readRDS(paste0(directory,"analysis_run_",run,".rds_basic.rds"))
  MBITES_egg <- readRDS(paste0(directory,"analysis_run_",run,".rds_spatialEgg.rds"))
  MBITES <- c(MBITES_basic,MBITES_egg)

  # how 2 plot vc
  with(MBITES,{
    vc_normalized <- vc_df$vc / nrow(vc_df)
    vc_normalized <- vc_normalized / (5*365) # normalize by time
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

  # num blood hosts S
  with(MBITES,{
    hbh <- hist(blood_hosts$humanHost,probability = TRUE,breaks = 100,
                col = adjustcolor("firebrick3",alpha.f = 0.5),
                xlab = "Number of Blood Hosts", ylab = "Density",
                main = paste0("MBITES Human Blood Hosts (mean: ",round(blood_hosts_mean,2),")"))
    abline(v = blood_hosts_mean,lwd=2.5,lty=2,col="firebrick3")
    abline(v = max(blood_hosts$humanHost),lwd=2.5,lty=2,col=adjustcolor("steelblue",alpha.f = 0.5))
    text(x = max(blood_hosts$humanHost),y=max(hbh$density)*0.1,paste0("max: ",max(blood_hosts$humanHost)),
         col=adjustcolor("steelblue",alpha.f = 0.75),adj=1.15)
  })

  rm(MBITES_basic,MBITES_egg,MBITES);gc()
}
dev.off()
par(mfrow=c(1,1))


###############################################################################
# Figure 7 (dispersion for landscape 13)
###############################################################################

# rm(list=ls());gc()
# out_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/finals/"
# plot_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/plots/"
#
# run <- "13"
# KERN <- readRDS(paste0(out_directory,"processed_kernels_",run,".rds"))
#
# with(KERN,{
#
#   # plot smoothed PDFs
#   par(mar = c(5, 5, 3, 5))
#   alpha <- 0.5
#   maxy <- max(PDF_sth$y)
#   plot(CDF_sth$x.out,PDF_sth$y,type="l",col=adjustcolor("mediumblue",alpha),lwd=3,ylim=c(.Machine$double.eps,maxy),xlim=c(0,4),
#        xlab="Distance",ylab="Density",main="Smoothed Distance Kernel CDF and PDF") # PDF
#   par(new = TRUE)
#
#   # plot smoothed CDFs
#   plot(CDF_sth$x.out,CDF_sth$est,type="l",col=adjustcolor("firebrick3",alpha),lwd=3,xlim=c(0,4), xaxt = "n", yaxt = "n",ylab = "", xlab = "")
#   axis(side=4, at = pretty(range(CDF_sth$est)))
#   mtext("Cumulative Probability", side = 4, line = 2.25)
#   par(mar = c(5,4,2,2)) # defaults
# })
#
#
# with(KERN,{
#
#   nsite <- length(sites)
#
#   # calculate mean of all the smooth CDFs and PDFs
#   cdfs <- sapply(X = sites,function(x){x$CDF_sth$est})
#   cdfs <- ifelse(cdfs<0,0,cdfs)
#   meanCDF <- rowMeans(cdfs)
#   pdfs <- sapply(X = sites,function(x){x$PDF_sth$y})
#   pdfs <- ifelse(pdfs<0,0,pdfs)
#   meanPDF <- rowMeans(pdfs)
#
#   # # plot smoothed PDFs
#   alpha <- 0.5
#   maxy <- max(sapply(sites,function(x){max(x$PDF_sth$y)}))
#   maxy <- round(maxy,digits = 2)
#   y <- ifelse(sites[[1]]$PDF_sth$y < 0,0,sites[[1]]$PDF_sth$y)
#   plot(sites[[1]]$PDF_sth$x,y,
#        type="l",col=adjustcolor("mediumblue",alpha),lwd=3,ylim=c(.Machine$double.eps,maxy),xlim=c(0,4),
#        main = "Smoothed Movement PDFs",xlab="Distance",ylab="Density")
#   for(i in 2:nsite){
#     y <- ifelse(sites[[i]]$PDF_sth$y < 0,0,sites[[i]]$PDF_sth$y)
#     lines(sites[[1]]$PDF_sth$x,y,col=adjustcolor("mediumblue",alpha),lwd=3)
#   }
#   lines(sites[[1]]$PDF_sth$x,meanPDF,col=grey(level = 0.1),lwd=3)
#
#   # plot smoothed CDFs
#   y <- ifelse(sites[[1]]$CDF_sth$est < 0,0,sites[[1]]$CDF_sth$est)
#   plot(sites[[1]]$CDF_sth$x.out,y,
#        type="l",col=adjustcolor("firebrick3",alpha),lwd=3,ylim=c(0,1),xlim=c(0,4),
#        main = "Smoothed Movement CDFs",xlab="Distance",ylab="Cumulative Probability")
#   for(i in 2:nsite){
#     y <- ifelse(sites[[i]]$CDF_sth$est < 0,0,sites[[i]]$CDF_sth$est)
#     lines(sites[[i]]$CDF_sth$x.out,y,col=adjustcolor("firebrick3",alpha),lwd=3)
#   }
#   lines(sites[[i]]$CDF_sth$x.out,meanCDF,col=grey(level = 0.1),lwd=3)
#
# })

# MBITES_Hvc <- readRDS(paste0(directory,"analysis_run_",run,".rds_spatialVC.rds"))
# with(MBITES,{
#   par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
#   plot(spatial_vc_CDF_sth$x.out, spatial_vc_CDF_sth$est,type="l",col="firebrick3",lwd=3,
#        ylab="CDF",xlab="Distance",main="Spatial Dispersion of Vectorial Capacity")
#   polygon(x=c(0,spatial_vc_CDF_sth$x.out,tail(spatial_vc_CDF_sth$x.out,1)),
#           y= c(head(spatial_vc_CDF_sth$est,1),spatial_vc_CDF_sth$est,head(spatial_vc_CDF_sth$est,1)),
#           border=NA, col=adjustcolor("firebrick3",alpha.f = 0.5))
#   par(new = TRUE)
#   plot(spatial_vc_PDF_sth$x.out, spatial_vc_PDF_sth$est, type = "l",col="steelblue",lwd=3,
#        axes = FALSE, bty = "n", xlab = "", ylab = "")
#   polygon(c(0,spatial_vc_PDF_sth$x.out), c(0,spatial_vc_PDF_sth$est),
#           border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
#   axis(side=4, at = pretty(range(spatial_vc_PDF_sth$est)))
#   mtext("PDF", side=4, line=3)
# })


###############################################################################
# Figure 8 (chord diagrams)
###############################################################################


rm(list = ls());gc()
library(MBITES)
library(jsonlite)
library(circlize)

# where the files can be found
directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/finals/"
plot_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/plots/"

# loop over all experiments
margins <- par()$mar
pdf(file = paste0(plot_directory,"MBITES_fig8.pdf"),width = 12,height = 8)
par(mfrow=c(4,7),mar = rep(0.001,4))

for(i in 1:26){

  cat("plotting run ",i," of 26\n")
  run <- as.character(i)

  # mosquito histories
  mosquitos_df <- fromJSON(paste0(directory,"/mosquito_F_",run,".json"), flatten = TRUE)
  null_m <- which(sapply(mosquitos_df$id,is.null))
  if(length(null_m)>0){
    mosquitos_df <- mosquitos_df[-null_m,]
  }

  # state transitions matrix
  M <- Bionomics_StateTransition(mosquitos_df)

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
                         # link.visible = Vis,
                         grid.border = "black",
                         annotationTrack = c("name","grid"),
                         self.link = 2,
                         link.arr.lwd=0.5,
                         link.arr.width=0.15,
                         link.arr.col=grey(0.25))

  rm(mosquitos_df,M);gc()
}

par(mar = margins,mfrow=c(1,1))
dev.off()


###############################################################################
# Figure 9 (changes in bionomics over landscapes)
###############################################################################

rm(list = ls());gc()
library(MBITES)
library(jsonlite)
library(Hmisc)

# directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/finals/"
directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/analyzed/"
plot_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/plots/"

# hold the means
max <- 26
lifespan_means <- rep(0,max)
numbloodhost_means <- rep(0,max)
feedingcycle_means <- rep(0,max)
bloodrate_means <- rep(0,max)

# hold the quantiles
lifespan_quant <- vector("list",max)
numbloodhost_quant <- vector("list",max)
feedingcycle_quant <- vector("list",max)
bloodrate_quant <- vector("list",max)

# probabilities for quantiles
q_probs <- c(0.2,0.25,0.5,0.75,0.8)

# iterate through all runs
pb <- txtProgressBar(1,max)
for(i in 1:max){

  run <- as.character(i)

  MBITES <- readRDS(paste0(directory,"analysis_run_",run,".rds_basic.rds"))

  # get means
  lifespan_means[i] <- MBITES$surv_mean
  numbloodhost_means[i] <- MBITES$blood_hosts_mean
  feedingcycle_means[i] <- MBITES$blood_interval_mean
  bloodrate_means[i] <- MBITES$blood_rate_mean

  # get quantiles
  lifespan_quant[[i]] <- wtd.quantile(MBITES$surv_tt,MBITES$surv_s,probs = q_probs)
  numbloodhost_quant[[i]] <- quantile(MBITES$blood_hosts$humanHost,probs = q_probs)
  feedingcycle_quant[[i]] <- quantile(MBITES$blood_interval$rest_intervals,probs = q_probs)
  bloodrate_quant[[i]] <- quantile(MBITES$blood_rate,probs = q_probs)

  setTxtProgressBar(pb,i)
}

pdf(file = paste0(plot_directory,"MBITES_fig9.pdf"),width = 12,height = 8)
par(mfrow=c(2,2))

# lifespan
mean_lf_col <- "firebrick3"
median_lf_col <- "firebrick3"
q80_lf_col <- adjustcolor(mean_lf_col,alpha.f = 0.5)
maxy_lf <- max(sapply(lifespan_quant,max))

plot(x = 1:max,
     y = lifespan_means,
     type="l",
     lty=1,lwd=3,
     col=mean_lf_col,
     xlab = "Simulated Landscape",ylab = "Time (Days)",main = "MBITES Lifespan Distribution",
     ylim = c(0,ceiling(maxy_lf)))
lines(x = 1:max,y=sapply(lifespan_quant,FUN = function(x){x[["50%"]]}),
      lty=2,lwd=3,col=median_lf_col)
polygon(x = c(1:max,max:1),
        y = c(
          sapply(lifespan_quant,FUN = function(x){x[["80%"]]}),
          rev(sapply(lifespan_quant,FUN = function(x){x[["20%"]]}))
        ),
        col = q80_lf_col,border = NA)

# blood feeding rate
mean_bf_col <- "chartreuse4"
median_bf_col <- "chartreuse4"
q80_bf_col <- adjustcolor(mean_bf_col,alpha.f = 0.5)
maxy_bf <- max(sapply(bloodrate_quant,max))

plot(x = 1:max,
     y = bloodrate_means,
     type="l",
     lty=1,lwd=3,
     col=mean_bf_col,
     xlab = "Simulated Landscape",ylab = "Age (Days)",main = "MBITES Blood Feeding Rate",
     ylim = c(0,ceiling(maxy_bf)))
lines(x = 1:max,y=sapply(bloodrate_quant,FUN = function(x){x[["50%"]]}),
      lty=2,lwd=3,col=median_bf_col)
polygon(x = c(1:max,max:1),
        y = c(
          sapply(bloodrate_quant,FUN = function(x){x[["80%"]]}),
          rev(sapply(bloodrate_quant,FUN = function(x){x[["20%"]]}))
        ),
        col = q80_bf_col,border = NA)

# feeding cycle duration
mean_fc_col <- "steelblue"
median_fc_col <- "steelblue"
q80_fc_col <- adjustcolor(mean_fc_col,alpha.f = 0.5)
maxy_fc <-max(sapply(feedingcycle_quant,max))

plot(x = 1:max,
     y = feedingcycle_means,
     type="l",
     lty=1,lwd=3,
     col=mean_fc_col,
     xlab = "Simulated Landscape",ylab = "Time (Days)",main = "MBITES Feeding Cycle Duration",
     ylim = c(0,ceiling(maxy_fc)))
lines(x = 1:max,y=sapply(feedingcycle_quant,FUN = function(x){x[["50%"]]}),
      lty=2,lwd=3,col=median_fc_col)
polygon(x = c(1:max,max:1),
        y = c(
          sapply(feedingcycle_quant,FUN = function(x){x[["80%"]]}),
          rev(sapply(feedingcycle_quant,FUN = function(x){x[["20%"]]}))
        ),
        col = q80_fc_col,border = NA)

# number of blood hosts
mean_bh_col <- "mediumorchid4"
median_bh_col <- "mediumorchid4"
q80_bh_col <- adjustcolor(mean_bh_col,alpha.f = 0.5)
maxy_bh <-max(sapply(numbloodhost_quant,max))

plot(x = 1:max,
     y = numbloodhost_means,
     type="l",
     lty=1,lwd=3,
     col=mean_bh_col,
     xlab = "Simulated Landscape",ylab = "Number of Hosts",main = "MBITES Human Blood Hosts",
     ylim = c(0,ceiling(maxy_bh)))
lines(x = 1:max,y=sapply(numbloodhost_quant,FUN = function(x){x[["50%"]]}),
      lty=2,lwd=3,col=median_bh_col)
polygon(x = c(1:max,max:1),
        y = c(
          sapply(numbloodhost_quant,FUN = function(x){x[["80%"]]}),
          rev(sapply(numbloodhost_quant,FUN = function(x){x[["20%"]]}))
        ),
        col = q80_bh_col,border = NA)

dev.off()
par(mfrow=c(1,1))


###############################################################################
# Figure 10 (changes in VC over landscapes)
###############################################################################

rm(list = ls());gc()
library(MBITES)
library(jsonlite)
library(Hmisc)

# directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/finals/"
directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/analyzed/"
plot_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/plots/"

# hold the means
max <- 26
vc_means <- rep(0,max)
spatialvc_means <- rep(0,max)

# hold the quantiles
vc_quant <- vector("list",max)
spatialvc_quant <- vector("list",max)

# probabilities for quantiles
q_probs <- c(0.2,0.25,0.5,0.75,0.8)

# iterate through all runs
pb <- txtProgressBar(1,max)
for(i in 1:max){

  run <- as.character(i)
  MBITES_spatial <- readRDS(paste0(directory,"analysis_run_",run,".rds_spatialVC.rds"))
  MBITES_vc <- readRDS(paste0(directory,"analysis_run_",run,".rds_basic.rds"))
  MBITES <- c(MBITES_spatial,MBITES_vc)

  # vc correction
  vc_normalized <- MBITES$vc_df$vc / nrow(MBITES$vc_df)
  vc_normalized <- vc_normalized / (5*365) # normalize by time
  vc_max <- max(vc_normalized)
  vc_mean_norm <- mean(vc_normalized)
  vc_means[i] <- vc_mean_norm

  # spatial vc
  spatialvc_means[i] <- weighted.mean(MBITES$spatial_vc_PDF_sth$x.out,MBITES$spatial_vc_PDF_sth$est)

  # vc
  vc_quant[[i]] <- quantile(vc_normalized,probs = q_probs)

  # spatial vc
  spatialvc_quant[[i]] <- wtd.quantile(MBITES$spatial_vc_PDF_sth$x.out,MBITES$spatial_vc_PDF_sth$est,probs = q_probs)

  setTxtProgressBar(pb,i)
}
rm(MBITES_vc,MBITES_spatial,MBITES);gc()

# VC
mean_vc_col <- "firebrick3"
median_vc_col <- "firebrick3"
q80_vc_col <- adjustcolor(mean_vc_col,alpha.f = 0.5)
maxy_vc <-max(sapply(vc_quant,max))

plot(x = 1:max,
     y = vc_means,
     type="l",
     lty=1,lwd=3,
     col=mean_vc_col,
     xlab = "Simulated Landscape",ylab = "Number of Secondary Bites",main = "MBITES Vectorial Capacity",
     ylim = c(0,ceiling(maxy_vc*1e1)*1e-1))
lines(x = 1:max,y=sapply(vc_quant,FUN = function(x){x[["50%"]]}),
      lty=2,lwd=3,col=median_vc_col)
polygon(x = c(1:max,max:1),
        y = c(
          sapply(vc_quant,FUN = function(x){x[["80%"]]}),
          rev(sapply(vc_quant,FUN = function(x){x[["20%"]]}))
        ),
        col = q80_vc_col,border = NA)

# spatial VC
mean_svc_col <- "mediumorchid4"
median_svc_col <- "mediumorchid4"
q80_svc_col <- adjustcolor(mean_svc_col,alpha.f = 0.5)
maxy_svc <-max(sapply(spatialvc_quant,max))

plot(x = 1:max,
     y = spatialvc_means,
     type="l",
     lty=1,lwd=3,
     col=mean_svc_col,
     xlab = "Simulated Landscape",ylab = "Distance",main = "MBITES Spatial Dispersion of VC",
     ylim = c(0,ceiling(maxy_svc*1e1)*1e-1))
lines(x = 1:max,y=sapply(spatialvc_quant,FUN = function(x){x[["50%"]]}),
      lty=2,lwd=3,col=median_svc_col)
polygon(x = c(1:max,max:1),
        y = c(
          sapply(spatialvc_quant,FUN = function(x){x[["80%"]]}),
          rev(sapply(spatialvc_quant,FUN = function(x){x[["20%"]]}))
        ),
        col = q80_svc_col,border = NA)
