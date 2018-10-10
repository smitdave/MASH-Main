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
library(jsonlite)
library(parallel)
library(lokern)

###############################################################################
# make MBITES bionomics plots for all runs
###############################################################################

# where the files can be found
# directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/finals/"
directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/analyzed/"
lscape_dir <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/"
plot_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/plots/"
# plot_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/test/"

# loop over all experiments
pb <- txtProgressBar(min = 1,max = 26)
for(i in 1:26){

  run <- as.character(i)
  # MBITES <- readRDS(paste0(directory,"/analysis",run,".rds"))
  # read in data
  MBITES_basic <- readRDS(paste0(directory,"analysis_run_",run,".rds_basic.rds"))
  MBITES_egg <- readRDS(paste0(directory,"analysis_run_",run,".rds_spatialEgg.rds"))
  MBITES_Hvc <- readRDS(paste0(directory,"analysis_run_",run,".rds_spatialVC.rds"))
  MBITES_uniqueVC <- readRDS(paste0(directory,"uniqueVC",run,".rds"))
  # MBITES_Mvc <- readRDS(paste0(directory,"analysis_run_",run,".rds_spatialMosyVC.rds"))
  # MBITES <- c(MBITES_basic,MBITES_egg,MBITES_Hvc,MBITES_Mvc)
  MBITES <- c(MBITES_basic,MBITES_egg,MBITES_Hvc,MBITES_uniqueVC)

  # lifespan/survival function
  pdf(file = paste0(plot_directory,"MBITES_survival_",run,".pdf"),width = 12,height = 8)
  with(MBITES,{
    plot(surv_tt, surv_s,
         type = "l", xlab = "Age (Days)", ylab = "Density",
         main=paste0("MBITES Survival Function (mean: ",round(surv_mean,2),")"),
         lwd=2,col="firebrick3",xlim=c(0,50))
    polygon(c(0,surv_tt), c(0,surv_s),
            border=NA, col=adjustcolor("firebrick3",alpha.f = 0.5))
    abline(v = surv_mean,lwd=2.5,lty=2,col="firebrick3")
  })
  dev.off()

  # number of human blood hosts
  pdf(file = paste0(plot_directory,"MBITES_numbloodhost_",run,".pdf"),width = 12,height = 8)
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
  dev.off()

  # feeding interval
  pdf(file = paste0(plot_directory,"MBITES_feedingcycle_",run,".pdf"),width = 12,height = 8)
  with(MBITES,{
    hist(blood_interval$rest_intervals,probability = TRUE,breaks = 100,
         col = adjustcolor("firebrick3",alpha.f = 0.5),
         xlab = "Time (days)", ylab = "Density",
         main = paste0("MBITES Feeding Cycle Duration (mean: ",round(blood_interval_mean,2),")"))
    abline(v = blood_interval_mean,lwd=2.5,lty=2,col="firebrick3")
  })
  dev.off()

  # vectorial capacity
  pdf(file = paste0(plot_directory,"MBITES_vc_",run,".pdf"),width = 12,height = 8)
  with(MBITES,{
    vc_normalized <- vc_df$vc / nrow(vc_df)
    vc_normalized <- vc_normalized / (5*365) # normalize by time
    vc_max <- max(vc_normalized)
    vc_mean_norm <- mean(vc_normalized)
    vc <- hist(vc_normalized,probability = TRUE,breaks = 100,
         col = adjustcolor("firebrick3",alpha.f = 0.5),
         xlab = "Secondary Bites", ylab = "Density",
         main = paste0("MBITES Vectorial Capacity (mean: ",round(vc_mean_norm,2),")"))
    abline(v = vc_mean_norm,lwd=2.5,lty=2,col="firebrick3")
    abline(v = vc_max,lwd=2.5,lty=2,col=adjustcolor("steelblue",alpha.f = 0.5))
    text(x = vc_max,y=max(vc$density)*0.1,paste0("max: ",round(vc_max,2)),
         col=adjustcolor("steelblue",alpha.f = 0.75),adj=1.15)
  })
  dev.off()

  # unique secondary host vectorial capacity
  pdf(file = paste0(plot_directory,"MBITES_vc_unique_",run,".pdf"),width = 12,height = 8)
  with(MBITES,{
    vc_normalized_u <- MBITES_uniqueVC / length(MBITES_uniqueVC)
    vc_normalized_u <- vc_normalized_u / (5*365) # normalize by time
    vc_max_u <- max(vc_normalized_u)
    vc_mean_norm_u <- mean(vc_normalized_u)
    vc_mean_norm_u <- round(vc_mean_norm_u,4)
    vc_u <- hist(vc_normalized_u,probability = TRUE,breaks = 100,
               col = adjustcolor("firebrick3",alpha.f = 0.5),
               xlab = "Secondary Bites", ylab = "Density",
               main = paste0("MBITES Vectorial Capacity (mean: ",vc_mean_norm_u,")\n(unique secondary hosts)"))
    abline(v = vc_mean_norm_u,lwd=2.5,lty=2,col="firebrick3")
    abline(v = vc_max_u,lwd=2.5,lty=2,col=adjustcolor("steelblue",alpha.f = 0.5))
    text(x = vc_max_u,y=max(vc_u$density)*0.1,paste0("max: ",round(vc_max_u,2)),
         col=adjustcolor("steelblue",alpha.f = 0.75),adj=1.15)
  })
  dev.off()

  # lifetime egg production
  pdf(file = paste0(plot_directory,"MBITES_lifetimeEgg_",run,".pdf"),width = 12,height = 8)
  with(MBITES,{
    hist(lifetime_egg$lifetime,probability = TRUE,breaks = 100,
         col = adjustcolor("firebrick3",alpha.f = 0.5),
         xlab = "Eggs", ylab = "Density",
         main = paste0("MBITES Lifetime Egg Production (mean: ",round(lifetime_egg_mean,2),")"))
    abline(v = lifetime_egg_mean,lwd=2.5,lty=2,col="firebrick3")
  })
  dev.off()

  # egg laying rate
  pdf(file = paste0(plot_directory,"MBITES_eggrate_",run,".pdf"),width = 12,height = 8)
  with(MBITES,{
    hist(egg_rate$ages,probability = TRUE,breaks = 100,
         col = adjustcolor("firebrick3",alpha.f = 0.5),
         xlab = "Age (Days)", ylab = "Density",
         main = paste0("MBITES Egg Laying Rate (mean: ",round(egg_rate_mean,2),")"))
    abline(v = egg_rate_mean,lwd=2.5,lty=2,col="firebrick3")
  })
  dev.off()

  # blood feeding rate
  pdf(file = paste0(plot_directory,"MBITES_bloodrate_",run,".pdf"),width = 12,height = 8)
  with(MBITES,{
    hist(blood_rate,probability = TRUE,breaks = 100,
         col = adjustcolor("firebrick3",alpha.f = 0.5),
         xlab = "Age (Days)", ylab = "Density",
         main = paste0("MBITES Blood Feeding Rate (mean: ",round(blood_rate_mean,2),")"))
    abline(v = blood_rate_mean,lwd=2.5,lty=2,col="firebrick3")
  })
  dev.off()

  # spatial vc
  pdf(file = paste0(plot_directory,"MBITES_spatialvc_",run,".pdf"),width = 12,height = 8)
  with(MBITES,{
    par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
    plot(spatial_vc_CDF_sth$x.out, spatial_vc_CDF_sth$est,type="l",col="firebrick3",lwd=3,
         ylab="CDF",xlab="Distance",main="Spatial Dispersion of Vectorial Capacity")
    polygon(x=c(0,spatial_vc_CDF_sth$x.out,tail(spatial_vc_CDF_sth$x.out,1)),
            y= c(head(spatial_vc_CDF_sth$est,1),spatial_vc_CDF_sth$est,head(spatial_vc_CDF_sth$est,1)),
            border=NA, col=adjustcolor("firebrick3",alpha.f = 0.5))
    par(new = TRUE)
    plot(spatial_vc_PDF_sth$x.out, spatial_vc_PDF_sth$est, type = "l",col="steelblue",lwd=3,
         axes = FALSE, bty = "n", xlab = "", ylab = "")
    polygon(c(0,spatial_vc_PDF_sth$x.out), c(0,spatial_vc_PDF_sth$est),
            border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
    axis(side=4, at = pretty(range(spatial_vc_PDF_sth$est)))
    mtext("PDF", side=4, line=3)
  })
  dev.off()

  # spatial egg dispersion
  pdf(file = paste0(plot_directory,"MBITES_spatialegg_",run,".pdf"),width = 12,height = 8)
  with(MBITES,{
    par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
    plot(spatial_egg_CDF_sth$x.out, spatial_egg_CDF_sth$est,type="l",col="firebrick3",lwd=3,
         ylab="CDF",xlab="Distance",main="Spatial Dispersion of Egg Batches")
    polygon(x=c(0,spatial_egg_CDF_sth$x.out,tail(spatial_egg_CDF_sth$x.out,1)),
            y= c(head(spatial_egg_CDF_sth$est,1),spatial_egg_CDF_sth$est,head(spatial_egg_CDF_sth$est,1)),
            border=NA, col=adjustcolor("firebrick3",alpha.f = 0.5))
    par(new = TRUE)
    plot(spatial_egg_PDF_sth$x.out, spatial_egg_PDF_sth$est, type = "l",col="steelblue",lwd=3,
         axes = FALSE, bty = "n", xlab = "", ylab = "")
    polygon(c(0,spatial_egg_PDF_sth$x.out), c(0,spatial_egg_PDF_sth$est),
            border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
    axis(side=4, at = pretty(range(spatial_egg_PDF_sth$est)))
    mtext("PDF", side=4, line=3)
    dev.off()
  })

  rm(MBITES);gc()
  setTxtProgressBar(pb,i)
}
setTxtProgressBar(pb,i+1)
cat("\n")


###############################################################################
# make chord diagrams for all runs
###############################################################################

rm(list = ls());gc()
library(MBITES)
library(jsonlite)
library(circlize)

# where the files can be found
directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/finals/"
plot_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/plots/"

# loop over all experiments
# pdf(file = paste0(plot_directory,"MBITES_chordAll.pdf"),width = 12,height = 8)
# par(mfrow=c(4,7))
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

  # # take out D self loop but keep its width
  # Vis <- M
  # Vis <- Vis*0
  # Vis <- Vis+1
  # Vis["D","D"] <- 0
  # Vis <- as.logical(Vis)

  # make chord diagram
  # colors (F, B, R, L, O, D)
  cols <- adjustcolor(c("firebrick1","firebrick3","mediumorchid3","steelblue1","steelblue3","grey30"),
                      alpha.f = 0.75)
  names(cols) <- c("F","B","R","L","O","D")
  cols_df <- expand.grid(s=rownames(M),d=colnames(M),stringsAsFactors = FALSE)
  cols_df$cols <- cols[cols_df[,"s"]]
  pdf(file = paste0(plot_directory,"MBITES_chord",run,".pdf"),width = 12,height = 8)
  chordDiagramFromMatrix(M,
                         directional = 1,
                         direction.type = "arrows",
                         col = cols_df,
                         grid.col = cols,
                         # link.visible = Vis,
                         grid.border = "black",
                         annotationTrack = c("name","grid"),
                         self.link = 2)
  # title(paste0("Simulated Landscape ",run))
  dev.off()

  rm(mosquitos_df,M);gc()
}
# par(mfrow=c(1,1))
# dev.off()


###############################################################################
# plot changes in means over all runs
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
vc_means <- rep(0,max)
vc_u_means <- rep(0,max)
lifetimeEgg_means <- rep(0,max)
eggrate_means <- rep(0,max)
bloodrate_means <- rep(0,max)
spatialvc_means <- rep(0,max)
spatialegg_means <- rep(0,max)

# hold the quantiles
lifespan_quant <- vector("list",max)
numbloodhost_quant <- vector("list",max)
feedingcycle_quant <- vector("list",max)
vc_quant <- vector("list",max)
vc_u_quant <- vector("list",max)
lifetimeEgg_quant <- vector("list",max)
eggrate_quant <- vector("list",max)
bloodrate_quant <- vector("list",max)
spatialvc_quant <- vector("list",max)
spatialegg_quant <- vector("list",max)

# probabilities for quantiles
q_probs <- c(0.2,0.25,0.5,0.75,0.8)

# iterate through all runs
pb <- txtProgressBar(1,max)
for(i in 1:max){

  run <- as.character(i)
  # MBITES <- readRDS(paste0(directory,"/analysis",run,".rds"))

  MBITES_basic <- readRDS(paste0(directory,"analysis_run_",run,".rds_basic.rds"))
  MBITES_egg <- readRDS(paste0(directory,"analysis_run_",run,".rds_spatialEgg.rds"))
  MBITES_Hvc <- readRDS(paste0(directory,"analysis_run_",run,".rds_spatialVC.rds"))
  MBITES_uniqueVC <- readRDS(paste0(directory,"uniqueVC",run,".rds"))
  MBITES <- c(MBITES_basic,MBITES_egg,MBITES_Hvc)
  MBITES$MBITES_uniqueVC <- MBITES_uniqueVC

  # get means
  lifespan_means[i] <- MBITES$surv_mean
  numbloodhost_means[i] <- MBITES$blood_hosts_mean
  feedingcycle_means[i] <- MBITES$blood_interval_mean

  # vc correction
  vc_normalized <- MBITES$vc_df$vc / nrow(MBITES$vc_df)
  vc_normalized <- vc_normalized / (5*365) # normalize by time
  vc_max <- max(vc_normalized)
  vc_mean_norm <- mean(vc_normalized)
  vc_means[i] <- vc_mean_norm
  # vc_means[i] <- MBITES$vc_mean

  # unique secondary hosts for vc
  vc_normalized_u <- MBITES$MBITES_uniqueVC / length(MBITES$MBITES_uniqueVC)
  vc_normalized_u <- vc_normalized_u / (5*365) # normalize by time
  vc_max_u <- max(vc_normalized_u)
  vc_mean_norm_u <- mean(vc_normalized_u)
  vc_u_means[i] <- vc_mean_norm_u

  lifetimeEgg_means[i] <- MBITES$lifetime_egg_mean
  eggrate_means[i] <- MBITES$egg_rate_mean
  bloodrate_means[i] <- MBITES$blood_rate_mean
  spatialvc_means[i] <- weighted.mean(MBITES$spatial_vc_PDF_sth$x.out,MBITES$spatial_vc_PDF_sth$est)
  spatialegg_means[i] <- weighted.mean(MBITES$spatial_egg_PDF_sth$x.out,MBITES$spatial_egg_PDF_sth$est)

  # get quantiles
  lifespan_quant[[i]] <- wtd.quantile(MBITES$surv_tt,MBITES$surv_s,probs = q_probs)
  numbloodhost_quant[[i]] <- quantile(MBITES$blood_hosts$humanHost,probs = q_probs)
  feedingcycle_quant[[i]] <- quantile(MBITES$blood_interval$rest_intervals,probs = q_probs)
  # vc_quant[[i]] <- quantile(MBITES$vc_df$vc,probs = q_probs)

  # vc
  vc_quant[[i]] <- quantile(vc_normalized,probs = q_probs)

  # vc with unique hosts
  vc_u_quant[[i]] <- quantile(vc_normalized_u,probs = q_probs)

  lifetimeEgg_quant[[i]] <- quantile(MBITES$lifetime_egg$lifetime,probs = q_probs)
  eggrate_quant[[i]] <- quantile(MBITES$egg_rate$ages,probs = q_probs)
  bloodrate_quant[[i]] <- quantile(MBITES$blood_rate,probs = q_probs)
  spatialvc_quant[[i]] <- wtd.quantile(MBITES$spatial_vc_PDF_sth$x.out,MBITES$spatial_vc_PDF_sth$est,probs = q_probs)
  spatialegg_quant[[i]] <- wtd.quantile(MBITES$spatial_egg_PDF_sth$x.out,MBITES$spatial_egg_PDF_sth$est,probs = q_probs)

  setTxtProgressBar(pb,i)
}


# lifespan
maxy <- max(sapply(lifespan_quant,max))
pdf(file = paste0(plot_directory,"MBITES_means_lifespan.pdf"),width = 12,height = 8)
plot(x = 1:max,
     y = lifespan_means,
     pch=16,col=adjustcolor("firebrick3",alpha.f = 0.75),
     xlab = "Simulated Landscape",ylab = "Time (Days)",main = "MBITES Lifespan Distribution",
     ylim = c(0,ceiling(maxy)))
for(i in 1:max){
  rect(xleft = (i-0.2),
       ybottom = lifespan_quant[[i]][[1]],
       xright = (i+0.2),
       ytop = lifespan_quant[[i]][[5]],
       border = adjustcolor("firebrick3",alpha.f = 0.75),
       lwd = 1.5)
  # lower 25% quantile
  segments(x0 = (i-0.2),
           y0 = lifespan_quant[[i]][[2]],
           x1 = (i+0.2),
           y1 = lifespan_quant[[i]][[2]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # upper 75% quantile
  segments(x0 = (i-0.2),
           y0 = lifespan_quant[[i]][[4]],
           x1 = (i+0.2),
           y1 = lifespan_quant[[i]][[4]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # median
  segments(x0 = (i-0.2),
           y0 = lifespan_quant[[i]][[3]],
           x1 = (i+0.2),
           y1 = lifespan_quant[[i]][[3]],
           col = "grey40",
           lwd = 2.5,lend=2)
}
dev.off()

# number of blood hosts
maxy <- max(sapply(numbloodhost_quant,max))
pdf(file = paste0(plot_directory,"MBITES_means_numBloodHost.pdf"),width = 12,height = 8)
plot(x = 1:max,
     y = numbloodhost_means,
     pch=16,col=adjustcolor("firebrick3",alpha.f = 0.75),
     xlab = "Simulated Landscape",ylab = "Number of Hosts",main = "MBITES Human Blood Hosts",
     ylim = c(0,ceiling(maxy)))
for(i in 1:max){
  rect(xleft = (i-0.2),
       ybottom = numbloodhost_quant[[i]][[1]],
       xright = (i+0.2),
       ytop = numbloodhost_quant[[i]][[5]],
       border = adjustcolor("firebrick3",alpha.f = 0.75),
       lwd = 1.5)
  # lower 25% quantile
  segments(x0 = (i-0.2),
           y0 = numbloodhost_quant[[i]][[2]],
           x1 = (i+0.2),
           y1 = numbloodhost_quant[[i]][[2]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # upper 75% quantile
  segments(x0 = (i-0.2),
           y0 = numbloodhost_quant[[i]][[4]],
           x1 = (i+0.2),
           y1 = numbloodhost_quant[[i]][[4]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # median
  segments(x0 = (i-0.2),
           y0 = numbloodhost_quant[[i]][[3]],
           x1 = (i+0.2),
           y1 = numbloodhost_quant[[i]][[3]],
           col = "grey40",
           lwd = 2.5,lend=2)
}
dev.off()

# feeding interval
maxy <- max(sapply(feedingcycle_quant,max))
pdf(file = paste0(plot_directory,"MBITES_means_feedingInterval.pdf"),width = 12,height = 8)
plot(x = 1:max,
     y = feedingcycle_means,
     pch=16,col=adjustcolor("firebrick3",alpha.f = 0.75),
     xlab = "Simulated Landscape",ylab = "Time (Days)",main = "MBITES Feeding Cycle Duration",
     ylim = c(0,ceiling(maxy)))
for(i in 1:max){
  rect(xleft = (i-0.2),
       ybottom = feedingcycle_quant[[i]][[1]],
       xright = (i+0.2),
       ytop = feedingcycle_quant[[i]][[5]],
       border = adjustcolor("firebrick3",alpha.f = 0.75),
       lwd = 1.5)
  # lower 25% quantile
  segments(x0 = (i-0.2),
           y0 = feedingcycle_quant[[i]][[2]],
           x1 = (i+0.2),
           y1 = feedingcycle_quant[[i]][[2]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # upper 75% quantile
  segments(x0 = (i-0.2),
           y0 = feedingcycle_quant[[i]][[4]],
           x1 = (i+0.2),
           y1 = feedingcycle_quant[[i]][[4]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # median
  segments(x0 = (i-0.2),
           y0 = feedingcycle_quant[[i]][[3]],
           x1 = (i+0.2),
           y1 = feedingcycle_quant[[i]][[3]],
           col = "grey40",
           lwd = 2.5,lend=2)
}
dev.off()

# vectorial capacity
maxy <- max(c(sapply(vc_quant,max)),vc_means)
pdf(file = paste0(plot_directory,"MBITES_means_vc.pdf"),width = 12,height = 8)
plot(x = 1:max,
     y = vc_means,
     pch=16,col=adjustcolor("firebrick3",alpha.f = 0.75),
     xlab = "Simulated Landscape",ylab = "Secondary Bites",main = "MBITES Vectorial Capacity",
     ylim = c(0,
              ceiling(1e1*maxy)*1e-1
              ))
for(i in 1:max){
  rect(xleft = (i-0.2),
       ybottom = vc_quant[[i]][[1]],
       xright = (i+0.2),
       ytop = vc_quant[[i]][[5]],
       border = adjustcolor("firebrick3",alpha.f = 0.75),
       lwd = 1.5)
  # lower 25% quantile
  segments(x0 = (i-0.2),
           y0 = vc_quant[[i]][[2]],
           x1 = (i+0.2),
           y1 = vc_quant[[i]][[2]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # upper 75% quantile
  segments(x0 = (i-0.2),
           y0 = vc_quant[[i]][[4]],
           x1 = (i+0.2),
           y1 = vc_quant[[i]][[4]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # median
  segments(x0 = (i-0.2),
           y0 = vc_quant[[i]][[3]],
           x1 = (i+0.2),
           y1 = vc_quant[[i]][[3]],
           col = "grey40",
           lwd = 2.5,lend=2)
}
dev.off()

# vectorial capacity with unique hosts
maxy <- max(c(sapply(vc_u_quant,max)),vc_u_means)
pdf(file = paste0(plot_directory,"MBITES_means_vc_unique.pdf"),width = 12,height = 8)
plot(x = 1:max,
     y = vc_u_means,
     pch=16,col=adjustcolor("firebrick3",alpha.f = 0.75),
     xlab = "Simulated Landscape",ylab = "Secondary Bites",main = "MBITES Vectorial Capacity\n(unique secondary hosts)",
     ylim = c(0,
              ceiling(1e1*maxy)*1e-1
              ))
for(i in 1:max){
  rect(xleft = (i-0.2),
       ybottom = vc_u_quant[[i]][[1]],
       xright = (i+0.2),
       ytop = vc_u_quant[[i]][[5]],
       border = adjustcolor("firebrick3",alpha.f = 0.75),
       lwd = 1.5)
  # lower 25% quantile
  segments(x0 = (i-0.2),
           y0 = vc_u_quant[[i]][[2]],
           x1 = (i+0.2),
           y1 = vc_u_quant[[i]][[2]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # upper 75% quantile
  segments(x0 = (i-0.2),
           y0 = vc_u_quant[[i]][[4]],
           x1 = (i+0.2),
           y1 = vc_u_quant[[i]][[4]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # median
  segments(x0 = (i-0.2),
           y0 = vc_u_quant[[i]][[3]],
           x1 = (i+0.2),
           y1 = vc_u_quant[[i]][[3]],
           col = "grey40",
           lwd = 2.5,lend=2)
}
dev.off()

# lifetime egg production
maxy <- max(c(sapply(lifetimeEgg_quant,max)),lifetimeEgg_means)
pdf(file = paste0(plot_directory,"MBITES_means_lifetimeEgg.pdf"),width = 12,height = 8)
plot(x = 1:max,
     y = lifetimeEgg_means,
     pch=16,col=adjustcolor("firebrick3",alpha.f = 0.75),
     xlab = "Simulated Landscape",ylab = "Eggs",main = "MBITES Lifetime Egg Production",
     ylim = c(0,ceiling(maxy)))
for(i in 1:max){
  rect(xleft = (i-0.2),
       ybottom = lifetimeEgg_quant[[i]][[1]],
       xright = (i+0.2),
       ytop = lifetimeEgg_quant[[i]][[5]],
       border = adjustcolor("firebrick3",alpha.f = 0.75),
       lwd = 1.5)
  # lower 25% quantile
  segments(x0 = (i-0.2),
           y0 = lifetimeEgg_quant[[i]][[2]],
           x1 = (i+0.2),
           y1 = lifetimeEgg_quant[[i]][[2]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # upper 75% quantile
  segments(x0 = (i-0.2),
           y0 = lifetimeEgg_quant[[i]][[4]],
           x1 = (i+0.2),
           y1 = lifetimeEgg_quant[[i]][[4]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # median
  segments(x0 = (i-0.2),
           y0 = lifetimeEgg_quant[[i]][[3]],
           x1 = (i+0.2),
           y1 = lifetimeEgg_quant[[i]][[3]],
           col = "grey40",
           lwd = 2.5,lend=2)
}
dev.off()

# egg laying rate
maxy <- max(c(sapply(eggrate_quant,max)),eggrate_means)
pdf(file = paste0(plot_directory,"MBITES_means_eggrate.pdf"),width = 12,height = 8)
plot(x = 1:max,
     y = eggrate_means,
     pch=16,col=adjustcolor("firebrick3",alpha.f = 0.75),
     xlab = "Simulated Landscape",ylab = "Age (Days)",main = "MBITES Egg Laying Rate",
     ylim = c(0,ceiling(maxy)))
for(i in 1:max){
  rect(xleft = (i-0.2),
       ybottom = eggrate_quant[[i]][[1]],
       xright = (i+0.2),
       ytop = eggrate_quant[[i]][[5]],
       border = adjustcolor("firebrick3",alpha.f = 0.75),
       lwd = 1.5)
  # lower 25% quantile
  segments(x0 = (i-0.2),
           y0 = eggrate_quant[[i]][[2]],
           x1 = (i+0.2),
           y1 = eggrate_quant[[i]][[2]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # upper 75% quantile
  segments(x0 = (i-0.2),
           y0 = eggrate_quant[[i]][[4]],
           x1 = (i+0.2),
           y1 = eggrate_quant[[i]][[4]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # median
  segments(x0 = (i-0.2),
           y0 = eggrate_quant[[i]][[3]],
           x1 = (i+0.2),
           y1 = eggrate_quant[[i]][[3]],
           col = "grey40",
           lwd = 2.5,lend=2)
}
dev.off()

# blood feeding rate
maxy <- max(c(sapply(bloodrate_quant,max)),bloodrate_means)
pdf(file = paste0(plot_directory,"MBITES_means_bloodrate.pdf"),width = 12,height = 8)
plot(x = 1:max,
     y = bloodrate_means,
     pch=16,col=adjustcolor("firebrick3",alpha.f = 0.75),
     xlab = "Simulated Landscape",ylab = "Age (Days)",main = "MBITES Blood Feeding Rate",
     ylim = c(0,ceiling(maxy)))
for(i in 1:max){
  rect(xleft = (i-0.2),
       ybottom = bloodrate_quant[[i]][[1]],
       xright = (i+0.2),
       ytop = bloodrate_quant[[i]][[5]],
       border = adjustcolor("firebrick3",alpha.f = 0.75),
       lwd = 1.5)
  # lower 25% quantile
  segments(x0 = (i-0.2),
           y0 = bloodrate_quant[[i]][[2]],
           x1 = (i+0.2),
           y1 = bloodrate_quant[[i]][[2]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # upper 75% quantile
  segments(x0 = (i-0.2),
           y0 = bloodrate_quant[[i]][[4]],
           x1 = (i+0.2),
           y1 = bloodrate_quant[[i]][[4]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # median
  segments(x0 = (i-0.2),
           y0 = bloodrate_quant[[i]][[3]],
           x1 = (i+0.2),
           y1 = bloodrate_quant[[i]][[3]],
           col = "grey40",
           lwd = 2.5,lend=2)
}
dev.off()

# spatial vc
maxy <- max(c(sapply(spatialvc_quant,max)),spatialvc_means)
pdf(file = paste0(plot_directory,"MBITES_means_spatialvc.pdf"),width = 12,height = 8)
plot(x = 1:max,
     y = spatialvc_means,
     pch=16,col=adjustcolor("firebrick3",alpha.f = 0.75),
     xlab = "Simulated Landscape",ylab = "Distance",main = "MBITES Spatial Dispersion of VC",
     ylim = c(0,ceiling(maxy)))
for(i in 1:max){
  rect(xleft = (i-0.2),
       ybottom = spatialvc_quant[[i]][[1]],
       xright = (i+0.2),
       ytop = spatialvc_quant[[i]][[5]],
       border = adjustcolor("firebrick3",alpha.f = 0.75),
       lwd = 1.5)
  # lower 25% quantile
  segments(x0 = (i-0.2),
           y0 = spatialvc_quant[[i]][[2]],
           x1 = (i+0.2),
           y1 = spatialvc_quant[[i]][[2]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # upper 75% quantile
  segments(x0 = (i-0.2),
           y0 = spatialvc_quant[[i]][[4]],
           x1 = (i+0.2),
           y1 = spatialvc_quant[[i]][[4]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # median
  segments(x0 = (i-0.2),
           y0 = spatialvc_quant[[i]][[3]],
           x1 = (i+0.2),
           y1 = spatialvc_quant[[i]][[3]],
           col = "grey40",
           lwd = 2.5,lend=2)
}
dev.off()

# spatial oviposition
maxy <- max(c(sapply(spatialegg_quant,max)),spatialegg_means)
pdf(file = paste0(plot_directory,"MBITES_means_spatialegg.pdf"),width = 12,height = 8)
plot(x = 1:max,
     y = spatialegg_means,
     pch=16,col=adjustcolor("firebrick3",alpha.f = 0.75),
     xlab = "Simulated Landscape",ylab = "Distance",main = "MBITES Spatial Dispersion of Eggs",
     ylim = c(0,ceiling(maxy)))
for(i in 1:max){
  rect(xleft = (i-0.2),
       ybottom = spatialegg_quant[[i]][[1]],
       xright = (i+0.2),
       ytop = spatialegg_quant[[i]][[5]],
       border = adjustcolor("firebrick3",alpha.f = 0.75),
       lwd = 1.5)
  # lower 25% quantile
  segments(x0 = (i-0.2),
           y0 = spatialegg_quant[[i]][[2]],
           x1 = (i+0.2),
           y1 = spatialegg_quant[[i]][[2]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # upper 75% quantile
  segments(x0 = (i-0.2),
           y0 = spatialegg_quant[[i]][[4]],
           x1 = (i+0.2),
           y1 = spatialegg_quant[[i]][[4]],
           col = adjustcolor("steelblue",alpha.f = 0.8),
           lwd = 2.5,lend=2)
  # median
  segments(x0 = (i-0.2),
           y0 = spatialegg_quant[[i]][[3]],
           x1 = (i+0.2),
           y1 = spatialegg_quant[[i]][[3]],
           col = "grey40",
           lwd = 2.5,lend=2)
}
dev.off()
