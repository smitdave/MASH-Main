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

###############################################################################
# make MBITES bionomics plots for all runs
###############################################################################

# where the files can be found
directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/finals/"
lscape_dir <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/"
plot_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/plots/"

# loop over all experiments
pb <- txtProgressBar(min = 1,max = 26)
for(i in 1:26){

  run <- as.character(i)
  MBITES <- readRDS(paste0(directory,"/analysis",run,".rds"))

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
    vc <- hist(vc_df$vc,probability = TRUE,breaks = 100,
         col = adjustcolor("firebrick3",alpha.f = 0.5),
         xlab = "Secondary Bites", ylab = "Density",
         main = paste0("MBITES Vectorial Capacity (mean: ",round(vc_mean,2),")"))
    abline(v = vc_mean,lwd=2.5,lty=2,col="firebrick3")
    abline(v = max(vc_df$vc),lwd=2.5,lty=2,col=adjustcolor("steelblue",alpha.f = 0.5))
    text(x = max(vc_df$vc),y=max(vc$density)*0.1,paste0("max: ",max(vc_df$vc)),
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

  # only need to recompute because i forgot to properly assign in the EXPERIMENT-Processing.R script.
  dmat <- as.matrix(read.csv(paste0(lscape_dir,"DavidSmith/MBITES-Demo/dist_",run,".csv"), header = FALSE))

  # get pairs of eggs
  spatial_egg_pairs <- Filter(f = function(x){
    !(is.nan(x$natal) && is.nan(x$dest))
  },x = MBITES$lifetime_egg$dispersion)


  # spatial egg dispersion
  spatial_egg_pairs <- lapply(spatial_egg_pairs,function(x){
    out <- NULL
    i <- x$natal
    for(j in 1:length(x$dest)){
      out <- append(out,dmat[i,x$dest[j]])
    }
    return(out)
  })
  spatial_egg_dist <- do.call(c,spatial_egg_pairs)
  spatial_egg_dist <- sort(spatial_egg_dist,decreasing = FALSE)

  spatial_egg_bins <- unique(spatial_egg_dist)


  # get empirical PDF by summing stuff in the distance bins (takes awhile, use parallel if you can)
  spatial_egg_PDF_emp <- mclapply(X = spatial_egg_bins,FUN = function(x,spatial_egg_dist){
    length(spatial_egg_dist[which(fequal(spatial_egg_dist,x))])
  },spatial_egg_dist=spatial_egg_dist,mc.cores = detectCores()-2)
  spatial_egg_PDF_emp <- unlist(spatial_egg_PDF_emp) # mclapply outputs lists; coerce to vector
  # technically its a PMF so we normalize it
  spatial_egg_PDF_emp <- spatial_egg_PDF_emp/sum(spatial_egg_PDF_emp)

  # get empirical CDF by preforming a cumulative sum over data points in distance bins
  spatial_egg_CDF_emp <- cumsum(spatial_egg_PDF_emp)

  # smoothed CDF and PDF
  spatial_egg_CDF_sth <- glkerns(spatial_egg_bins,spatial_egg_CDF_emp,deriv = 0,korder = 4,x.out=spatial_egg_bins)
  spatial_egg_PDF_sth <- glkerns(spatial_egg_bins,spatial_egg_CDF_emp,deriv = 1,korder = 3,x.out=spatial_egg_bins)


  # plot
  pdf(file = paste0(plot_directory,"MBITES_spatialegg_",run,".pdf"),width = 12,height = 8)
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


  rm(MBITES,dmat,
     spatial_egg_pairs,spatial_egg_dist,spatial_egg_bins,
     spatial_egg_CDF_emp,spatial_egg_PDF_emp,spatial_egg_CDF_sth,spatial_egg_PDF_sth);gc()
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

# loop over all experiments
pb <- txtProgressBar(min = 1,max = 26)
for(i in 1:26){

  run <- as.character(i)

  # mosquito histories
  mosquitos_df <- fromJSON(paste0(directory,"/mosquito_F_",run,".json"), flatten = TRUE)
  null_m <- which(sapply(mosquitos_df$id,is.null))
  if(length(null_m)>0){
    mosquitos_df <- mosquitos_df[-null_m,]
  }

  # state transitions matrix
  M <- Bionomics_StateTransition(mosquitos_df)

  # take out D self loop but keep its width
  Vis <- M
  Vis <- Vis*0
  Vis <- Vis+1
  Vis["D","D"] <- 0
  Vis <- as.logical(Vis)

  # make chord diagram
  # colors (F, B, R, L, O, D)
  cols <- adjustcolor(c("firebrick1","firebrick3","mediumorchid3","steelblue1","steelblue3","grey30"),
                      alpha.f = 0.75)
  names(cols) <- c("F","B","R","L","O","D")
  cols_df <- expand.grid(s=rownames(M),d=colnames(M),stringsAsFactors = FALSE)
  cols_df$cols <- cols[cols_df[,"s"]]
  chordDiagramFromMatrix(M,
                         directional = 1,
                         direction.type = "arrows",
                         col = cols_df,
                         grid.col = cols,
                         link.visible = Vis,
                         grid.border = "black",
                         annotationTrack = c("name","grid"),
                         self.link = 2)

  rm(mosquitos_df,M);gc()
  setTxtProgressBar(pb,i)
}
setTxtProgressBar(pb,i+1)
cat("\n")
