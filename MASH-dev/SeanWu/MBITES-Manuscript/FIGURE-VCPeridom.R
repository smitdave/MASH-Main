# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###############################################################################
# # Figure 10 (changes in VC over landscapes)
# ###############################################################################
# 
# # hold the means
# max <- 26
# vc_means <- rep(0,max)
# spatialvc_means <- rep(0,max)
# 
# # hold the quantiles
# vc_quant <- vector("list",max)
# spatialvc_quant <- vector("list",max)
# 
# # probabilities for quantiles
# q_probs <- c(0.25,0.5,0.75)
# 
# # iterate through all runs
# pb <- txtProgressBar(1,max)
# for(i in 1:max){
#   
#   run <- as.character(i)
#   
#   # vc correction
#   vc_normalized <- sumstat[[i]]$VC$VC / length(sumstat[[i]]$VC$VC)
#   vc_normalized <- vc_normalized / (5*365) # normalize by time
#   vc_mean_norm <- mean(vc_normalized)
#   vc_means[i] <- vc_mean_norm
#   
#   # spatial vc
#   spatialvc_means[i] <- mean(sumstat[[i]]$VC$dispersion)
#   
#   # vc
#   vc_quant[[i]] <- quantile(vc_normalized,probs = q_probs)
#   
#   # spatial vc
#   spatialvc_quant[[i]] <- quantile(sumstat[[i]]$VC$dispersion, probs = q_probs)
#   
#   setTxtProgressBar(pb,i)
# }
# 
# pdf(file = paste0(plots_dir,"MBITES_fig10.pdf"),width = 12,height = 6)
# par(mar = c(4.5, 4.5, 2, 4.5),mfrow=c(1,2))
# 
# # mean VC
# vc_ylim <- c(min(c(unlist(vc_quant)),vc_means),max(c(vc_means,unlist(vc_quant))))
# vc_25 <- sapply(vc_quant,function(x){x[["25%"]]})
# vc_75 <- sapply(vc_quant,function(x){x[["75%"]]})
# plot(vc_means,
#      type="l", xaxt = "n", yaxt = "n",ylab = "", xlab = "",ylim = vc_ylim
# )
# polygon(
#   x = c(1:26,26:1),
#   y = c(vc_75,rev(vc_25)),
#   border = NA,col = adjustcolor("firebrick3",0.5)
# )
# lines(x = 1:26,
#       y = vc_means,
#       col = adjustcolor("firebrick3",1),lwd=2)
# lines(x = 1:26,
#       y = sapply(vc_quant,function(x){x[["50%"]]}),
#       col = adjustcolor("firebrick3",1),lwd=2,lty=2)
# axis(side=2, at = pretty(range(c(0,vc_means,vc_ylim)),n = 8))
# mtext("Vectorial Capacity", side = 3, line = 0.5,cex=1.25)
# mtext("Percent peri-domestic", side = 1, line = 1.25)
# mtext("Number of Secondary Bites", side = 2, line = 2.25)
# 
# 
# # mean dispersion of VC
# vc_s_ylim <- c(min(c(unlist(spatialvc_quant)),spatialvc_means),max(c(spatialvc_means,unlist(spatialvc_quant))))
# vc_s_25 <- sapply(spatialvc_quant,function(x){x[["25%"]]})
# vc_s_75 <- sapply(spatialvc_quant,function(x){x[["75%"]]})
# plot(spatialvc_means,
#      type="l", xaxt = "n", yaxt = "n",ylab = "", xlab = "",ylim = vc_s_ylim
# )
# polygon(
#   x = c(1:26,26:1),
#   y = c(vc_s_75,rev(vc_s_25)),
#   border = NA,col = adjustcolor("mediumorchid4",0.5)
# )
# lines(x = 1:26,
#       y = spatialvc_means,
#       col = adjustcolor("mediumorchid4",1),lwd=2)
# lines(x = 1:26,
#       y = sapply(spatialvc_quant,function(x){x[["50%"]]}),
#       col = adjustcolor("mediumorchid4",1),lwd=2,lty=2)
# axis(side=2, at = pretty(range(c(0,spatialvc_means,vc_s_ylim)),n = 8))
# mtext("Spatial Disperion of Vectorial Capacity", side = 3, line = 0.5,cex=1.25)
# mtext("Percent peri-domestic", side = 1, line = 1.25)
# mtext("Distance", side = 2, line = 2.25)
# 
# 
# par(mar = margins,mfrow=c(1,1))
# dev.off()
# 
# 
# ###############################################################################
# # SI Figure 1 (QSD over states as fn. of landscapes)
# ###############################################################################
# 
# qsd <- t(sapply(sumstat,function(x){
#   setNames(qsd_dtmc(x$transitions),c("F","B","R","L","O"))
# }))
# 
# qsd_cols <- c("firebrick1","firebrick4","mediumorchid3","steelblue1","steelblue4")
# 
# pdf(file = paste0(plots_dir,"MBITES_figSI1.pdf"),width = 12,height = 8)
# par(mar = c(4.5, 4.5, 2, 4.5))
# 
# plot(x=0:25,y=qsd[,"F"],yaxt="n",ylab="",xlab="",type="l",ylim=c(0,1),col=qsd_cols[1],lwd=2.5)
# text(x = 0,y = qsd[1,"F"],labels = "F",col = qsd_cols[1],pos = 3,cex = 1.15)
# text(x = 0,y = qsd[1,"B"],labels = "B",col = qsd_cols[2],pos = 3,cex = 1.15)
# text(x = 1,y = qsd[1,"R"],labels = "R",col = qsd_cols[3],pos = 3,cex = 1.15)
# text(x = 0,y = qsd[1,"L"],labels = "L",col = qsd_cols[4],pos = 3,cex = 1.15)
# text(x = 0,y = qsd[1,"O"],labels = "O",col = qsd_cols[5],pos = 1,cex = 1.15)
# lines(x = 0:25,
#       y = qsd[,"B"],
#       col = qsd_cols[2],
#       lwd=2.5)
# lines(x = 0:25,
#       y = qsd[,"R"],
#       col = qsd_cols[3],
#       lwd=2.5)
# lines(x = 0:25,
#       y = qsd[,"L"],
#       col = qsd_cols[4],
#       lwd=2.5)
# lines(x = 0:25,
#       y = qsd[,"O"],
#       col = qsd_cols[5],
#       lwd=2.5)
# axis(side=2, at = seq(0,1,by=0.2))
# mtext("Quasi-stationary Distribution", side = 3, line = 0.5,cex=1.25)
# mtext("Percent peri-domestic", side = 1, line = 2)
# mtext("Probability Density", side = 2, line = 2.25)
# 
# 
# par(mar = margins)
# dev.off()
