###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Plot Peri-domestic Point Sets
#     MBITES Team
#     August 2018
#
###############################################################################


##############################################################################f#
# plot landscapes for all point sets
###############################################################################

rm(list=ls());gc()

# where the files can be found
directory <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/"
plot_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/plots/"

lscapes <- readRDS(file = paste0(directory,"DavidSmith/MBITES-Demo/periDomesticRaw.rds"))

margins <- par()$mar

for(i in 1:26){

  cat("plotting pointsets for landscape ",i," of 26\n")
  run <- as.character(i)

  pdf(file = paste0(plot_directory,"MBITES_points",run,".pdf"),width = 12,height = 8)
  par(mar = rep(0.2,4),bg = grey(0.15))
  plot(lscapes[[i]]$sites[,c("x","y")],
       pch=21,cex=4.5,bg=grey(level = 0.95,alpha = 0.75),
       col="white",axes = FALSE,ann=FALSE)
  text(lscapes[[i]]$sites[,c("x","y")],
       labels=c("F","H","B")[lscapes[[i]]$sites[,"type"]+1],
       # col=adjustcolor("grey30",alpha.f = 0.75),
       col=c(adjustcolor("firebrick3",alpha.f = 0.85),
             adjustcolor("steelblue",alpha.f = 0.85),
             adjustcolor("mediumorchid3",alpha.f = 0.85))[lscapes[[i]]$sites[,"type"]+1],
       cex=0.6)
  dev.off()
}