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


###############################################################################
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


###############################################################################
# plot for MBITES manuscript
###############################################################################


rm(list=ls());gc()

# where the files can be found
directory <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/"
plot_directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/plots/"

# lscapes <- readRDS(file = paste0(directory,"DavidSmith/MBITES-Demo/periDomesticRaw.rds"))

margins <- par()$mar

# dwellings, habitats
cols <- c("firebrick3","steelblue")
adjcols <- adjustcolor(cols,alpha.f = 0.65)
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

pdf(file = paste0(plot_directory,"MBITES_landscape",run,".pdf"),width = 12,height = 8)
par(mar = rep(0.8,4))
plot(rbind(dwelling_xy[,1:2],habitat_xy[,1:2]),
     pch=c(rep(shapes[1],dwelling_n),rep(shapes[2],habitat_n)),
     bg=c(rep(adjcols[1],dwelling_n),rep(adjcols[2],habitat_n)),
     col=grey(0.25),
     axes = FALSE,ann=FALSE
     )
box(which = "plot", lty = "solid")
par(mar = margins)
dev.off()
