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
