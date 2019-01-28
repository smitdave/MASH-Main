###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Figure - Landscape Point Sets
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
lscape_dir <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/DavidSmith/MBITES-Demo/"

# generate a color scheme similar to ggplot2's default categorical color scheme
ggcol <- function(n, alpha = 1) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alpha)[1:n]
}

# dwellings, habitats, both
cols <- ggcol(n = 3)
adjcols <- adjustcolor(cols,alpha.f = 0.5)
shapes <- c(21,24,22)

# compare floats
fequal <- function(x,y,tol=.Machine$double.eps^0.5){
  abs(x-y) < tol
}

# load landscape data
lscapes_ix <- c(1,13,26)
lscapes <- readRDS(file = paste0(lscape_dir,"periDomesticRaw.rds"))

# output file
pdf(file = here("figures/Landscape.pdf"),width = 16,height = 6)


###############################################################################
# generate figure
###############################################################################

par(mfrow=c(1,3),mar = rep(0.5,4))

titles <- paste0(c(0,50,100),"% Peri-domestic Breeding")
for(i in 1:length(lscapes_ix)){
  
  l <- lscapes_ix[[i]]
  plot(0, xaxt = 'n',xaxt ="n", yaxt ="n", pch = '', ylab = '', xlab = '',xlim = c(-11.5,11.5),ylim = c(-11.25,11.5))
  points(x = lscapes[[l]]$sites[,c("x","y")],
         pch = shapes[lscapes[[l]]$sites[,"type"]+1],
         bg = adjcols[lscapes[[l]]$sites[,"type"]+1],
         col = cols[lscapes[[l]]$sites[,"type"]+1])
  text(x = 6.5,y = 11.35,labels = titles[i], cex = 1.45)
  legend(x = "topleft",
         pt.bg = adjcols,
         col = cols,
         pch = shapes,
         legend = c("Haunts","Habitats","Both"),
         bty = "n")
  
}

dev.off()
par(gpars)