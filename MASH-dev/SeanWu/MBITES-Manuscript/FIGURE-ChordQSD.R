###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Figure - State Transitions (Chord Diagrams & QSD)
#     MBITES Team
#     February 2019
#
###############################################################################


###############################################################################
# load data
###############################################################################

rm(list=ls());gc()
library(here)
library(circlize)

gpars <- par()
analysis_dir <- "/Users/slwu89/Desktop/mbites/peridomIHME/analyzed/"

sumstat <- readRDS(file = paste0(analysis_dir,"summary.rds"))
simtime <- 5*365 # simulation time (used to normalize VC metric to a daily rate)

# calculate QSD (quasi-stationary distribution) for transition matrix P over transient set
# see eqn 3: Darroch, J. N.; Seneta, E. (1965). "On Quasi-Stationary Distributions in Absorbing Discrete-Time Finite Markov Chains". 
#            Journal of Applied Probability. 2 (1): 88–100. doi:10.2307/3211876
qsd_dtmc <- function(M,abs = c("D")){
  P <- M[-which(rownames(M) %in% abs),-which(colnames(M) %in% abs)]
  pi <- c(F=1,B=0,R=0,L=0,O=0)
  e <- rep(1,5)
  num <- (pi %*% solve(diag(nrow(P)) - P))
  num / as.numeric((num %*% e))
}


# output file
pdf(file = here("figures/ChordQSD.pdf"),width = 12,height = 10)


###############################################################################
# plot chord diagrams
###############################################################################

par(mfrow=c(2,3),mar = c(0.001,0.001,2,0.001))

# set up colors
cols <- adjustcolor(c("firebrick1","firebrick4","mediumorchid3","steelblue1","steelblue4","grey30"),
                    alpha.f = 0.85)
names(cols) <- c("F","B","R","L","O","D")
cols_df <- expand.grid(s=names(cols),d=names(cols),stringsAsFactors = FALSE)
cols_df$cols <- cols[cols_df[,"s"]]

titles <- paste0(c(0,50,100),"% Peri-domestic Breeding")

lscapes <- c(1,13,26)
for(i in 1:3){
  
  chordDiagramFromMatrix(sumstat[[lscapes[i]]]$transitions,
                         directional = 1,
                         direction.type = "arrows",
                         col = cols_df,
                         grid.col = cols,
                         grid.border = "black",
                         annotationTrack = c("grid"), 
                         annotationTrackHeight = uh(4, "mm"),
                         self.link = 2,
                         link.arr.length=0.05,
                         link.arr.lwd=0.5,
                         link.arr.width=0.05,
                         link.arr.col=grey(0.25))
  
  for(si in get.all.sector.index()) {
    xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
    ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
    circos.text(mean(xlim), mean(ylim), si, sector.index = si, track.index = 1, 
                facing = "bending.inside", niceFacing = TRUE, col = "black", cex = 1.15, font = 2)
  }
  title(main = titles[i],line = 0.5, font.main =2, cex.main = 1.5)
  
}


###############################################################################
# plot quasi-stationary distributions
###############################################################################

# x-axis setup for plotting boxes
xwidth <- 1
xtol <- 0.15
xlim <- (6*xtol)+(5*xwidth)
xleft <- ((1:5)*xtol) + ((0:4)*xwidth)
xright <- ((1:5)*xtol) + ((1:5)*xwidth)

par(mar=c(2.5,3.75,2,2))
for(i in 1:3){
  
  # calculate QSD
  qsd <- qsd_dtmc(M = sumstat[[lscapes[i]]]$transitions)
  
  # empty plot
  plot(0, xaxt = 'n', pch = '', ylab = '', xlab = '',xlim = c(0,xlim),ylim = c(0,1))
  mtext("Probability Mass", side = 2, line = 2.25, cex = 1.05)
  for(j in 1:length(qsd)){
    rect(xleft = xleft[j],ybottom = 0,
         xright = xright[j],ytop = qsd[j],
         col = cols[j],border = NA)
  }
  axis(1, at=(xleft+xright)/2, labels=names(cols)[1:5],cex.axis=1.15)

}

dev.off()
do.call("par",gpars)