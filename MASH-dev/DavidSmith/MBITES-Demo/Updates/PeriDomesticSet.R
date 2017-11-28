source ("microPointSetUtilities.R")

fclusters = makeClusters(21, nClusters=25, rng=10, nn=10)
lclusters = makeClusters(20, nClusters=25, rng=10, nn=10)

xy.f = clusters2xy(fclusters)
l.pointsets = list()

l.pointsets[[1]] = clusters2xy(lclusters)

newlcluster = c(fclusters[1], lclusters[2:25])
l.pointsets[[2]] = clusters2xy(newlcluster)

for(i in 2:24){
  newlcluster = c(fclusters[1:i], lclusters[(i+1):25])
  l.pointsets[[i+1]] = clusters2xy(newlcluster)
}

l.pointsets[[26]] = clusters2xy(fclusters)

#plotpdf=TRUE
plotpdf=FALSE
if(plotpdf==TRUE){
  pdf("periDomesticClusters.pdf", width=10, height=10)
  for(i in 1:26)
    plot.fl(xy.f, l.pointsets[[i]])
  dev.off(dev.cur())
} 