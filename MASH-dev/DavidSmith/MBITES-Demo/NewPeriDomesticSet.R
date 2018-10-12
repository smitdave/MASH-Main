source ("microPointSetUtilities.R")

N = 10
fclusters = makeClusters(21, nClusters=N, rng=10, nn=5)
lclusters = makeClusters(20, nClusters=N, rng=10, nn=5)

xy.f = clusters2xy(fclusters)
l.pointsets = list()

l.pointsets[[1]] = clusters2xy(lclusters)

newlcluster = c(fclusters[1], lclusters[2:N])
l.pointsets[[2]] = clusters2xy(newlcluster)

for(i in 2:N){
  newlcluster = c(fclusters[1:i], lclusters[(i+1):25])
  l.pointsets[[i+1]] = clusters2xy(newlcluster)
}

l.pointsets[[N+1]] = clusters2xy(fclusters)

#plotpdf=TRUE
plotpdf=FALSE
if(plotpdf==TRUE){
  pdf("periDomesticClusters.pdf", width=10, height=10)
  for(i in 1:26)
    plot.fl(xy.f, l.pointsets[[i]])
  dev.off(dev.cur())
} 

plot.fl(xy.f, l.pointsets[[1]])


for( i in 1:N){
  flnm = paste("new.peridom.f", 100+i, ".xyw", sep="")
  write.table(data.frame(xy.f), flnm, sep = ",")
  flnm = paste("new.peridom.l", 100+i, ".xyw", sep="")
  write.table(data.frame(l.pointsets[[i]]), flnm, sep = ",")
}

