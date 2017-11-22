fclusters = getClusters(21, nCenters=25, rng=10, nn = 10)
lclusters = getClusters(20, nCenters=25, rng=10, nn = 10)

xy.f = clusters2xy(fclusters)
pointsets = list()
pdf("overlap.pdf")
xy.l = clusters2xy(lclusters)
plot.fl(xy.f, xy.l)
pointsets[[1]] = list(xy.f =xy.f, xy.l=xy.l)

newlcluster = c(fclusters[1], lclusters[2:25])
xy.l = clusters2xy(newlcluster)
plot.fl(xy.f, xy.l)
pointsets[[2]] = list(xy.f =xy.f, xy.l=xy.l)

for(i in 2:24){
  newlcluster = c(fclusters[1:i], lclusters[(i+1):25])
  xy.l = clusters2xy(newlcluster)
  plot.fl(xy.f, xy.l)
  pointsets[[i+1]] = list(xy.f =xy.f, xy.l=xy.l)
}

xy.l = clusters2xy(fclusters)
plot.fl(xy.f, xy.l)
pointsets[[26]] = list(xy.f =xy.f, xy.l=xy.l)

dev.off(dev.cur()) 
plot.fl(xy.f, xy.l)
segments(0,0,0,1)
#xy.l = clusters2xy(lclusters)