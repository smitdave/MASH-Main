fclusters = getClusters(21, nCenters=25, rng=10, nn = 10)
lclusters = getClusters(20, nCenters=25, rng=10, nn = 10)

xy.f = clusters2xy(fclusters)
pointsets = list()
pdf("overlap.pdf")
xy.l = clusters2xy(lclusters)
pointSetDiagnostic(xy.f, xy.l)
pointsets[[1]] = list(xy.f =xy.f, xy.l=xy.l)

newlcluster = c(fclusters[1], lclusters[2:25])
xy.l = clusters2xy(newlcluster)
pointSetDiagnostic(xy.f, xy.l)
pointsets[[2]] = list(xy.f =xy.f, xy.l=xy.l)

for(i in 2:24){
  newlcluster = c(fclusters[1:i], lclusters[(i+1):25])
  xy.l = clusters2xy(newlcluster)
  pointSetDiagnostic(xy.f, xy.l)
  pointsets[[i+1]] = list(xy.f =xy.f, xy.l=xy.l)
}

xy.l = clusters2xy(fclusters)
pointSetDiagnostic(xy.f, xy.l)
pointsets[[26]] = list(xy.f =xy.f, xy.l=xy.l)

dev.off(dev.cur()) 

#xy.l = clusters2xy(lclusters)

#pointSetDiagnostic(xy.f, xy.l)
