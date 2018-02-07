g = matrix(0,nrow=pfped$get_PedLength(),ncol=pfped$get_nAntigenLoci())
p = g
for(i in 1:pfped$get_PedLength()){
  gtype = pfped$get_gtype(i)
  g[i,] = gtype
  ptype = pfped$get_ptype(i)
  p[i,] = ptype
}

plot(g[,6])

hist(g[,6],freq=F,breaks=10)
