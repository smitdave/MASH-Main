rm(list=ls());gc()
library(spatstat)

# number of each resources
nfeed <- 250
nhab <- 250

# percent overlap
peridomPer <- seq(0,1,by=0.05)
nsim <- length(peridomPer)

# get number of points of each type for each percent peridomestic breeding
sitetypes <- lapply(peridomPer,function(x){
  both <- floor(nhab*x) # number of breeding sites in feeding sites
  habs <- nhab - both # lonely habitats are those not in peridomestic sites
  feed <- nfeed - both  # lonely feeding sites are just the leftovers
  return(c(
    both=both,
    habs=habs,
    feed=feed
  ))
})
sitetypesM <- Reduce(rbind,sitetypes)
sitetypesPer <- sitetypesM/rowSums(sitetypesM)

# make pointsets

# CSR (or pseudo-CSR; because we're conditioning on the total its like a multinomial point process, but relatively poisson-like)

types <- c("both","habitats","feeding")

points_csr <- lapply(sitetypes,function(x){
  spatstat::rmpoint(n = x,f = 1,win = owin(c(0,10),c(0,10)),types = types,verbose = TRUE,nsim = 1)
})

