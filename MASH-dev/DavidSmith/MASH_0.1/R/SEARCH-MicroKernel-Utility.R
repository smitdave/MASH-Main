#################################################################
#
#   MASH
#   R6-ified
#   MICRO-Search Kernels Utilities (analysis & visualization)
#   David Smith, Hector Sanchez, Sean Wu
#   June 1, 2017
#
#################################################################


#################################################################
# SEARCH-MicroKernels
#################################################################

#' MICRO Search Kernels: Plot Kernel Functions
#'
#'
#'
#'
#'
#' @param S pointer to list of starting sites (for example, Landscape$get_AquaSites())
#' @param D pointer to list of destination sites (for example, Landscape$get_FeedingSites())
#' @param N number of random starting sites
#' @param dMax maximum distance for distance bins
#' @param dMesh granularity of distance bins
#' @param sigma a param
#' @param eps a param
#' @param beta a param
#' @export
MicroKernelPlot_utility <- function(S,D, N = 50, dMesh = 0.01, dMax=NULL, sigma = 3, eps = 0.1, beta = 0){

  M1 = MicroKernel_PowerKernel(S,D) #Markov transition matrix
  M2 = MicroKernel_DistanceMat(S,D) #distance matrix
  M1norm = M1/sum(M1) #normalized transition matrix

  if(is.null(dMax)){
    dMax = max(M2) + max(M2)*0.05
  }

  xCDF = function(x, M2, M1){ #return movement CDF by distance bins
    sum(M1[which(M2<=x, arr.ind = TRUE)])
  }

  x=seq(0, dMax, by = dMesh) #distance bins
  cdfT = sapply(X = x,FUN = xCDF, M2=M2, M1=M1norm) #generate movement CDF
  d = length(S)

  cdf = parallel::mclapply(X = 1:N,FUN = function(X,x,xCDF,M2,M1){
    j = sample(x = d,size = 1) # pick a random starting site
    sapply(X = x,FUN =  xCDF, M2=M2[j,], M1=M1[j,])
  },x=x,xCDF=xCDF,M2=M2,M1=M1,mc.cores=parallel::detectCores()-2L,mc.set.seed = TRUE)
  cdf = Reduce(f = rbind,x = cdf,init = NULL)

  xmx = max(cdfT[1], diff(cdfT), cdf[2,]-cdf[1,], cdf[3,]-cdf[2,])

  par(mfrow=c(1,2))
  colors = viridis::viridis(n=N)
  plot(x, cdfT, type = "l", xlab = "Distance", ylab = "CDF", ylim = c(0,1))
  grid()
  cdf = cdf[order(apply(cdf,MARGIN = 1,which.max)),] #sort rows by probability of long-range movement
  for(i in 1:N){
    lines(x, cdf[i,], col = colors[i])
  }
  lines(x,cdfT, lwd =3)

  #need to plot the max of the y values as ylin.
  plot(x, c(cdfT[1],diff(cdfT)), xlab = "Distance", ylab = "PDF", type = "l", ylim = c(0, 1), main = "Micro Search Kernels")
  grid()
  for(i in 1:N){
    lines(x, c(cdf[i,1],diff(cdf[i,])), col = colors[i])
  }
  lines(x,c(cdfT[1],diff(cdfT)), lwd =3)
  par(mfrow=c(1,1))
}
