###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MICRO
#   MBITES: Search Utilities
#   MASH Team
#   January 2018
#
###############################################################################

#################################################################
# Exponential Kernels for Energy/Wing Tattering
#################################################################

#' MICRO Kernels: Fit Exponential Lambda
#'
#' Fit an exponential distribution by quantile such that 'quantile' amount of activity
#' occurs at less than or equal to input 'distance' d. Returns fitted rate parameter fitted from \code{\link[base]{optimise}}.
#'
#' @param d numeric distance
#' @param q numeric quantile (must be between 0 and 1)
#' @param up numeric upper limit on rate parameter (can be set to 1/(max distance anything could happen at))
#'
#' @return numeric; fitted value of lambda (rate parameter)
#' @export
MicroKernel_FitExpCDF <- function(d,q,up=1){
  f_opt = function(x){
    abs(d - qexp(p = q,rate = x))
  }
  sol = optimise(f = f_opt,lower = 0,upper = up,maximum = FALSE)
  return(sol$minimum)
}

#' MICRO Kernels: Fit Beta Parameters
#'
#' Fit parameters of a Beta distribution such that it will have a given mean and coefficient of variation.
#'
#' @param mean desired mean of Beta distribution (must be between 0 and 1)
#' @param cv desired coefficient of variation of Beta distribution (0,inf)
#'
#' @return list; alpha (shape1) and beta (shape2) parameters of Beta distribution
#' @export
MicroKernel_FitBeta <- function(mean,cv){
  alpha = (1-mean-(mean*(cv^2))) / (cv^2)
  beta = ((mean-1)*(-1+mean+(mean*(cv^2)))) / (mean*(cv^2))
  return(list(alpha=alpha,beta=beta))
}

#################################################################
# SEARCH-MicroKernels
#################################################################


#' MICRO Search Kernels: Plot Kernel Functions
#'
#' Visualize kernel between a set of starting sites and destination sites.
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
