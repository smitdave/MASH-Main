###############################################################################
#
#       ___   ____  __  _____
#     /   | / __ \/ / / /   |
#    / /| |/ / / / / / / /| |
#   / ___ / /_/ / /_/ / ___ |
#  /_/  |_\___\_\____/_/  |_|
#
#   MASH-MICRO
#   AQUATIC ECOLOGY: Seasonal Signal
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################


#' Aquatic Ecology: Generate Simple Seasonal Emergence
#'
#' Generate simple sinusoidally forced seasonal emergence.
#'
#' @param aquaPars a list of the following structure
#'  * N: number of aquatic habitats (required)
#'  * lambda: number of emerging adult females per human per day averaged over one year for the entire \code{\link{Landscape}} (required)
#'  * lambdaWeight: vector of weights applied to each site (if not specified or set to \code{NULL} initialize to Gamma(1,1) distribution)
#'  * offset: vector of seasonal offsets in peak emergence applied to each site (if not specified or set to \code{NULL} initialize to 0 for all sites)
#' @md
#' @return list \code{lambda} where each element is the daily emergence for that \code{\link{FeedingSite}}
#' @examples
#' makeLambda_MicroEmerge(aquaPars= list(lambda = c(2,3,4)))
#' @export
makeLambda_Emerge <- function(aquaPars){

  with(aquaPars,{

    if(!exists("lambdaWeight",inherits = FALSE) || is.null(lambdaWeight)){lambdaWeight = rgamma(n = N,shape = 1,rate = 1)}

    K = lambda*lambdaWeight / sum(lambdaWeight)
    if(!exists("offset",inherits = FALSE) || is.null(lambdaWeight)){offset = rep(0,length=N)}

    lambdaOut = vector(mode="list",length=N)
    for(ix in 1:N){
      lambdaOut[[ix]] = K[ix]*(1+sin(2*pi*(1:365-offset[ix])/365))
    }

    return(lambdaOut)
  })

}





# seasonF = function(t,par=c(A=1,m=1,O=0,k1=3,k2=1,Q=1)){with(as.list(par),{
#   # NOTE:
#   #  we use cosine rather than sine because cos(0) = 1
#   #  such that the parameter O references the seasonal peak
#
#   #  A = amplitude
#   #  m = mean; if m<1 negative numbers are set to zero
#   #  Q = number of peaks per year, can be < 1
#   #  O = the peak (relative to 0)
#   #  k1,k2  are shape parameters
#
#   pmax(0, A*(m + cos(2*Q*pi*(t-O)/365))^k1, 0)^k2
# })}
#
# sFi = function(i, t, PAR){
#   seasonF(t,PAR$pars[i,])
# }
#
# seasonGenF = function(t, PAR){
#   apply(sapply(1:PAR$N,sFi),1,prod,PAR=PAR)
# }
#
#
# #tt = c(0:7300)
#
# #plot(tt, seasonGenF(tt,PAR), type = "l")
#
# changePar = function(x,name){
#   switch(standardMenu("ask",
#     msg = paste("Changing ", name, "\n: Current value is :", name, "\n"),
#     ttl = "What do you want to do?\n",
#     menuText = c("Increase it by 10 percent", "Reduce it by 10 percent", "Input value"),
#     clrscrn =FALSE),
#     #------------------------
#     "1" = 1.1*x,
#     "2" = 0.9*x,
#     "3" = getNumber(),
#     "0" = x
#   )
# }
#
# setupSeasonF = function(params=c(A=1,m=1,O=0,k1=1,k2=2,Q=1)){
#
#   names = c("A", "m", "O", "k1", "k2", "Q")
#
#   ix = 1
#   while(ix > 0){
#    tt = seq(1:(365*ceiling(1/params[6]+.001)))
#    plot(tt, seasonF(tt, params), type = "l")
#    ix = switch(standardMenu("ask",
#      ttl = "Which parameter do you want to change?\n",
#      menuText = c(names, "No more changes"),
#      clrscrn = FALSE),
#      #------------------------
#      "1" = 1,
#      "2" = 2,
#      "3" = 3,
#      "4" = 4,
#      "5" = 5,
#      "6" = 6,
#      "7" = 0,
#      "0" = 1
#     )
#     if(ix>0) params[ix] = changePar(params[ix], names[ix])
#     with(as.list(par),
#       lines(tt, seasonF(tt, params), type = "l", lwd = 2)
#     )
#   }
#   params
# }
#
# #plotGenF = function(PAR){
# #   tt = seq(1:(365*ceiling(max(1/par[[6]]+.001))))
# #   par(mfrow = c(2,1))
# #   allParts = sapply(1:PAR$N,sFi, PAR=par)
# #   plot(tt, allParts[1,], ylim = range(0,allParts))
# #   for(i in 2:par$N){
# #     lines(tt, allParts[i,])
# #   }
# #   plot(tt,seasonGenF(tt, PAR), type = "l")
# #}
# #
# #setupGenF= function(
# #  par = list(
# #    N=3,
# #    pars = data.frame(cbind(
# #    A=c(1,3,1),
# #    m=c(1,.5,1),
# #    O=c(0,30,1),
# #    k1=c(1,1,1),
# #    k2=c(2,1,1),
# #    Q =c(1,2,.41))
# #
# # )
# #){
# #  ix = 1
# #  while(ix>0){
# #    plotGenF(PAR)
# #    cat("Which component do you want to change? (0 to accept current form)\n")
# #    N = getInteger()
# #  }
# #  par
# #}
#
#
#
# #par = setupSeasonF()
