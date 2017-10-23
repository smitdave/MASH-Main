####################################################################################
#
#   MASH
#   R6-ified
#   Aquatic Ecology Utility Functions
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   July 19, 2017
#
####################################################################################


####################################################################################
# Emerge
####################################################################################

#' Plot Lambda for Emerge Module
#'
#' Given a list of vectors for lambda across sites, plot emergence.
#'
#' @param lambda a list of vectors
#' @examples
#' AquaEmergeLambdaPlot_utility(lambda = makeLambda_Macro(aquaPars = list(N = 10, lambda = 10)))
#' @export
AquaEmergeLambdaPlot_utility <- function(lambda){
  pcol = viridis::viridis(n = length(lambda))
  ylim = range(unlist(lambda))
  plot(1:365,type="n",ylim = ylim, ylab = expression(lambda[i]), xlab = "Time (Days)")
  grid()
  for(ix in 1:length(lambda)){
    lines(1:365, lambda[[ix]], col=pcol[ix])
  }
}
