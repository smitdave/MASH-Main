###############################################################################
#         _____                      __
#        / ___/___  ____ ___________/ /_
#        \__ \/ _ \/ __ `/ ___/ ___/ __ \
#      ___/ /  __/ /_/ / /  / /__/ / / /
#     /____/\___/\__,_/_/   \___/_/ /_/
#
#     MBITES-Search Kernels
#     MBITES Team
#     March 2018
#
###############################################################################


#' Search: Generate Movement Probabilities
#'
#' This function takes a distance matrix and returns a list where each element contains
#' movement probabilities from site i to all sites j!=i, and the integer id of those destination sites.
#'
#' @param dist numeric matrix with row and column names where elements are pairwise euclidean distance between sites.
#' @param w numeric vector of search weights for each site
#' @param sigma
#' @param eps
#' @param beta
#'
#' @export
search_PowerKernel <- function(dist, w, sigma=3, eps=0.1, beta=0){

  n = nrow(dist)
  if(length(w)!=n){stop("number of weights must equal dimensions of the distance matrix\n")}
  names = colnames(dist)
  moveProbs = vector(mode="list",length=n)

  for(i in 1:n){
    moveProbs[[i]]$move = w[-i]^(-beta * dist[i,-i]) * (eps + dist[i,-i])^-sigma
    moveProbs[[i]]$move = moveProbs[[i]]$move / sum(moveProbs[[i]]$move)
    moveProbs[[i]]$move_id = as.integer(names)[-i]
  }

  return(moveProbs)
}
