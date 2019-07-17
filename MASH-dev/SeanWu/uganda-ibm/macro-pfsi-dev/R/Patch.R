################################################################################
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   Patch constructor parameters
#
#   Sean Wu
#   December 2018
#
################################################################################

#' Patch: Constructor Parameters
#'
#' Generates a single named list with parameters to construct a
#' 'patch' object (see inst/include/Patch.hpp for constructor parameters).

#'
#' @param id integer id
#' @param move 'row' of the stochastic movement matri
#' @param bWeightZoo biting weight assigned to non-human hosts
#' @param bWeightZootox biting weight assigned to endectocide-treated non-human hosts
#' @param reservoir boolean; is this patch a reservoir? (no simulation of mosquitos)
#' @param res_EIR fixed EIR for reservoir patch
#'
#' @export
patch_conpars <- function(id,move,bWeightZoo,bWeightZootox,reservoir,res_EIR){

  if(!is.integer(id) | id < 0){
    stop(paste0("id: ",id," not allowed; use non-negative integer id"))
  }

  if(!isTRUE(all.equal(sum(move),1))){
    stop("movement vector 'move' must sum to one")
  }

  if(reservoir & (!is.numeric(res_EIR) | (res_EIR < 0))){
    stop("reservoir patches must have non-negative float values for 'res_EIR'")
  }

  list(
    id = as.integer(id),
    move = as.numeric(move),
    bWeightZoo = as.numeric(bWeightZoo),
    bWeightZootox = as.numeric(bWeightZootox),
    reservoir = as.logical(reservoir),
    res_EIR = as.numeric(res_EIR)
  )
}


#' Patch: Constructor Parameters for all Patches
#'
#' Generates a list with parameters to construct all patches by calling \code{\link{patch_conpars}}
#' repeatedly. Note that vectors 'reservoir' and 'res_EIR' must be given for all patches (even non-reservoir patches)
#'
#' @param move stochastic movement matrix
#' @param bWeightZoo biting weight assigned to non-human hosts
#' @param bWeightZootox biting weight assigned to endectocide-treated non-human hosts
#' @param reservoir boolean; is this patch a reservoir? (no simulation of mosquitos)
#' @param res_EIR fixed EIR for reservoir patch
#'
#' @export
patches_parameters <- function(move,bWeightZoo,bWeightZootox,reservoir,res_EIR){

  if(nrow(move) != ncol(move)){
    stop("matrix 'move' must be a row-normalized stochastic matrix")
  }

  if(any(diag(move) > 0)){
    warnings("non-zero diagonal elements detected in 'move' matrix; make sure you really want this!")
  }

  n <- nrow(move)
  id <- as.integer(0:(n-1))

  patches <- lapply(id,function(x){
    patch_conpars(x,move[x+1,],bWeightZoo[x+1],bWeightZootox[x+1],reservoir[x+1],res_EIR[x+1])
  })

  return(patches)
}
