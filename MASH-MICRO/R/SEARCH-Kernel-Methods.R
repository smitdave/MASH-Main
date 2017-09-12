###############################################################################
#      _____                      __
#     / ___/___  ____ ___________/ /_
#     \__ \/ _ \/ __ `/ ___/ ___/ __ \
#    ___/ /  __/ /_/ / /  / /__/ / / /
#   /____/\___/\__,_/_/   \___/_/ /_/
#
#   MASH-MICRO
#   SEARCH: Kernel Methods
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################


###############################################################################
# 'MicroKernel_SampleMvOb' Method
###############################################################################

#' MICRO Search Kernels: \code{\link{MicroMosquito}} Sample MvOb
#'
#' This method is a helper for \code{\link{MicroKernel_moveMe}} and samples the appropriate MvOb in MvAll stored in an enclosing \code{\link{Tile}} object.
#'  * This function is bound to \code{MosquitoPopFemale$SampleMove} and \code{MosquitoPopMale$SampleMove}
#'
#' @param ixS: current site index \code{ix} of this mosquito
#' @param state: current behavioral state of this mosquito
#' @param inPointSet: current point set this mosquito is in
#' @return new index
#' @md
MicroKernel_SampleMvOb_MosquitoPop <- function(ixS, state, inPointSet){

  MvOb = private$TilePointer$get_movement(ixS,state,inPointSet)

  x = runif(1)

  if(x <= MvOb$PR[1]){ # no movement
    return(MvOb$ix)
  } else {
    if(x <= MvOb$PR[1] + MvOb$PR[2]){ # near movement
      # ixNear = sample(x = length(MvOb$near$id),size = 1,prob = MvOb$near$pr)
      # return(MvOb$near$id[ixNear])
      return(
        sampleIx_utility(x = MvOb$near$id, size = 1, prob = MvOb$near$pr)
      )
    } else {
      if(x <= sum(MvOb$pr)){ # around movement
        stop("'around' movement not yet implemented")
      } else { # moveFar movement
        stop("'moveFar' movement not yet implemented")
      }
    }
  }
}


###############################################################################
# MosquitoFemale and MosquitoMale 'moveMe' Methods
###############################################################################

#' MICRO Search Kernels: \code{\link{MosquitoFemale}} Movement Function for Full M-BITES Lifecycle Model
#'
#' Move one mosquito based on site and next behavioral state.
#'  * This method is bound to \code{MosquitoFemale$moveMe()}
#'
MicroKernel_moveMe_FULL <- function(){

  # pSetNew = switch(private$state,
  #     F = {"f"},
  #     L = {"l"},
  #     S = {"s"},
  #     M = {"m"},
  #     {private$inPointSet}
  #   )
  #
  # private$ix = private$FemalePopPointer$SampleMove(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
  # private$inPointSet = pSetNew

}

#' MICRO Search Kernels: \code{\link{MosquitoFemale}} Movement Function for Full M-BITES Lifecycle Model
#'
#' Move one mosquito based on site and next behavioral state.
#'  * This method is bound to \code{MosquitoFemale$moveMe()}
#'
MicroKernel_moveMe_BRO <- function(){

  switch(private$state,

      B = {
          private$ix = private$FemalePopPointer$SampleMove(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
          private$inPointSet = "f"
        },
      O = {
          private$ix = private$FemalePopPointer$SampleMove(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
          private$inPointSet = "l"
        },
      {return(NULL)}
    )

}

#' MICRO Search Kernels: \code{\link{MosquitoFemale}} Movement Function for Full M-BITES Lifecycle Model
#'
#' Move one mosquito based on site and next behavioral state. This method is bound to \code{MosquitoFemale$moveMe()}
#'
MicroKernel_moveMe_BROM <- function(){

  # pSetNew = switch(private$state,
  #     B = {"f"},
  #     O = {"l"},
  #     M = {"m"},
  #     {private$inPointSet}
  #   )
  #
  # private$ix = private$FemalePopPointer$SampleMove(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
  # private$inPointSet = pSetNew

}

#' MICRO Search Kernels: \code{\link{MosquitoMale}} Movement Function for Full M-BITES Lifecycle Model
#'
#' Move one mosquito based on site and next behavioral state. This method is bound to \code{MosquitoMale$moveMe()}
#'
MicroKernel_moveMe_Male <- function(){

  # private$ix = private$FemalePopPointer$SampleMove(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
  #
  # switch(private$state,
  #   M = {private$inPointSet = "m"},
  #   S = {private$inPointSet = "s"}
  # )
  cat("if you are seeing me then Sean needs to fix movement for males: he needs to make get_MicroKernel_movement_Male male specific and make MicroKernel_SampleMvOb_MosquitoPop for males\n",sep="")
  pSetNew = switch(private$state,
      M = {"m"},
      S = {"s"},
      {private$inPointSet}
    )

  private$ix = private$FemalePopPointer$SampleMove(ixS = private$ix, state = private$state, inPointSet = private$inPointSet)
  private$inPointSet = pSetNew
}


###############################################################################
# MicroTile 'get_movement' Methods
###############################################################################

#' MICRO Search Kernels: \code{\link{MicroTile}} Access MvAll Object for Female Full M-BITES Lifecycle Model
#'
#' Replace generic \code{MicroTile$get_movement()} method for MicroKernel module, return a MvOb from \code{movementFemale} field of a microsimulation tile.
#'  *  This method bound to \code{MicroTile$get_movement}
#' @md
get_MicroKernel_movement_FULL<- function(ixS, state, inPointSet){
  switch(state,
    F = {
        if(inPointSet=="f"){return(private$movementFemale$F2F[[ixS]])}
        if(inPointSet=="s"){return(private$movementFemale$S2F[[ixS]])}
        if(inPointSet=="m"){return(private$movementFemale$M2F[[ixS]])}
        if(inPointSet=="l"){return(private$movementFemale$L2F[[ixS]])}
      },
    L = {
        if(inPointSet=="f"){return(private$movementFemale$F2L[[ixS]])}
        if(inPointSet=="s"){return(private$movementFemale$S2L[[ixS]])}
        if(inPointSet=="m"){return(private$movementFemale$M2L[[ixS]])}
        if(inPointSet=="l"){return(private$movementFemale$L2L[[ixS]])}
      },
    S = {
        if(inPointSet=="f"){return(private$movementFemale$F2S[[ixS]])}
        if(inPointSet=="s"){return(private$movementFemale$S2S[[ixS]])}
        if(inPointSet=="m"){return(private$movementFemale$M2S[[ixS]])}
        if(inPointSet=="l"){return(private$movementFemale$L2S[[ixS]])}
      },
    M = {
        if(inPointSet=="f"){return(private$movementFemale$F2M[[ixS]])}
        if(inPointSet=="s"){return(private$movementFemale$S2M[[ixS]])}
        if(inPointSet=="m"){return(private$movementFemale$M2M[[ixS]])}
        if(inPointSet=="l"){return(private$movementFemale$L2M[[ixS]])}
      },
    {return(NULL)}
  )
}

#' MICRO Search Kernels: \code{\link{MicroTile}} Access MvAll Object for Female M-BITES BRO Lifecycle Model
#'
#' Replace generic \code{MicroTile$get_movement()} method for MicroKernel module, return a MvOb from \code{movementFemale} field of a microsimulation tile.
#'  *  This method bound to \code{MicroTile$get_movement}
#' @md
get_MicroKernel_movement_BRO <- function(ixS, state, inPointSet){
  switch(state,
    B = {
        if(inPointSet=="f"){return(private$movementFemale$F2F[[ixS]])}
        if(inPointSet=="l"){return(private$movementFemale$L2F[[ixS]])}
      },
    O = {
        if(inPointSet=="f"){return(private$movementFemale$F2L[[ixS]])}
        if(inPointSet=="l"){return(private$movementFemale$L2L[[ixS]])}
      },
    {return(NULL)}
  )}

#' MICRO Search Kernels: \code{\link{MicroTile}} Access MvAll Object for Female M-BITES BROM Lifecycle Model
#'
#' Replace generic \code{MicroTile$get_movement()} method for MicroKernel module, return a MvOb from \code{movementFemale} field of a microsimulation tile.
#'  *  This method bound to \code{MicroTile$get_movement}
#' @md
get_MicroKernel_movement_BROM <- function(ixS, state, inPointSet){
  switch(state,
    B = {
        if(inPointSet=="f"){return(private$movementFemale$F2F[[ixS]])}
        if(inPointSet=="m"){return(private$movementFemale$M2F[[ixS]])}
        if(inPointSet=="l"){return(private$movementFemale$L2F[[ixS]])}
      },
    O = {
        if(inPointSet=="f"){return(private$movementFemale$F2L[[ixS]])}
        if(inPointSet=="m"){return(private$movementFemale$M2L[[ixS]])}
        if(inPointSet=="l"){return(private$movementFemale$L2L[[ixS]])}
      },
    M = {
        if(inPointSet=="f"){return(private$movementFemale$F2M[[ixS]])}
        if(inPointSet=="m"){return(private$movementFemale$M2M[[ixS]])}
        if(inPointSet=="l"){return(private$movementFemale$L2M[[ixS]])}
      },
    {return(NULL)}
  )
}

#' MICRO Search Kernels: \code{\link{MicroTile}} Access MvAll Object for Male M-BITES Lifecycle Models
#'
#' Replace generic \code{MicroTile$get_movement()} method for MicroKernel module, return a MvOb from \code{movementMale} field of a microsimulation tile.
#'  *  This method bound to \code{MicroTile$get_movementMale}
#' @md
get_MicroKernel_movement_Male <- function(ixS, state, inPointSet){
  switch(state,
    M = {
        if(inPointSet=="m"){return(private$movementMale$M2M[[ixS]])}
        if(inPointSet=="s"){return(private$movementMale$S2M[[ixS]])}
      },
    S = {
        if(inPointSet=="m"){return(private$movementMale$M2S[[ixS]])}
        if(inPointSet=="s"){return(private$movementMale$S2S[[ixS]])}
      },
    {return(NULL)}
  )
}
