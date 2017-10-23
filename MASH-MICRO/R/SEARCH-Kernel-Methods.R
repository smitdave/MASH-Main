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

#' MICRO Search Kernels: \code{\link{MosquitoPopFemale}} Sample MvOb
#'
#' This method is a helper for \code{\link{MicroKernel_moveMe}} and samples the appropriate MvOb in MvAll stored in an enclosing \code{\link{Tile}} object.
#'  * This function is bound to \code{MosquitoPopFemale$SampleMove}
#'
#' @param locNow: current site index \code{ix} of this mosquito
#' @param state: current behavioral state of this mosquito
#' @param inPointSet: current point set this mosquito is in
#' @return new index
#' @md
MicroKernel_SampleMvOb_MosquitoPopFemale <- function(locNow, state, pSetNow){

  MvOb = private$TilePointer$get_movement(locNow,state,pSetNow)

  x = runif(1)

  if(x <= MvOb$PR[1]){ # no movement
    return(MvOb$ix)
  } else {
    if(x <= MvOb$PR[1] + MvOb$PR[2]){ # near movement
      return(sampleIx_utility(x = MvOb$near$id, size = 1, prob = MvOb$near$pr))
    } else {
      if(x <= sum(MvOb$pr)){ # around movement
        stop("'around' movement not yet implemented")
      } else { # moveFar movement
        stop("'moveFar' movement not yet implemented")
      }
    }
  }
}

#' MICRO Search Kernels: \code{\link{MosquitoPopMale}} Sample MvOb
#'
#' This method is a helper for \code{\link{MicroKernel_moveMe}} and samples the appropriate MvOb in MvAll stored in an enclosing \code{\link{Tile}} object.
#'  * This function is bound to \code{MosquitoPopMale$SampleMove}
#'
#' @param locNow: current site index \code{locNow} of this mosquito
#' @param state: current behavioral state of this mosquito
#' @param inPointSet: current point set this mosquito is in
#' @return integer
#'
MicroKernel_SampleMvOb_MosquitoPopMale <- function(locNow, state, pSetNow){

  MvOb = private$TilePointer$get_movementMale(locNow,state,pSetNow)
  sampleIx_utility(x = MvOb$near$id, size = 1, prob = MvOb$near$pr)

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

  switch(private$state,
    F = {
      private$locNow = private$FemalePopPointer$SampleMove(locNow=private$locNow,state=private$state,pSetNow=private$pSetNow)
      private$pSetNow = "f"
    },
    L = {
      private$locNow = private$FemalePopPointer$SampleMove(locNow=private$locNow,state=private$state,pSetNow=private$pSetNow)
      private$pSetNow = "l"
    },
    M = {
      private$locNow = private$FemalePopPointer$SampleMove(locNow=private$locNow,state=private$state,pSetNow=private$pSetNow)
      private$pSetNow = "m"
    },
    S = {
      private$locNow = private$FemalePopPointer$SampleMove(locNow=private$locNow,state=private$state,pSetNow=private$pSetNow)
      private$pSetNow = "s"
    },
    {return(NULL)}
  )

}

#' MICRO Search Kernels: \code{\link{MosquitoFemale}} Movement Function for Full M-BITES Lifecycle Model
#'
#' Move one mosquito based on site and next behavioral state.
#'  * This method is bound to \code{MosquitoFemale$moveMe()}
#'
MicroKernel_moveMe_BRO <- function(){

    switch(private$state,
      B = {
        private$locNow = private$FemalePopPointer$SampleMove(locNow=private$locNow,state=private$state,pSetNow=private$pSetNow)
        private$pSetNow = "f"
      },
      O = {
        private$locNow = private$FemalePopPointer$SampleMove(locNow=private$locNow,state=private$state,pSetNow=private$pSetNow)
        private$pSetNow = "l"
      },
      M = {
        private$locNow = private$FemalePopPointer$SampleMove(locNow=private$locNow,state=private$state,pSetNow=private$pSetNow)
        private$pSetNow = "m"
      },
      S = {
        private$locNow = private$FemalePopPointer$SampleMove(locNow=private$locNow,state=private$state,pSetNow=private$pSetNow)
        private$pSetNow = "s"
      },
      {return(NULL)}
    )

}

#' MICRO Search Kernels: \code{\link{MosquitoMale}} Movement Function for Full M-BITES Lifecycle Model
#'
#' Move one mosquito based on site and next behavioral state. This method is bound to \code{MosquitoMale$moveMe()}
#'
MicroKernel_moveMe_Male <- function(){

  switch(private$state,

      M = {
          private$locNow = private$MalePopPointer$SampleMove(locNow=private$locNow,state=private$state,pSetNow=private$pSetNow)
          private$pSetNow = "m"
        },
      S = {
          private$locNow = private$MalePopPointer$SampleMove(locNow=private$locNow,state=private$state,pSetNow=private$pSetNow)
          private$pSetNow = "s"
        },
      {return(NULL)}
    )

}


###############################################################################
# MicroTile 'get_movement' Methods
###############################################################################

#' MICRO Search Kernels: \code{\link{MicroTile}} Access MvAll Object for Female Full M-BITES Lifecycle Model
#'
#' Replace generic \code{MicroTile$get_movement()} method for MicroKernel module, return a MvOb from \code{movementFemale} field of a microsimulation tile.
#'  *  This method bound to \code{MicroTile$get_movement}
#'
get_MicroKernel_movement_FULL<- function(locNow,state,pSetNow){
  switch(state,
    F = {
        if(pSetNow=="f"){return(private$movementFemale$F2F[[locNow]])}
        if(pSetNow=="s"){return(private$movementFemale$S2F[[locNow]])}
        if(pSetNow=="m"){return(private$movementFemale$M2F[[locNow]])}
        if(pSetNow=="l"){return(private$movementFemale$L2F[[locNow]])}
      },
    L = {
        if(pSetNow=="f"){return(private$movementFemale$F2L[[locNow]])}
        if(pSetNow=="s"){return(private$movementFemale$S2L[[locNow]])}
        if(pSetNow=="m"){return(private$movementFemale$M2L[[locNow]])}
        if(pSetNow=="l"){return(private$movementFemale$L2L[[locNow]])}
      },
    S = {
        if(pSetNow=="f"){return(private$movementFemale$F2S[[locNow]])}
        if(pSetNow=="s"){return(private$movementFemale$S2S[[locNow]])}
        if(pSetNow=="m"){return(private$movementFemale$M2S[[locNow]])}
        if(pSetNow=="l"){return(private$movementFemale$L2S[[locNow]])}
      },
    M = {
        if(pSetNow=="f"){return(private$movementFemale$F2M[[locNow]])}
        if(pSetNow=="s"){return(private$movementFemale$S2M[[locNow]])}
        if(pSetNow=="m"){return(private$movementFemale$M2M[[locNow]])}
        if(pSetNow=="l"){return(private$movementFemale$L2M[[locNow]])}
      },
    {return(NULL)}
  )
}

#' MICRO Search Kernels: \code{\link{MicroTile}} Access MvAll Object for Female M-BITES BRO Lifecycle Model
#'
#' Replace generic \code{MicroTile$get_movement()} method for MicroKernel module, return a MvOb from \code{movementFemale} field of a microsimulation tile.
#'  *  This method bound to \code{MicroTile$get_movement}
#'
get_MicroKernel_movement_BRO <- function(locNow,state,pSetNow){

  switch(state,
    B = {
        if(pSetNow=="f"){return(private$movementFemale$F2F[[locNow]])}
        if(pSetNow=="m"){return(private$movementFemale$M2F[[locNow]])}
        if(pSetNow=="s"){return(private$movementFemale$S2F[[locNow]])}
        if(pSetNow=="l"){return(private$movementFemale$L2F[[locNow]])}
      },
    O = {
        if(pSetNow=="f"){return(private$movementFemale$F2L[[locNow]])}
        if(pSetNow=="m"){return(private$movementFemale$M2L[[locNow]])}
        if(pSetNow=="s"){return(private$movementFemale$S2L[[locNow]])}
        if(pSetNow=="l"){return(private$movementFemale$L2L[[locNow]])}
      },
    M = {
        if(pSetNow=="f"){return(private$movementFemale$F2M[[locNow]])}
        if(pSetNow=="m"){return(private$movementFemale$M2M[[locNow]])}
        if(pSetNow=="s"){return(private$movementFemale$S2M[[locNow]])}
        if(pSetNow=="l"){return(private$movementFemale$L2M[[locNow]])}
      },
    S = {
        if(pSetNow=="f"){return(private$movementFemale$F2M[[locNow]])}
        if(pSetNow=="m"){return(private$movementFemale$M2M[[locNow]])}
        if(pSetNow=="s"){return(private$movementFemale$S2M[[locNow]])}
        if(pSetNow=="l"){return(private$movementFemale$L2M[[locNow]])}
    },
    {return(NULL)}
  )
}

#' MICRO Search Kernels: \code{\link{MicroTile}} Access MvAll Object for Male M-BITES Lifecycle Models
#'
#' Replace generic \code{MicroTile$get_movement()} method for MicroKernel module, return a MvOb from \code{movementMale} field of a microsimulation tile.
#'  *  This method bound to \code{MicroTile$get_movementMale}
#'
get_MicroKernel_movement_Male <- function(locNow,state,pSetNow){
  switch(state,
    M = {
        if(pSetNow=="l"){return(private$movementMale$L2M[[locNow]])}
        if(pSetNow=="m"){return(private$movementMale$M2M[[locNow]])}
        if(pSetNow=="s"){return(private$movementMale$S2M[[locNow]])}
      },
    S = {
        if(pSetNow=="l"){return(private$movementMale$L2S[[locNow]])}
        if(pSetNow=="m"){return(private$movementMale$M2S[[locNow]])}
        if(pSetNow=="s"){return(private$movementMale$S2S[[locNow]])}
      },
    {return(NULL)}
  )
}
