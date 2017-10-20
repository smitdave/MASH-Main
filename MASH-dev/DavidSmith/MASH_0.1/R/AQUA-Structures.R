#################################################################
#
#   MASH
#   R6-ified
#   Generic Definitions for Aquatic Ecology modules
#   David Smith, Hector Sanchez, Sean Wu
#   May 24, 2016
#
#################################################################


#################################################################
# Imago Queue
#################################################################

#' Generate a ImagoQ Slot
#'
#' Generate a new batch of emerging mosquitoes for a slot in an ImagoQ. It can be called from \code{\link{allocImagoQ}}.
#'
#' @param N number of emerging adults
#' @param tEmerge time this batch will emerge
#' @param damID ID of mother
#' @param sireID ID of father
#' @param genotype integer genotype identifier
#'
#' @section Structure:
#' * N number of emerging adults
#' * tEmerge time of emergence
#' * damID ID of mother
#' * sireID ID of father
#' * genotype integer genotype identifier
#' @md
#'
#' @return a named list
#' @examples
#' newImago()
#' @export
newImago <- function(N = 0L, tEmerge = 0L, damID = 0L, sireID = 0L, genotype = NULL){
  list(
    N=N,
    tEmerge=tEmerge,
    damID=damID,
    sireID=sireID,
    genotype=genotype
    )
}

#' Allocate ImagoQ
#'
#' This function allocates an empty ImagoQ
#'
#' @param N size of empty queue
#' @return empty ImagoQ
#' @examples
#' allocImagoQ(N=N)
#' @export
allocImagoQ <- function(N){
  return(replicate(n=N,expr=newImago(),simplify=FALSE))
}


#################################################################
# Egg Queue
#################################################################

#' Empty Egg Batch Slot for EggQ
#'
#' This function is a low-level utility to generate a null placeholder value for an egg batch in EggQ.
#' It is called by \code{\link{allocEggQ}} and \code{\link{extendEggQ}}.
#'
#' @param N number of eggs in this batch
#' @param tOviposit time of oviposition
#' @param damID ID of mother
#' @param sireID ID of father
#' @param genotype placeholder
#'
#' @section Structure:
#' * N number of emerging adults
#' * tOviposit time of oviposition
#' * damID ID of mother
#' * sireID ID of father
#' * genotype integer genotype identifier
#' @md
#'
#' @return a named list
#' @examples
#' newEgg()
#' @export
newEgg <- function(N = 0L, tOviposit = 0L, damID = 0L, sireID = 0L, genotype = NULL){
  list(
    N=N,
    tOviposit=tOviposit,
    damID=damID,
    sireID=sireID,
    genotype=genotype
    )
}

#' Allocate EggQ
#'
#' This function allocates an empty EggQ
#'
#' @param N size of empty queue
#' @return empty EggQ
#' @examples
#' allocEggQ(N=N)
#' @export
allocEggQ = function(N){
  return(replicate(n = N,expr = newEgg(),simplify = FALSE))
}
