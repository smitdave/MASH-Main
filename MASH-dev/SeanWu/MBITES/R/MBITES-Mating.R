###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Mating
#     MBITES Team
#     March 2018
#
###############################################################################

#' MBITES: Mating & Swarming Behavior
#'
#' write about me!
#'  * male swarming behavior
#'  * female mate seeking behavior
#'
#' To disable mating, \code{\link{mbites_findSwarmNull}} must be enabled.
#'
#' @name Mating
NULL
#> NULL


###############################################################################
# Female mating behavior
###############################################################################

#' MBITES: Choose a Mate
#'
#' During the mating bout, a female mosquito chooses a mate from the mating queue closure
#' at the mating site, see \code{\link{make_MatingQ}} for details on the mating queue.
#'
#'  * This method is bound to \code{Mosquito_Female$chooseMate}.
#'
mbites_chooseMate <- function(){
  private$mate_res = private$site$sample_mate() # sample resources
  private$mateID = private$mate_res$MatingQ$sampleQ()
}

#' MBITES: Mating
#'
#' If the female mosquito successfully found a mate, set \code{mated = TRUE} and check if the mosquito has fulfilled
#' the pre-gonotrophic energy requirement, and set \code{mature = TRUE} if sufficient energy has been acquired.
#'
#'  * This method is bound to \code{Mosquito_Female$mating}.
#'
mbites_mating <- function(){
  # set flag
  private$mated = TRUE
  # check maturation
  if(private$energyPreG <= 0){
    private$mature = TRUE
  }
  # genetics here
}

# set methods
Mosquito_Female$set(which = "public",name = "chooseMate",
    value = mbites_chooseMate, overwrite = TRUE
)

Mosquito_Female$set(which = "public",name = "mating",
    value = mbites_mating, overwrite = TRUE
)
