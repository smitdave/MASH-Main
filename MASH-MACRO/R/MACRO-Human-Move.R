###############################################################################
#       __  ___
#      /  |/  /___ _   _____
#     / /|_/ / __ \ | / / _ \
#    / /  / / /_/ / |/ /  __/
#   /_/  /_/\____/|___/\___/
#
#   MASH-MACRO
#   Movement
#   MASH Team
#   December 2017
#
###############################################################################

###############################################################################
# A trip is movement to another patch
# movement is movement within a patch
###############################################################################

#' Human: Take a Trip to a Patch
#'
#' Take a trip to a new \code{\link{MacroPatch}} and update the source and destination human biting weight
#'
#' @param patchID the id of the patch to travel to
#'
trip_Human <- function(patchID){
  self$decrement_bWeightHuman()
  private$patchID = patchID
  self$accumulate_bWeightHuman(patchID)
}
