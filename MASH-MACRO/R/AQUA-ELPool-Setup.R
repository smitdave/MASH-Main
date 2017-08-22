###############################################################################
#
#       ___   ____  __  _____
#     /   | / __ \/ / / /   |
#    / /| |/ / / / / / / /| |
#   / ___ / /_/ / /_/ / ___ |
#  /_/  |_\___\_\____/_/  |_|
#
#   MASH-MACRO
#   AQUATIC ECOLOGY: ELPool Setup
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   August 21, 2017
#
###############################################################################

# eg: AQUA.EL4P.SETUP ... etc

#' Aquatic Ecology: ELPool Setup
#'
#' Initialize methods.
#'
#' @param overwrite overwrite existing methods
#'
#'
#' @section Classes:
#'  * MacroPatch: \code{\link{get_ELPool_MacroPatch}} bound to \code{MacroPatch$get_ELPool}
#'  * MacroPatch: \code{\link{set_ELPool_MacroPatch}} bound to \code{MacroPatch$set_ELPool}
#'
#' @export
AQUA.ELPool.SETUP <- function(overwrite = TRUE){

  MacroPatch$set(which = "public",name = "get_ELPool",
    value = get_ELPool_MacroPatch,
    overwrite = TRUE)

  MacroPatch$set(which = "public",name = "set_ELPool",
    value = set_ELPool_MacroPatch,
    overwrite = TRUE)

  # HumanPop$set(which = "public",name = "get_history",
  #   value = get_history_HumanPop,
  #   overwrite = TRUE)
  #
  # HumanPop$set(which = "public",name = "get_history",
  #   value = get_history_HumanPop,
  #   overwrite = TRUE)
  #
  # HumanPop$set(which = "public",name = "get_history",
  #   value = get_history_HumanPop,
  #   overwrite = TRUE)
  #
  # HumanPop$set(which = "public",name = "get_history",
  #   value = get_history_HumanPop,
  #   overwrite = TRUE)
  #
  # HumanPop$set(which = "public",name = "get_history",
  #   value = get_history_HumanPop,
  #   overwrite = TRUE)
  #
  # HumanPop$set(which = "public",name = "get_history",
  #   value = get_history_HumanPop,
  #   overwrite = TRUE)

}
