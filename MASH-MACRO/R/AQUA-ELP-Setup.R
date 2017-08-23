###############################################################################
#
#       ___   ____  __  _____
#     /   | / __ \/ / / /   |
#    / /| |/ / / / / / / /| |
#   / ___ / /_/ / /_/ / ___ |
#  /_/  |_\___\_\____/_/  |_|
#
#   MASH-MACRO
#   AQUATIC ECOLOGY: ELP Setup
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   August 21, 2017
#
###############################################################################

# eg: AQUA.EL4P.SETUP ... etc

#' Aquatic Ecology: ELP Setup
#'
#' Initialize methods.
#'
#' @param overwrite overwrite existing methods
#'
#'
#' @section Classes:
#'  * MacroPatch: \code{\link{get_ELP_MacroPatch}} bound to \code{MacroPatch$get_ELP}
#'  * MacroPatch: \code{\link{set_ELP_MacroPatch}} bound to \code{MacroPatch$set_ELP}
#'
#' @export
AQUA.ELP.SETUP <- function(overwrite = TRUE){

  MacroPatch$set(which = "public",name = "get_ELP",
    value = get_ELP_MacroPatch,
    overwrite = TRUE)

  MacroPatch$set(which = "public",name = "set_ELP",
    value = set_ELP_MacroPatch,
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
