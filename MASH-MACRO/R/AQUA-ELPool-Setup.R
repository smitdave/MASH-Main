###############################################################################
#
#       ___   ____  __  _____
#     /   | / __ \/ / / /   |
#    / /| |/ / / / / / / /| |
#   / ___ / /_/ / /_/ / ___ |
#  /_/  |_\___\_\____/_/  |_|
#
#   MASH-MACRO
#   AQUATIC ECOLOGY: MosquitoRM Setup
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   August 21, 2017
#
###############################################################################

# eg: AQUA.EL4P.SETUP ... etc

AQUA.ELPool.SETUP <- function(overwrite = TRUE){

  HumanPop$set(which = "public",name = "get_ELPool",
    value = get_ELPool_MacroPatch,
    overwrite = TRUE)

  HumanPop$set(which = "public",name = "set_ELPool",
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
