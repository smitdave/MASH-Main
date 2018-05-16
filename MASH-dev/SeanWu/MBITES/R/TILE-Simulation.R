###############################################################################
#       _______ __
#      /_  __(_) /__
#       / / / / / _ \
#      / / / / /  __/
#     /_/ /_/_/\___/
#
#     Tile-Simulation
#     MBITES Team
#     May 2018
#
###############################################################################

#' Tile: Daily Simulation
#'
#' Run daily simulation algorithm in following order:
#'    1. simulate human activity space
#'    2. simulate aquatic ecology (daily aquatic dynamics and add imagos to adult population)
#'    3. MBITES
#'    4. simulate human event queue
#'
oneDay_Tile <- function(){

  # human activity space simulation
  private$Humans$apply(tag="oneDay_ActivitySpace")

  # Aquatic Ecology
  private$Sites$apply(tag="oneDay_AquaticEcology")

  # MBITES
  private$Mosquitoes$apply(tag="MBITES")

  # human event queue simulation
  private$Humans$apply(tag="oneDay_EventQ")
}

Tile$set(which = "public",name = "oneDay",
    value = oneDay_Tile, overwrite = TRUE
)
