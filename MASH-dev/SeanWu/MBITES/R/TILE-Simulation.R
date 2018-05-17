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

  # clear activity space
  private$Sites$apply(tag="clear_ActivitySpace")

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

#' reset all the landscape things on the tile between runs
reset_Tile <- function(){
  # reset all sites
  private$Sites$apply(tag="reset")
  # reset humans
  private$Humans$rm_all()
  # reset mosquitoes
  private$Mosquitoes$rm_all()
}

Tile$set(which = "public",name = "reset",
    value = reset_Tile, overwrite = TRUE
)
