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
#' @name Mating
NULL
#> NULL


###############################################################################
# Female mating behavior
###############################################################################

mbites_chooseMate <- function(){
  private$mate_res = private$site$sample_mate() # sample resources
  private$mateID = private$mate_res$MatingQ$sampleQ()
}

mbites_mating <- function(){
  # set flag
  private$mated = TRUE
  # check maturation
  if(private$energyPreG <= 0){
    private$mature = TRUE
  }
  # genetics?
}
