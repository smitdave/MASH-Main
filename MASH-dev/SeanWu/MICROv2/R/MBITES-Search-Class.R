###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MICRO
#   Search: MoveObject Class
#   MASH Team
#   January 2018
#
###############################################################################


# here: peri-domestic stuff
# near: normal movement
# far: NOT IMPLEMENTED (would go to other tiles or something)


###############################################################################
# Site Class
###############################################################################

#' MoveObject Class
#'
#' A movement object lives in a \code{\link{Site}} and returns a
#'
#'
#'
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: im an agument!
#'
#' @section **Methods**:
#'  * method: im a method!
#'
#' @section **Fields**:
#'  * field: im a field!
#'
#' @export
MoveObject <- R6::R6Class(classname = "MoveObject",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   #################################################
                   # Constructor
                   #################################################

                   initialize = function(){

                   },



                 ),

                 # private members
                 private = list(

                   # feeding site destinations
                   move_f = list(
                     here = numeric(1), # movement within a site
                     near = numeric(1), # movement within a tile
                     far = numeric(1), # movement between tiles

                     here_pr = numeric(1), # vector of probabilities to select feeding spots in this site
                     near_pr = numeric(1), # vector of probabilities to select sites within this tile
                     far_pr = numeric(1) # vector of probabilities to move between tiles
                   ),

                   # aquatic habitat destinations
                   move_l = list(
                     here = numeric(1), # movement within a site
                     near = numeric(1), # movement within a tile
                     far = numeric(1), # movement between tiles

                     here_pr = numeric(1), # vector of probabilities to select feeding spots in this site
                     near_pr = numeric(1), # vector of probabilities to select sites within this tile
                     far_pr = numeric(1) # vector of probabilities to move between tiles
                   ),

                   # mating site destinations
                   move_m = list(
                     here = numeric(1), # movement within a site
                     near = numeric(1), # movement within a tile
                     far = numeric(1), # movement between tiles

                     here_pr = numeric(1), # vector of probabilities to select feeding spots in this site
                     near_pr = numeric(1), # vector of probabilities to select sites within this tile
                     far_pr = numeric(1) # vector of probabilities to move between tiles
                   ),

                   # sugar site destinations
                   move_s = list(
                     here = numeric(1),
                     near = numeric(1),
                     far = numeric(1),
                   )

                 )
)
