###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MICRO
#   Search: MoveObject Class Declaration
#   MASH Team
#   January 2018
#
###############################################################################

###############################################################################
# MoveObject Abstract Base Class
###############################################################################

#' Movement Object Abstract Base Class
#'
#' A movement object lives in a \code{\link{Tile}}
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
#'  * move(ix, dest): this function must return a (possibly the same) site index and distance flown to calculate wing tattering
#'    * ix: index of \code{\link{Site}} the mosquito is currently at
#'    * dest: character in {f,l,m,s} indicating the resource the mosquito is trying to get to
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

                   # Constructor
                   initialize = function(){
                     stop("constructor for MoveObject abstract base class should never be called")
                   },

                   # Methods
                   move = function(ix, dest){
                     stop("move for MoveObject abstract base class should never be called")
                   }

                 ),

                 # private members
                 private = list(

                   # reference to containing tile
                   tileP        = NULL # tile* tileP

                 )
)


###############################################################################
# Search: Kernel
###############################################################################

#' Movement Probabilities for 'Kernel'
#'
#' Gives relevant movement information called in \code{\link{move_MoveObject}} for
#' a single \code{\link{Site}}.
#'  * here: peri-domestic movement (multiple resources found at same site)
#'  * near: movement within the same tile
#'  * far: movement between tiles
#'
move_obj_Kernel <- list(
  here_f = numeric(1), # movement within a site
  here_l = numeric(1), # movement within a site
  here_m = numeric(1), # movement within a site
  here_s = numeric(1), # movement within a site

  near = numeric(1), # movement within a tile
  far = numeric(1), # movement between tiles

  near_ix = integer(1), # vector of indices to select sites within this tile
  far_ix = integer(1), # vector of indices to move between tiles

  near_pr = numeric(1), # vector of probabilities to select sites within this tile
  far_pr = numeric(1), # vector of probabilities to move between tiles

  here_dist = numeric(1), # distance for movement within a site
  near_dist = numeric(1), # vector of distances for movement within this tile
  far_dist = numeric(1) # vector of distances for movement between tiles
)

# this would be a typedef struct move_obj{};

#' Movement Object Kernel Class
#'
#' A movement object lives in a \code{\link{Tile}}
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
#'  * move(ix, dest): this function must return a (possibly the same) site index and distance flown to calculate wing tattering
#'    * ix: index of \code{\link{Site}} the mosquito is currently at
#'    * dest: character in {f,l,m,s} indicating the resource the mosquito is trying to get to
#'
#' @section **Fields**:
#'  * field: im a field!
#'
#' @export
MoveObject_Kernel <- R6::R6Class(classname = "MoveObject_Kernel",
                 inherit = MoveObject,
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   # Constructor
                   initialize = function(){

                   }

                   # Methods

                 ),

                 # private members
                 private = list(

                   # reference to containing tile
                   tileP        = NULL # tile* tileP

                 )
)
