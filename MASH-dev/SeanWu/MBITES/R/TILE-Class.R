###############################################################################
#       _______ __
#      /_  __(_) /__
#       / / / / / _ \
#      / / / / /  __/
#     /_/ /_/_/\___/
#
#     Tile-Class
#     MBITES Team
#     February 2018
#
###############################################################################

#' Tile Class
#'
#' A \code{Tile} consists of a set of \code{\link[MBITES]{Site}} objects defininng where events occur,
#' and the agents that enact the dynamics on the tile.
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
#'  * method: i'm a method!
#'
#' @section **Fields**:
#'  * field: i'm a field!
#'
#' @export
Tile <- R6::R6Class(classname = "Tile",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(){

                     # get a tile id from global parameters and add my reference to the globals
                     private$id = MBITES:::Globals$get_tileID()
                     MBITES:::Globals$add_tile(self)

                     # create containers
                     private$Mosquitoes = HashMap$new(N=1e4L)
                     private$Human = HashMap$new(N=1e3L)

                     # log the event
                     futile.logger::flog.trace("Tile %i being born at self: %s , private: %s",private$id,pryr::address(self),pryr::address(private))
                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     futile.logger::flog.trace("Tile %i being killed at self: %s , private: %s",private$id,pryr::address(self),pryr::address(private))
                   } # end destructor

                 ),

                 # private members
                 private = list(

                   id               = integer(1), # integer ID of this tile
                   Sites            = list(), # list of references to Site objects
                   Mosquitoes       = NULL, # a MosquitoPop object
                   Humans           = NULL # a HumanPop object

                 )
) # end Tile class definition

# get_mosquitoes

# get_humans


#' Tile: Return a Site Reference
#'
#' Return the reference to a given \code{\link[MBITES]{Site}} object by id.
#'
get_site_Tile <- function(i){
  return(private$Sites[[i]])
}

Tile$set(which = "public",name = "get_site",
    value = get_site_Tile, overwrite = TRUE
)
