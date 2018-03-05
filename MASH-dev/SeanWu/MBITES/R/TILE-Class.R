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
#' and the agents that direct the dynamics on the tile.
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
#'  * add_egg: function that must take an egg batch and add it to the \code{EggQ}
#'  * one_day: function that updates daily aquatic population dynamics
#'  * push_imago: function that takes emerging imagos from the \code{ImagoQ} and pushes them to the adult mosquito population
#'
#' @section **Fields**:
#'  * EggQ: a list of egg batches
#'  * ImagoQ: a list of emerging imagos (adult mosquitoes)
#'  * SiteP: a reference to a class that inherits the interface of \code{\link[MBITES]{Aqua_Resource}}
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
                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                   } # end destructor

                 ),

                 # private members
                 private = list(

                   Sites = list(), # list of references to Site objects
                   Mosquitoes = NULL, # a MosquitoPop object
                   Humans = NULL # a HumanPop object

                 )
) # end Aqua_Resource class definition



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
