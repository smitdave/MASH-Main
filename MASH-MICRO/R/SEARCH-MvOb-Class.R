###############################################################################
#      _____                      __
#     / ___/___  ____ ___________/ /_
#     \__ \/ _ \/ __ `/ ___/ ___/ __ \
#    ___/ /  __/ /_/ / /  / /__/ / / /
#   /____/\___/\__,_/_/   \___/_/ /_/
#
#   MASH-MICRO
#   SEARCH: Movement Object (MvOb) Class
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################


#' MovementObject Class Definition
#'
#' im a class!
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
MovementObject <- R6::R6Class(classname = "MovementObject",
                          portable = TRUE,
                          cloneable = FALSE,
                          lock_class = FALSE,
                          lock_objects = FALSE,

                          # public members
                          public = list(

                            ##############################################################
                            # Constructor
                            ##############################################################

                          ),

                          # private members
                          private = list(

                            # Pointers
                            SitePointer      = NULL,       # Point to the site I am in
                            LandscapePointer = NULL,       # Point to Landscape object in same microsimulation Tile
                            TilePointer      = NULL,       # Point to enclosing microsimulation Tile

                          )

)


#' Get SitePointer
#'
#' Return pointer to my enclosing site
#'  * This method is bound to \code{FeedingSite$get_ix}
#'
get_SitePointer_MovementObject <- function(){return(private$ix)}

FeedingSite$set(which = "public",name = "get_ix",
  value = get_ix_FeedingSite, overwrite = TRUE
)

#' Set ix
#'
#' Set site index \code{ix}
#'  * This method is bound to \code{FeedingSite$set_ix}
#'
#' @param ix integer
#'
set_SitePointer_MovementObject <- function(ix){private$ix = ix}

FeedingSite$set(which = "public",name = "set_ix",
  value = set_ix_FeedingSite, overwrite = TRUE
)
