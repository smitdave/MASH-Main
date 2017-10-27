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
#'  * This method is bound to \code{MovementObject$get_SitePointer}
#'
get_SitePointer_MovementObject <- function(){return(private$SitePointer)}

MovementObject$set(which = "public",name = "get_SitePointer",
  value = get_SitePointer_MovementObject, overwrite = TRUE
)

#' Set SitePointer
#'
#' Set pointer to my enclosing site
#'  * This method is bound to \code{MovementObject$set_SitePointer}
#'
#' @param Site a \code{\link[R6]{R6Class}} environment
#'
set_SitePointer_MovementObject <- function(Site){private$SitePointer = Site}

MovementObject$set(which = "public",name = "set_SitePointer",
  value = set_SitePointer_MovementObject, overwrite = TRUE
)

#' Get LandscapePointer
#'
#' Return pointer to my enclosing site
#'  * This method is bound to \code{MovementObject$get_LandscapePointer}
#'
get_LandscapePointer_MovementObject <- function(){return(private$LandscapePointer)}

MovementObject$set(which = "public",name = "get_LandscapePointer",
  value = get_LandscapePointer_MovementObject, overwrite = TRUE
)

#' Set LandscapePointer
#'
#' Set pointer to my enclosing site
#'  * This method is bound to \code{MovementObject$set_LandscapePointer}
#'
#' @param Landscape a \code{\link{Landscape}} environment
#'
set_LandscapePointer_MovementObject <- function(Landscape){private$LandscapePointer = Landscape}

MovementObject$set(which = "public",name = "set_LandscapePointer",
  value = set_LandscapePointer_MovementObject, overwrite = TRUE
)
