# ###############################################################################
# #      _____                      __
# #     / ___/___  ____ ___________/ /_
# #     \__ \/ _ \/ __ `/ ___/ ___/ __ \
# #    ___/ /  __/ /_/ / /  / /__/ / / /
# #   /____/\___/\__,_/_/   \___/_/ /_/
# #
# #   MASH-MICRO
# #   SEARCH: Movement Object (MvOb) Class
# #   MASH-MICRO Team
# #   September 7, 2017
# #
# ###############################################################################
#
#
# #' MovementObject Class Definition
# #'
# #' The MovementObject class is to be used as an abstract base class (see \url{http://www.cplusplus.com/doc/tutorial/polymorphism/})
# #' from which other classes in the SEARCH family of algorithms for mosquito movement inherit from.
# #'
# #' @docType class
# #' @format An \code{\link[R6]{R6Class}} generator object
# #' @keywords R6 class
# #'
# #' @section **Constructor**:
# #'  * argument: im an agument!
# #'
# #' @section **Methods**:
# #'  * method: im a method!
# #'
# #' @section **Fields**:
# #'  * field: im a field!
# #'
# #' @export
# MovementObject <- R6::R6Class(classname = "MovementObject",
#                           portable = TRUE,
#                           cloneable = FALSE,
#                           lock_class = FALSE,
#                           lock_objects = FALSE,
#
#                           # public members
#                           public = list(),
#
#                           # private members
#                           private = list(
#
#                             # Pointers
#                             SitePointer      = NULL,       # Point to the site I am in
#                             FemalePopPointer = NULL,       # Point to the female mosquito population in this tile
#                             MalePopPointer   = NULL,       # Point to the male mosquito population in this tile
#                             LandscapePointer = NULL,       # Point to Landscape object in same microsimulation Tile
#                             TilePointer      = NULL,       # Point to enclosing microsimulation Tile
#
#                           )
#
# )
#
# #' Get SitePointer
# #'
# #' Return pointer to my enclosing site
# #'  * This method is bound to \code{MovementObject$get_SitePointer}
# #'
# get_SitePointer_MovementObject <- function(){return(private$SitePointer)}
#
# MovementObject$set(which = "public",name = "get_SitePointer",
#   value = get_SitePointer_MovementObject, overwrite = TRUE
# )
#
# #' Set SitePointer
# #'
# #' Set pointer to my enclosing site
# #'  * This method is bound to \code{MovementObject$set_SitePointer}
# #'
# #' @param Site a \code{\link[R6]{R6Class}} environment
# #'
# set_SitePointer_MovementObject <- function(Site){private$SitePointer = Site}
#
# MovementObject$set(which = "public",name = "set_SitePointer",
#   value = set_SitePointer_MovementObject, overwrite = TRUE
# )
#
# #' Get FemalePopPointer
# #'
# #' Return pointer to my enclosing site
# #'  * This method is bound to \code{MovementObject$get_FemalePopPointer}
# #'
# get_FemalePopPointer_MovementObject <- function(){return(private$FemalePopPointer)}
#
# MovementObject$set(which = "public",name = "get_FemalePopPointer",
#   value = get_FemalePopPointer_MovementObject, overwrite = TRUE
# )
#
# #' Set FemalePopPointer
# #'
# #' Set pointer to my enclosing site
# #'  * This method is bound to \code{MovementObject$set_FemalePopPointer}
# #'
# #' @param Site a \code{\link[R6]{R6Class}} environment
# #'
# set_FemalePopPointer_MovementObject <- function(Site){private$FemalePopPointer = Site}
#
# MovementObject$set(which = "public",name = "set_FemalePopPointer",
#   value = set_FemalePopPointer_MovementObject, overwrite = TRUE
# )
#
# #' Get MalePopPointer
# #'
# #' Return pointer to my enclosing site
# #'  * This method is bound to \code{MovementObject$get_MalePopPointer}
# #'
# get_MalePopPointer_MovementObject <- function(){return(private$MalePopPointer)}
#
# MovementObject$set(which = "public",name = "get_MalePopPointer",
#   value = get_MalePopPointer_MovementObject, overwrite = TRUE
# )
#
# #' Set MalePopPointer
# #'
# #' Set pointer to my enclosing site
# #'  * This method is bound to \code{MovementObject$set_MalePopPointer}
# #'
# #' @param Site a \code{\link[R6]{R6Class}} environment
# #'
# set_MalePopPointer_MovementObject <- function(Site){private$MalePopPointer = Site}
#
# MovementObject$set(which = "public",name = "set_MalePopPointer",
#   value = set_MalePopPointer_MovementObject, overwrite = TRUE
# )
#
#
# #' Get LandscapePointer
# #'
# #' Return pointer to my enclosing site
# #'  * This method is bound to \code{MovementObject$get_LandscapePointer}
# #'
# get_LandscapePointer_MovementObject <- function(){return(private$LandscapePointer)}
#
# MovementObject$set(which = "public",name = "get_LandscapePointer",
#   value = get_LandscapePointer_MovementObject, overwrite = TRUE
# )
#
# #' Set LandscapePointer
# #'
# #' Set pointer to my enclosing site
# #'  * This method is bound to \code{MovementObject$set_LandscapePointer}
# #'
# #' @param Landscape a \code{\link{Landscape}} environment
# #'
# set_LandscapePointer_MovementObject <- function(Landscape){private$LandscapePointer = Landscape}
#
# MovementObject$set(which = "public",name = "set_LandscapePointer",
#   value = set_LandscapePointer_MovementObject, overwrite = TRUE
# )
#
# #' Get TilePointer
# #'
# #' Return pointer to my enclosing site
# #'  * This method is bound to \code{MovementObject$get_TilePointer}
# #'
# get_TilePointer_MovementObject <- function(){return(private$TilePointer)}
#
# MovementObject$set(which = "public",name = "get_TilePointer",
#   value = get_TilePointer_MovementObject, overwrite = TRUE
# )
#
# #' Set TilePointer
# #'
# #' Set pointer to my enclosing site
# #'  * This method is bound to \code{MovementObject$set_TilePointer}
# #'
# #' @param Tile a \code{\link{Tile}} environment
# #'
# set_TilePointer_MovementObject <- function(Tile){private$TilePointer = Tile}
#
# MovementObject$set(which = "public",name = "set_TilePointer",
#   value = set_TilePointer_MovementObject, overwrite = TRUE
# )
