###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Globals
#     MBITES Team
#     February 2018
#
###############################################################################

#' MBITES Globals Singleton
#'
#' This class is a singleton object in the \code{MBITES} package namespace that stores global values needed for the MBITES simulation.
#' It can be accessed by \code{MBITES:::Globals}.
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
#'  * get_mosquito_id: get an integer ID for a mosquito
#'
#' @section **Fields**:
#'  * id: integer identifier of site
#'  * field: im a field!
#'
MBITES_Globals <- R6::R6Class(classname = "MBITES_Globals",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 public = list(

                   initialize = function(){
                     futile.logger::flog.trace("MBITES_Globals being born at: %s",pryr::address(self))
                   },

                   finalize = function(){
                     futile.logger::flog.trace("MBITES_Globals being killed at: %s",pryr::address(self))
                   },

                   # # for debugging
                   # print = function(){
                   #   selfAd=pryr::address(self)
                   #   privAd = pryr::address(private)
                   #   cat("i'm a MBITES_Globals instance residing as self: ",selfAd," and private: ",privAd,"\n")
                   #   return(list(selfAd,privAd))
                   # },

                   # reset all values
                   reset = function(){
                     cat("reset all MBITES global parameters to initial values\n")
                     private$t_now = 0L
                     private$mosquito_id = 0L
                   },

                   get_mosquito_id = function(){
                     private$mosquito_id = private$mosquito_id + 1L
                     return(private$mosquito_id)
                   },

                   get_t_now = function(){
                     return(private$t_now)
                   },

                   increment_t_now = function(){
                     private$t_now = private$t_now + 1L
                   },

                   get_tile = function(){
                     return(private$tile)
                   }

                 ),

                 private = list(

                   t_now              = 0L, # current simulation time

                   mosquito_id        = -1L, # global counter of IDs
                   mosquito_f_out     = NULL, # connection object for logging female mosquito histories
                   mosquito_m_out     = NULL, # connection object for logging male mosquito histories
                   human_out          = NULL, # connection object for logging human histories

                   tile               = NULL # reference to tile

                 ),
) # end MBITES_Globals definition

# global assignment to package namespace at end of script

###############################################################################
# Methods
###############################################################################

#' MBITES Globals: Set Tile Reference
#'
#' Set the reference to a tile
#'
#'  * This method is bound to \code{MBITES_Globals$set_tile()}.
#'
#' @param tile a reference to a \code{\link[MBITES]{Tile}} object
#'
set_tile_MBITES_Globals <- function(tile){
  private$tile = tile
}

MBITES_Globals$set(which = "public",name = "set_tile",
          value = set_tile_MBITES_Globals, overwrite = TRUE
)






###############################################################################
# assign MBITES globals instance in the package namespace (a bit hacky)
###############################################################################

Globals <- MBITES_Globals$new()
