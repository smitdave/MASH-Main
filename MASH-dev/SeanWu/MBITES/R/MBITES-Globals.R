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

                 # public fields
                 public = list(

                   # begin constructor
                   initialize = function(){
                     futile.logger::flog.trace("MBITES_Globals being born at: self %s , private %s",pryr::address(self),pryr::address(private))
                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     futile.logger::flog.trace("MBITES_Globals being killed at: self %s , private %s",pryr::address(self),pryr::address(private))

                     # world state globals
                     private$tNow = 0L

                     # mosquito globals
                     private$mosquito_id = 0L
                     if(!is.null(private$mosquito_f_out)){
                      close(private$mosquito_f_out)
                     }
                     if(!is.null(private$mosquito_m_out)){
                      close(private$mosquito_m_out)
                     }

                     # human globals
                     private$human_id = 0L
                     if(!is.null(private$human_out)){
                      close(private$human_out)
                     }

                     # tile globals
                     private$tile_id = 0L
                     private$tiles = list()

                     invisible(gc())
                   } # end destructor

                 ), # end public fields

                 # private fields
                 private = list(

                   # world-state globals
                   tNow               = 0L, # current simulation time
                   mutex              = NULL,
                   SETUP              = FALSE,

                   # mosquito globals
                   mosquito_id        = 0L, # global counter of IDs
                   mosquito_f_out     = NULL, # connection object for logging female mosquito histories
                   mosquito_m_out     = NULL, # connection object for logging male mosquito histories

                   # human globals
                   human_id           = 0L, # global counter of IDs
                   human_out          = NULL, # connection object for logging human histories

                   # tile globals
                   tile_id            = 0L, # global counter of tile IDs
                   tiles              = list() # references to all the tiles

                 ) # end private fields

) # end MBITES_Globals definition

# global assignment to package namespace at end of script


###############################################################################
# World-state global methods
###############################################################################

init_lock_MBITES_Globals <- function(){
  private$mutex = synchronicity::boost.mutex()
}

get_lock_MBITES_Globals <- function(){
  synchronicity::lock(private$mutex)
}

release_lock_MBITES_Globals <- function(){
  synchronicity::unlock(private$mutex)
}

get_tNow_MBITES_Globals <- function(){
  return(private$tNow)
}

increment_tNow_MBITES_Globals <- function(){
  private$tNow = private$tNow + 1L
}

get_SETUP_MBITES_Globals <- function(){
  return(private$SETUP)
}

set_SETUP_MBITES_Globals <- function(bool){
  if(!is.logical(bool)){
    stop("setup flag in MBITES_Globals must be a logical\n")
  }
  private$SETUP = bool
}

MBITES_Globals$set(which = "public",name = "increment_tNow",
          value = increment_tNow_MBITES_Globals, overwrite = TRUE
)

MBITES_Globals$set(which = "public",name = "get_tNow",
          value = get_tNow_MBITES_Globals, overwrite = TRUE
)

MBITES_Globals$set(which = "public",name = "get_lock",
          value = get_lock_MBITES_Globals, overwrite = TRUE
)

MBITES_Globals$set(which = "public",name = "release_lock",
          value = release_lock_MBITES_Globals, overwrite = TRUE
)

MBITES_Globals$set(which = "public",name = "init_lock",
          value = init_lock_MBITES_Globals, overwrite = TRUE
)

MBITES_Globals$set(which = "public",name = "get_SETUP",
          value = get_SETUP_MBITES_Globals, overwrite = TRUE
)

MBITES_Globals$set(which = "public",name = "set_SETUP",
          value = set_SETUP_MBITES_Globals, overwrite = TRUE
)



###############################################################################
# Mosquito related methods
###############################################################################

#' MBITES Globals: Get a new Mosquito ID
#'
#' Increments and gets a new mosquito ID; this function should only be called from the constructor of \code{\link[MBITES]{Mosquito}}
#' objects.
#'
#'  * This method is bound to \code{MBITES_Globals$get_mosquito_id}.
#'
get_mosquito_id_MBITES_Globals <- function(){
  private$mosquito_id = private$mosquito_id + 1L
  return(private$mosquito_id)
}

MBITES_Globals$set(which = "public",name = "get_mosquito_id",
          value = get_mosquito_id_MBITES_Globals, overwrite = TRUE
)


###############################################################################
# Human related methods
###############################################################################

#' MBITES Globals: Get a new Human ID
#'
#' Increments and gets a new mosquito ID; this function should only be called from the constructor of \code{\link[MBITES]{Human_NULL}} or Human
#' objects.
#'
#'  * This method is bound to \code{MBITES_Globals$get_human_id}.
#'
get_human_id_MBITES_Globals <- function(){
  private$human_id = private$human_id + 1L
  return(private$human_id)
}

MBITES_Globals$set(which = "public",name = "get_human_id",
          value = get_human_id_MBITES_Globals, overwrite = TRUE
)


###############################################################################
# Tile related methods
###############################################################################

#' MBITES Globals: Get a new Tile ID
#'
#' Increments and gets a new tile ID; this function should only be called from the constructor of \code{\link[MBITES]{Tile}}
#' objects.
#'
#'  * This method is bound to \code{MBITES_Globals$get_tileID}.
#'
get_tileID_MBITES_Globals <- function(){
  private$tile_id = private$tile_id + 1L
  return(private$tile_id)
}

#' MBITES Globals: Add a Tile Reference
#'
#' Adds a reference to a tile to the list of all tiles in the simulation.
#'
#'  * This method is bound to \code{MBITES_Globals$add_tile}.
#'
#' @param tile a reference to a \code{\link[MBITES]{Tile}} object
#'
add_tile_MBITES_Globals <- function(tile){
  private$tiles = append(private$tiles,tile)
}

#' MBITES Globals: Return a Tile Reference
#'
#' Returns the reference to the \code{\link[MBITES]{Tile}} object with the associated id.
#'
#'  * This method is bound to \code{MBITES_Globals$get_tile}.
#'
#' @param id an integer tile id
#'
get_tile_MBITES_Globals <- function(id){
  return(private$tiles[[id]])
}

#' MBITES Globals: Get Number of Tiles
#'
#' Return the total number of tiles created.
#'
#'  * This method is bound to \code{MBITES_Globals$get_n_tile}.
#'
get_n_tile_MBITES_Globals <- function(){
  return(length(private$tiles))
}

MBITES_Globals$set(which = "public",name = "get_tileID",
          value = get_tileID_MBITES_Globals, overwrite = TRUE
)

MBITES_Globals$set(which = "public",name = "add_tile",
          value = add_tile_MBITES_Globals, overwrite = TRUE
)

MBITES_Globals$set(which = "public",name = "get_tile",
          value = get_tile_MBITES_Globals, overwrite = TRUE
)

MBITES_Globals$set(which = "public",name = "get_n_tile",
          value = get_n_tile_MBITES_Globals, overwrite = TRUE
)




###############################################################################
# assign MBITES globals instance in the package namespace (a bit hacky)
###############################################################################

Globals <- MBITES_Globals$new()
