###############################################################################
#
#       __  _______  _____ ____  __  ________________
#      /  |/  / __ \/ ___// __ \/ / / /  _/_  __/ __ \
#     / /|_/ / / / /\__ \/ / / / / / // /  / / / / / /
#    / /  / / /_/ /___/ / /_/ / /_/ // /  / / / /_/ /
#   /_/  /_/\____//____/\___\_\____/___/ /_/  \____/
#
#   MASH-MACRO
#   Mosquito Abstract Base Class Definition
#   MASH Team
#   November 2017
#
###############################################################################

#' Mosquito Population Abstract Base Class Definition
#'
#' An abstract base class that specific mosquito ecology models will inherit from. This is not strictly necessary in the R6 object-oriented framework but it is intended
#' to mirror a C++ polymorphic implementation.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * The constructor (initialize) method must be overwritten by all inheriting classes.
#'
#' @section **Methods**:
#'  * oneDay_popDynamics: this method must be overwritten by all inheriting classes (pure virtual function in C++)
#'  * oneDay_oviposition: this method must be overwritten by all inheriting classes (pure virtual function in C++)
#'  * get_emergingAdults: this method must be overwritten by all inheriting classes (pure virtual function in C++)
#'  * get_HumansPointer: see \code{\link{get_HumansPointer_Mosquito_Base}}
#'  * set_HumansPointer: see \code{\link{set_HumansPointer_Mosquito_Base}}
#'  * get_TilePointer: see \code{\link{get_TilePointer_Mosquito_Base}}
#'  * set_TilePointer: see \code{\link{set_TilePointer_Mosquito_Base}}
#'
#' @section **Fields**:
#'  * HumansPointer: a reference to a \code{\link{HumanPop}} object
#'  * TilePointer: a reference to a \code{\link{MacroTile}} object
#'
#' @md
#' @export
Mosquito_Base <- R6::R6Class(classname = "Mosquito_Base",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public methods & fields
                 public = list(

                   #################################################
                   # Constructor
                   #################################################

                   initialize = function(){
                     # C++: make this an actual constructor.
                     # don't forget a !virtual! destructor
                     cat("constructor for Mosquito_Base class should never be called\n")
                   },

                   oneDay_popDynamics = function(){
                     # C++: make this a virtual function
                     cat("oneDay_popDynamics for Mosquito_Base should never be called\n")
                   },

                   oneDay_oviposition = function(){
                     # C++: make this a virtual function
                     cat("oneDay_oviposition for Mosquito_Base should never be called\n")
                   },

                   get_emergingAdults = function(){
                     # C++: make this a virtual function
                     # this gets emerging adults for any model and it's what AquaPop_Base interacts with
                     cat("get_emergingAdults for Mosquito_Base should never be called\n")
                   }

                  ),

                  # private methods & fields
                  private = list(

                    # Pointers
                    HumansPointer             = NULL, # C++: make this a std::weak_ptr (smart pointer that can see/use a shared resource but does not 'own' that resource nor should decide its lifespan)
                    TilePointer               = NULL # C++: make this a std::weak_ptr

                  )

) #end class definition


#' Get Humans Pointer
#'
#' Return a pointer to this \code{\link{HumanPop}} in this tile.
#'
#'  * This method is bound to \code{Mosquito_Base$get_HumansPointer}
#'
get_HumansPointer_Mosquito_Base <- function(){
  return(private$HumansPointer)
}

Mosquito_Base$set(which = "public",name = "get_HumansPointer",
  value = get_HumansPointer_Mosquito_Base, overwrite = TRUE
)

#' Set Humans Pointer
#'
#' Set a pointer to this \code{\link{HumanPop}} in this tile.
#'
#'  * This method is bound to \code{Mosquito_Base$get_HumansPointer}
#'
set_HumansPointer_Mosquito_Base <- function(HumansPointer){
  if(class(HumansPointer)[1]!="HumanPop"){stop("set_HumansPointer_Mosquito_Base must be set with a 'HumanPop' object reference")}
  private$HumansPointer = HumansPointer
}

Mosquito_Base$set(which = "public",name = "set_HumansPointer",
  value = set_HumansPointer_Mosquito_Base, overwrite = TRUE
)

#' Get Tile Pointer
#'
#' Return a pointer to the enclosing \code{\link{MacroTile}}
#'
#'  * This method is bound to \code{Mosquito_Base$get_TilePointer}
#'
get_TilePointer_Mosquito_Base <- function(){
  return(private$TilePointer)
}

Mosquito_Base$set(which = "public",name = "get_TilePointer",
  value = get_TilePointer_Mosquito_Base, overwrite = TRUE
)

#' Set Tile Pointer
#'
#' Set a pointer to the enclosing \code{\link{MacroTile}}
#'
#'  * This method is bound to \code{Mosquito_Base$set_TilePointer}
#'
set_TilePointer_Mosquito_Base <- function(TilePointer){
  if(class(TilePointer)[1]!="MacroTile"){stop("set_TilePointer_Mosquito_Base must be set with a 'MacroTile' object reference")}
  private$TilePointer = TilePointer
}

Mosquito_Base$set(which = "public",name = "set_TilePointer",
  value = set_TilePointer_Mosquito_Base, overwrite = TRUE
)
