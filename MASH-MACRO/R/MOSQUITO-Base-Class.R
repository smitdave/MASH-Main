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
#'  * output: this method must be overwritten by all inheriting classes (pure virtual function in C++)
#'  * reset: this method must be overwritten by all inheriting classes (pure virtual function in C++)
#'  * get_a: return the human feeding rate (required to calculate EIR by \code{\link{updateEIR_Human}})
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
                   },

                   initialize_output = function(con){
                     # C++: make this a virtual function
                     # initialize appropriate headers and output for model
                     cat("initialize_output for Mosquito_Base should never be called\n")
                   },

                   output = function(con){
                     # C++: make this a virtual function
                     # this write appropriate output for the model
                     cat("output for Mosquito_Base should never be called\n")
                   },

                   reset = function(){
                     # C++: make this a virtual function
                     # this resets the mosquito population back to initial values for repeated runs
                     cat("reset for Mosquito_Base should never be called\n")
                   },

                   # getters

                   get_a = function(){
                     return(private$a)
                   },

                   get_EIP = function(){
                     cat("get_EIP for Mosquito_Base should never be called\n")
                   }

                  ),

                  # private methods & fields
                  private = list(

                    # human feeding rate
                    a                         = numeric(1),

                    # Pointers
                    TilePointer               = NULL # C++: make this a std::weak_ptr

                  )

) #end class definition

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
