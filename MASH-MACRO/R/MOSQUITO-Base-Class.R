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
#'  * argument: i'm an argument!
#'
#' @section **Methods**:
#'  * method: i'm a method!
#'
#' @section **Fields**:
#'  * field: i'm a field!
#'
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
