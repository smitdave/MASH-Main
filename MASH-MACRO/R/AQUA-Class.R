


# aqua class

#' Aquatic Population Abstract Base Class Definition
#'
#' An abstract base class that specific aquatic ecology models will inherit from. This is not strictly necessary in the R6 object-oriented framework but it is intended
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
AquaPop_Base <- R6::R6Class(classname = "AquaPop_Base",
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
                     cat("constructor for AquaPop_Base class should never be called\n")
                   }

                  ),

                  # private methods & fields
                  private = list(

                    # Aquatic Populations
                    ImagoQ                    = NULL,

                    # Pointers
                    PatchPointer              = NULL

                  )

) #end class definition

#' Get Patch Pointer
#'
#' Return a pointer to the enclosing \code{\link{MacroPatch}}
#'
get_PatchPointer_AquaPop_Base <- function(){
  return(private$PatchPointer)
}

AquaPop_Base$set(which = "public",name = "get_PatchPointer",
  value = get_PatchPointer_AquaPop_Base, overwrite = TRUE
)

#' Set Patch Pointer
#'
#' Set a pointer to the enclosing \code{\link{MacroPatch}}
#'
#' @param PatchPointer a reference to \code{\link{MacroPatch}} object
#'
set_PatchPointer_AquaPop_Base <- function(PatchPointer){
  private$PatchPointer = PatchPointer
}

AquaPop_Base$set(which = "public",name = "set_PatchPointer",
  value = set_PatchPointer_AquaPop_Base, overwrite = TRUE
)


addCohort_AquaPop_Base <- function(){
  EmergingAdults = private$ImagoQ$get_ImagoQTime(tNow = private$PatchPointer$get_TilePointer()$get_tNow(),clear = TRUE)

  if(length(EmergingAdults) > 0){
    for(i in 1:length(EmergingAdults)){

    }
  }
}
