


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
                     # C++: make this an actual constructor.
                     # don't forget a !virtual! destructor
                     cat("constructor for AquaPop_Base class should never be called\n")
                   },

                   oneDay_popDynamics = function(){
                     # C++: make this a virtual function
                     cat("oneDay_popDynamics for AquaPop_Base should never be called\n")
                   }

                  ),

                  # private methods & fields
                  private = list(

                    # Aquatic Populations
                    EggQ                      = NULL,
                    ImagoQ                    = NULL,

                    # Pointers
                    PatchPointer              = NULL,
                    TilePointer               = NULL,
                    MosquitoPointer           = NULL

                  )

) #end class definition



#' Add Emerging Cohort to Adult Population
#'
#' Take emerging aquatic stage mosquitoes from \code{\link[MASHcpp]{ImagoQ}} and add to the adult population.
#'
#'  * This method is bound to \code{AquaPop_Base$oneDay_addCohort}
#'
oneDay_addCohort_AquaPop_Base <- function(){
  EmergingAdults = private$ImagoQ$get_ImagoQTime(tNow = private$PatchPointer$get_TilePointer()$get_tNow(),clear = TRUE)

  if(length(EmergingAdults) > 0){
    for(i in 1:length(EmergingAdults)){
      private$MosquitoPointer$get_emergingAdults(M=EmergingAdults[[i]]$N,ix=private$PatchPointer$get_patchID())
    }
  }
}

AquaPop_Base$set(which = "public",name = "oneDay_addCohort",
  value = oneDay_addCohort_AquaPop_Base, overwrite = TRUE
)

# Getters & Setters

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

#' Get Tile Pointer
#'
#' Return a pointer to the enclosing \code{\link{MacroTile}}
#'
get_TilePointer_AquaPop_Base <- function(){
  return(private$TilePointer)
}

AquaPop_Base$set(which = "public",name = "get_TilePointer",
  value = get_TilePointer_AquaPop_Base, overwrite = TRUE
)

#' Set Tile Pointer
#'
#' Set a pointer to the enclosing \code{\link{MacroTile}}
#'
#' @param TilePointer a reference to \code{\link{MacroTile}} object
#'
set_TilePointer_AquaPop_Base <- function(TilePointer){
  private$TilePointer = TilePointer
}

AquaPop_Base$set(which = "public",name = "set_TilePointer",
  value = set_TilePointer_AquaPop_Base, overwrite = TRUE
)

#' Get Mosquito Pointer
#'
#' Return a pointer to the enclosing \code{\link{MacroMosquito}}
#'
get_MosquitoPointer_AquaPop_Base <- function(){
  return(private$MosquitoPointer)
}

AquaPop_Base$set(which = "public",name = "get_MosquitoPointer",
  value = get_MosquitoPointer_AquaPop_Base, overwrite = TRUE
)

#' Set Mosquito Pointer
#'
#' Set a pointer to the enclosing \code{\link{MacroMosquito}}
#'
#' @param MosquitoPointer a reference to \code{\link{MacroMosquito}} object
#'
set_MosquitoPointer_AquaPop_Base <- function(MosquitoPointer){
  private$MosquitoPointer = MosquitoPointer
}

AquaPop_Base$set(which = "public",name = "set_MosquitoPointer",
  value = set_MosquitoPointer_AquaPop_Base, overwrite = TRUE
)
