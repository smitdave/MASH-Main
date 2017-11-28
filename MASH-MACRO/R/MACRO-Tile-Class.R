###############################################################################
#       __  ___                    _______ __
#      /  |/  /___ _______________/_  __(_) /__
#     / /|_/ / __ `/ ___/ ___/ __ \/ / / / / _ \
#    / /  / / /_/ / /__/ /  / /_/ / / / / /  __/
#   /_/  /_/\__,_/\___/_/   \____/_/ /_/_/\___/
#
#   MASH-MACRO
#   Tile Class Definition
#   MASH Team
#   November 2017
#
###############################################################################

#' MASH-MACRO MacroTile Class Definition
#'
#' write me
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
MacroTile <- R6::R6Class(classname = "MacroTile",
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

                   } # end constructor

                   # finalize = function(){}

                  ),

                  # private methods & fields
                  private = list(

                    # Simulation-level parameters
                    tStart                    = NULL,
                    tNow                      = NULL,

                    # class containers
                    Patches                   = NULL,
                    HumanPop                  = NULL,
                    Mosquito                  = NULL,

                    # Output Connections
                    directory                 = NULL

                  )

) #end class definition


get_Patch_MacroTile <- function(ix = NULL){
  if(!is.null(ix)){
    return(private$Patches[[ix]])
  } else {
    return(private$Patches)
  }
}
