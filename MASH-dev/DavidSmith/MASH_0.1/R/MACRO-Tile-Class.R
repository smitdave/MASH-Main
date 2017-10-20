#################################################################
#
#   MASH
#   R6-ified
#   MACRO MacroTile Class Definition
#   David Smith, Hector Sanchez, Sean Wu
#   May 11, 2016
#
#################################################################

#' MACRO Tile Class Definition
#'
#' This is a generic MACRO metapopulation tile blah blah ...
#'  below i describe the basic structure of the patch. methods and fields for specific COMPONENTS can be found in:
#' * somewhere 1
#' * somewhere 2
#' @md
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#' @details
#' talk about me in detail!
#' @section public:
#' \itemize{
#'   \item{\code{initialize(N)}}{
#'        talk about me!
#'        }
#'   \item{\code{function()}}{
#'        talk about me!
#'        }
#' }
#' @section private:
#' \itemize{
#'   \item{\code{function()}}{
#'        talk about me!
#'        }
#'   \item{\code{function()}}{
#'        talk about me!
#'        }
#' }
#' @section Active Bindings:
#' \itemize{
#'   \item{\code{coolActiveBinding}}{...}
#' }
#' @export
MacroTile <- R6::R6Class(classname = "MacroTile",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public methods & fields
                 public = list(

                   #################################################
                   # Initialize
                   #################################################

                   initialize = function(MacroTile_PAR){

                     # generate objects
                     private$HumanPop = HumanPop$new(HumanPop_PAR = MacroTile_PAR$HumanPop_PAR)
                     private$Patches = MacroPatch$new(MacroPatch_PAR = MacroTile_PAR$MacroPatch_PAR)
                     private$MosquitoPop = MacroMosquitoPop$new(N = MacroTile_PAR$N, MacroMosquitoPop_PAR = MacroTile_PAR$MacroMosquitoPop_PAR)

                     private$tNow = 0
                     private$MacroTile_PAR = MacroTile_PAR

                     # Human & HumanPop Pointers (duplicate for Humans in HumanPop$pop)
                     private$HumanPop$set_TilePointer(self)
                     private$HumanPop$set_MosquitoPointer(private$MosquitoPop)
                     private$HumanPop$set_PatchesPointer(private$Patches)

                     # Human & HumanPop initilization
                     for(ixH in 1:private$HumanPop$nHumans){

                       # pointers
                       private$HumanPop$get_Human(ixH)$set_TilePointer(self)
                       private$HumanPop$get_Human(ixH)$set_MosquitoPointer(private$MosquitoPop)
                       private$HumanPop$get_Human(ixH)$set_PatchesPointer(private$Patches)

                       # travel
                       private$HumanPop$get_Human(ixH)$set_location(MacroTile_PAR$HumanPop_PAR$homeIDs[ixH])
                       private$HumanPop$get_Human(ixH)$set_patchID(MacroTile_PAR$HumanPop_PAR$homeIDs[ixH])
                       private$HumanPop$get_Human(ixH)$init_travel(n=2)

                       # update baseline human biting weight
                       myPatch = private$HumanPop$get_Human(ixH)$get_patchID()
                       private$Patches$accumulate_bWeightHuman(bWeightHuman = private$HumanPop$get_Human(ixH)$get_bWeight(), ix = myPatch)

                       # patchID
                       private$HumanPop$set_patchID(MacroTile_PAR$HumanPop_PAR$homeIDs)
                     }

                     # Patches Pointers
                     private$Patches$set_TilePointer(self)
                     private$Patches$set_MosquitoPointer(private$MosquitoPop)
                     private$Patches$set_HumansPointer(private$HumanPop)

                     # MosquitoPop Pointers
                     private$MosquitoPop$set_TilePointer(self)
                     private$MosquitoPop$set_PatchesPointer(private$Patches)
                     private$MosquitoPop$set_HumansPointer(private$HumanPop)

                   },

                  #################################################################
                  # Getters & Setters
                  #################################################################

                   get_tNow = function(){
                     return(private$tNow)
                   },
                   set_tNow = function(tNow){
                     private$tNow = tNow
                   },

                   get_MacroTile_PAR = function(){
                     return(private$MacroTile_PAR)
                   },

                   set_MacroTile_PAR = function(MacroTile_PAR){
                     private$MacroTile_PAR = MacroTile_PAR
                   },

                   get_HumanPop = function(){
                     return(private$HumanPop)
                   },

                   get_Patches = function(){
                     return(private$Patches)
                   },

                   get_MosquitoPop = function(){
                     return(private$MosquitoPop)
                   }

                  ),

                  # private methods & fields
                  private = list(
                    tNow = NULL,
                    MacroTile_PAR = NULL,
                    HumanPop = NULL,
                    Patches = NULL,
                    MosquitoPop = NULL
                  )

)
