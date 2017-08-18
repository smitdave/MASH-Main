###############################################################################
#       __  ___                      ____        __       __
#      /  |/  /___ _______________  / __ \____ _/ /______/ /_
#     / /|_/ / __ `/ ___/ ___/ __ \/ /_/ / __ `/ __/ ___/ __ \
#    / /  / / /_/ / /__/ /  / /_/ / ____/ /_/ / /_/ /__/ / / /
#   /_/  /_/\__,_/\___/_/   \____/_/    \__,_/\__/\___/_/ /_/
#
#   MASH-MACRO
#   MACRO: MacroPatch Class Definition
#   David Smith, Hector Sanchez, Sean Wu
#   August 18, 2017
#
###############################################################################

#' MASH-MACRO MacroPatch Class Definition
#'
#' Generate a single well-mixed MacroPatch, which can run classical models in standalone mode or be linked in a network of
#' other patches in a \code{\link{MacroTile}}
#' Each instance of a \code{Human} lives in a \code{\link{HumanPop}}
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Constructor:
#'  * item 1:
#'
#' @section Methods:
#'  * **get_Human**: retrieve human whose field 'myID' matches argument 'humanID' in keylist of pop field.
#'  * item 1:
#'  * item 1:
#'  * item 1:
#'  * item 1:
#'
#' @md
#' @export
MacroPatch <- R6::R6Class(classname = "MacroPatch",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public methods & fields
                 public = list(

                   #################################################
                   # Initialize
                   #################################################

                   initialize = function(MacroPatch_PAR){

                     with(MacroPatch_PAR,{ # enter MacroPatch_PAR environment

                       # initialize shared parameters
                       private$N = N
                       private$hhID = hhID
                       private$humanIDs = humanIDs
                       private$bWeightHuman = bWeightHuman
                       private$bWeightZoo = bWeightZoo
                       private$bWeightZootox = bWeightZootox
                       private$Q = Q
                       private$kappa = kappa
                       private$aquaID = aquaID
                       private$aquaP = aquaP
                       private$aquaNewM = aquaNewM
                       private$weightAqua = weightAqua
                       private$weightOvitrap = weightOvitrap
                       private$weightSugar = weightSugar
                       private$weightBait = weightBait
                       private$weightMate = weightMate

                       # initialize AQUATIC ECOLOGY
                      #  do elsewhere.
                      #  if(aquaModule == "emerge"){
                       #
                      #    private$lambda = lambda
                       #
                      #  } else if(aquaModule == "EL4P"){
                      #    stop("sean hasn't written the routines for MACRO EL4P Aquatic Ecology")
                      #  } else {
                      #    stop("aquaModule must be a value in 'emerge' or 'EL4P'")
                      #  }


                      }) # exit MacroPatch_PAR environment

                   },

                   #################################################
                   # Getters and Setters
                   #################################################

                   # Biting weights
                   get_bWeightHuman = function(){
                      return(private$bWeightHuman)
                   },
                   set_bWeightHuman = function(bWeightHuman){
                      private$bWeightHuman = bWeightHuman
                   },
                   accumulate_bWeightHuman = function(bWeightHuman){
                     private$bWeightHuman = private$bWeightHuman + bWeightHuman
                   },

                   get_bWeightZoo = function(){
                      return(private$bWeightZoo)
                   },
                   set_bWeightZoo = function(bWeightZoo){
                      private$bWeightZoo = bWeightZoo
                   },

                   get_kappa = function(){
                     return(private$kappa)
                   },
                   set_kappa = function(kappa){
                     private$kappa = kappa
                   },
                   accumulate_kappa = function(kappa){
                     private$kappa = private$kappa + kappa
                   },

                   get_travelWeight = function(){
                     return(private$travelWeight)
                   },
                   set_travelWeight = function(travelWeight){
                     private$travelWeight = travelWeight
                   },

                   #################################################
                   # R6 Classes
                   #################################################

                   # MacroMosquitoPop
                   get_MacroMosquitoPop = function(){
                     return(private$MacroMosquitoPop)
                   },
                   set_MacroMosquitoPop = function(MacroMosquitoPop){
                     private$MacroMosquitoPop = MacroMosquitoPop
                   },

                   # HumanPop
                   get_HumanPop = function(){
                     return(private$HumanPop)
                   },
                   set_HumanPop = function(HumanPop){
                     private$HumanPop = HumanPop
                   },

                   #################################################
                   # Pointers
                   #################################################

                   # MacroTile
                   get_TilePointer = function(){
                     return(private$TilePointer)
                   },
                   set_TilePointer = function(TilePointer){
                     private$TilePointer = TilePointer
                   }

                  ),

                  # private methods & fields
                  private = list(

                    # Mosquito Feeding
                    bWeightHuman   = NULL,  # biting weight of HumanPop
                    bWeightZoo     = NULL,  # biting weight of livestock
                    kappa     = NULL,       # relative infectiousness

                    # Human Travel
                    travelWeight = NULL,    # weight for human travel

                    # R6 Classes
                    MacroMosquitoPop = NULL,
                    HumanPop = NULL,

                    # Pointers
                    TilePointer = NULL     # point to the enclosing metapopulation TILE (MACRO)

                  )

)
