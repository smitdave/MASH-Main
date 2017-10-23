#################################################################
#
#   MASH
#   R6-ified
#   MACRO MacroPatch Class Definition
#   David Smith, Hector Sanchez, Sean Wu
#   May 22, 2016
#
#################################################################

#' MACRO Patch Class Definition
#'
#' This is a generic collection MACRO patches blah blah ...
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
                       if(aquaModule == "emerge"){

                         private$season = season
                         private$PatchesImagoQ =  PatchesImagoQ
                         private$PatchesEggQ = PatchesEggQ

                        #  private$ImagoQ = MASH::ImagoQ()

                       } else if(aquaModule == "EL4P"){
                         stop("sean hasn't written the routines for MACRO EL4P Aquatic Ecology")
                       } else {
                         stop("aquaModule must be a value in 'emerge' or 'EL4P'")
                       }


                      }) # exit MacroPatch_PAR environment

                   },

                   #################################################
                   # Getters and Setters
                   #################################################

                   # N: number of patches
                   get_N = function(){
                     return(private$N)
                   },

                   # Houses (what household IDs are at each patch?)
                   get_hhID = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$hhID)
                     } else {
                       return(private$hhID[[ix]])
                     }
                   },

                   # Humans (where are people?)
                   get_humanIDs = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$humanIDs)
                     } else {
                       return(private$humanIDs[[ix]])
                     }
                   },
                   set_humanIDs = function(humanIDs, ix = NULL){
                     if(is.null(ix)){
                       private$humanIDs = humanIDs
                     } else {
                       private$humanIDs[[ix]] = humanIDs
                     }
                   },
                   add_humanIDs = function(oneID, ix){
                     # add a person to a patch
                     private$humanIDs[[ix]] = c(private$humanIDs[[ix]],oneID)
                   },
                   remove_humanIDs = function(oneID, ix){
                     # remove a person from a patch
                     private$humanIDs[[ix]] = private$humanIDs[[ix]][-which(private$humanIDs[[ix]]==oneID)]
                   },

                   # Biting weights
                   get_bWeightHuman = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$bWeightHuman)
                     } else {
                       return(private$bWeightHuman[ix])
                     }
                   },
                   set_bWeightHuman = function(bWeightHuman, ix = NULL){
                     # set one element or entire vector
                     if(!is.null(ix)){
                       private$bWeightHuman[ix] = bWeightHuman
                     } else {
                       private$bWeightHuman = bWeightHuman
                     }
                   },
                   accumulate_bWeightHuman = function(bWeightHuman, ix = NULL){
                     private$bWeightHuman[ix] = private$bWeightHuman[ix] + bWeightHuman
                   },

                   get_bWeightZoo = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$bWeightZoo)
                     } else {
                       return(private$bWeightZoo[ix])
                     }
                   },
                   set_bWeightZoo = function(bWeightZoo, ix = NULL){
                     # set one element or entire vector
                     if(!is.null(ix)){
                       private$bWeightZoo[ix] = bWeightZoo
                     } else {
                       private$bWeightZoo = bWeightZoo
                     }
                   },

                   get_bWeightZootox = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$bWeightZootox)
                     } else {
                       return(private$bWeightZootox[ix])
                     }
                   },
                   set_bWeightZootox = function(bWeightZootox, ix = NULL){
                     # set one element or entire vector
                     if(!is.null(ix)){
                       private$bWeightZootox[ix] = bWeightZootox
                     } else {
                       private$bWeightZootox = bWeightZootox
                     }
                   },

                   get_kappa = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$kappa)
                     } else {
                       return(private$kappa[ix])
                     }
                   },
                   set_kappa = function(kappa, ix = NULL){
                     # set one element or entire vector
                     if(!is.null(ix)){
                       private$kappa[ix] = kappa
                     } else {
                       private$kappa = kappa
                     }
                   },
                   accumulate_kappa = function(kappa, ix = NULL){
                     private$kappa[ix] = private$kappa[ix] + kappa
                   },

                   # Egg laying
                   get_aquaID = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$aquaID)
                     } else {
                       return(private$aquaID[ix])
                     }
                   },
                   set_aquaID = function(aquaID, ix = NULL){
                     if(!is.null(ix)){
                       private$aquaID[ix] = aquaID
                     } else {
                       private$aquaID = aquaID
                     }
                   },

                   get_aquaP = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$aquaP)
                     } else {
                       return(private$aquaP[ix])
                     }
                   },
                   set_aquaP = function(aquaP, ix = NULL){
                     if(!is.null(ix)){
                       private$aquaP[ix] = aquaP
                     } else {
                       private$aquaP = aquaP
                     }
                   },

                   get_aquaNewM = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$aquaNewM)
                     } else {
                       return(private$aquaNewM[ix])
                     }
                   },
                   set_aquaNewM = function(aquaNewM, ix = NULL){
                     if(!is.null(ix)){
                       private$aquaNewM[ix] = aquaNewM
                     } else {
                       private$aquaNewM = aquaNewM
                     }
                   },

                   get_weightAqua = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$weightAqua)
                     } else {
                       return(private$weightAqua[ix])
                     }
                   },
                   set_weightAqua = function(weightAqua, ix = NULL){
                     if(!is.null(ix)){
                       private$weightAqua[ix] = weightAqua
                     } else {
                       private$weightAqua = weightAqua
                     }
                   },

                   get_weightOvitrap = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$weightOvitrap)
                     } else {
                       return(private$weightOvitrap[ix])
                     }
                   },
                   set_weightOvitrap = function(weightOvitrap, ix = NULL){
                     if(!is.null(ix)){
                       private$weightOvitrap[ix] = weightOvitrap
                     } else {
                       private$weightOvitrap = weightOvitrap
                     }
                   },

                   # Sugar feeding
                   get_weightSugar = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$weightSugar)
                     } else {
                       return(private$weightSugar[ix])
                     }
                   },
                   set_weightSugar = function(weightSugar, ix = NULL){
                     if(!is.null(ix)){
                       private$weightSugar[ix] = weightSugar
                     } else {
                       private$weightSugar = weightSugar
                     }
                   },

                   get_weightBait = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$weightBait)
                     } else {
                       return(private$weightBait[ix])
                     }
                   },
                   set_weightBait = function(weightBait, ix = NULL){
                     if(!is.null(ix)){
                       private$weightBait[ix] = weightBait
                     } else {
                       private$weightBait = weightBait
                     }
                   },

                   # Mating
                   get_weightMate = function(ix = NULL){
                     if(is.null(ix)){
                       return(private$weightMate)
                     } else {
                       return(private$weightMate[ix])
                     }
                   },
                   set_weightMate = function(weightMate, ix = NULL){
                     if(!is.null(ix)){
                       private$weightMate[ix] = weightMate
                     } else {
                       private$weightMate = weightMate
                     }
                   },

                   #################################################
                   # Pointers
                   #################################################

                   # point to the enclosing metapopulation TILE (MACRO)
                   get_TilePointer = function(){
                     return(private$TilePointer)
                   },
                   set_TilePointer = function(TilePointer){
                     private$TilePointer = TilePointer
                   },

                   # point to the MacroMosquitoPop in this enclosing metapopulation TILE (MACRO)
                   get_MosquitoPointer = function(){
                     return(private$MosquitoPointer)
                   },
                   set_MosquitoPointer = function(MosquitoPointer){
                     private$MosquitoPointer = MosquitoPointer
                   },

                   # point to the HumanPop in this enclosing metapopulation TILE (MACRO)
                   get_HumansPointer = function(){
                     return(private$HumansPointer)
                   },
                   set_HumansPointer = function(HumansPointer){
                     private$HumansPointer = HumansPointer
                   }

                  ),

                  # private methods & fields
                  private = list(

                    N         = NULL, # number of patches

                    # Houses, for effect size estimation
                    # Can use same structures as MICRO for
                    # consistent modeling of vector control.
                    hhID      = list(),
                    humanIDs  = list(),

                    # How are infectious bites divided up?
                    bWeightHuman   = NULL,
                    bWeightZoo     = NULL,
                    bWeightZootox  = NULL,

                    # Net infectiousness
                    # Q         = NULL, in MacroMosquitoPop
                    kappa     = NULL,

                    #Egg laying
                    aquaID        = NULL,
                    aquaP         = NULL,
                    aquaNewM      = NULL,
                    weightAqua    = NULL,   # For modeling movement
                    weightOvitrap = NULL,

                    # Sugar feeding
                    weightSugar   = NULL,
                    weightBait    = NULL,

                    # Mating
                    weightMate    = NULL,

                    # Pointers
                    TilePointer = NULL, # point to the enclosing metapopulation TILE (MACRO)
                    MosquitoPointer = NULL, # point to the MacroMosquitoPop in this enclosing metapopulation TILE (MACRO)
                    HumansPointer = NULL # point to the HumanPop in this enclosing metapopulation TILE (MACRO)

                  )

)
