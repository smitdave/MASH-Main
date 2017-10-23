#################################################################
#
#   MASH
#   R6-ified
#   Minimal Landscape for Well-mixed Patch
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   May 9, 2017
#
#################################################################


#################################################################
# Landscape Definition
#################################################################

#' MICRO Landscape Class Definition
#'
#' This is a Landscape object that is part of a MICRO microsimulation Tile object, defined in \code{\link{MicroTile}}.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Methods:
#'  * **Constructor**
#'    * new: initialize a new \code{Landscape} object
#'      * Arguments:
#'        * \code{Landscape_PAR}: see \code{\link{Landscape.Parameters}} for parameter generation function.
#'  * **Getters & Setters**
#'    * get_FeedingSites: get \code{private$FeedingSites}
#'      * Arguments:
#'        * \code{ixS}: if \code{NULL} return all sites, otherwise return site indexed by \code{ixS}
#'    * set_FeedingSites: set \code{private$FeedingSites}
#'      * Arguments:
#'        * \code{FeedingSites}: replacement object (see below)
#'        * \code{ixS}: if \code{NULL} replace all sites, otherwise replace site indexed by \code{ixS}
#'    * get_AquaSites: get \code{private$AquaSites}
#'      * Arguments:
#'        * \code{ixS}: if \code{NULL} return all sites, otherwise return site indexed by \code{ixS}
#'    * set_AquaSites: set \code{private$AquaSites}
#'      * Arguments:
#'        * \code{AquaSites}: replacement object (see below)
#'        * \code{ixS}: if \code{NULL} replace all sites, otherwise replace site indexed by \code{ixS}
#'    * get_SugarSites: get \code{private$SugarSites}
#'    * set_SugarSites: set \code{private$SugarSites}
#'    * get_MatingSites: get \code{private$MatingSites}
#'    * set_MatingSites: set \code{private$MatingSites}
#'  * **Pointers**
#'    * get_TilePointer: get \code{\link{MicroTile}} pointer
#'    * set_TilePointer: set \code{\link{MicroTile}} pointer
#'    * set_FemalePopPointer:
#'    * set_FemalePopPointer:
#'    * get_MosquitoPopMalePointer:
#'    * set_MosquitoPopMalePointer:
#'    * get_HumansPointer: get \code{\link{HumanPop}} pointer
#'    * set_HumansPointer: set \code{\link{HumanPop}} pointer
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @md
#' @export
Landscape <- R6::R6Class(classname = "Landscape",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                  #################################################
                  # Initialize
                  #################################################

                  #  initialize
                   initialize = function(Landscape_PAR){

                     with(Landscape_PAR,{

                         #########################################
                         # Generate Feeding Sites
                         #########################################

                         private$FeedingSites = vector(mode="list",length=FeedingSite_PAR$nFeed)
                         self$FeedingSitesN = FeedingSite_PAR$nFeed
                         for(ix in 1:FeedingSite_PAR$nFeed){

                           private$FeedingSites[[ix]] = FeedingSite$new(
                             ix = ix,
                             siteXY = c(FeedingSite_PAR$siteXY$x[ix],FeedingSite_PAR$siteXY$y[ix]),
                             searchWt = FeedingSite_PAR$searchWt[ix],
                             enterP = FeedingSite_PAR$enterP[ix],
                             hazV = FeedingSite_PAR$hazV[ix],
                             hazW = FeedingSite_PAR$hazW[ix],
                             hazI = FeedingSite_PAR$hazI[ix],
                             sugar = FeedingSite_PAR$sugar[ix])

                         }

                         #########################################
                         # Generate Aquatic Habitats
                         #########################################

                         private$AquaSites = vector(mode="list",length=AquaticSite_PAR$nAqua)
                         self$AquaSitesN = AquaticSite_PAR$nAqua
                         for(ix in 1:AquaticSite_PAR$nAqua){

                           private$AquaSites[[ix]] = AquaticSite$new(
                            ix = ix,
                            siteXY = c(AquaticSite_PAR$siteXY$x[ix],AquaticSite_PAR$siteXY$y[ix]),
                            searchWt = AquaticSite_PAR$searchWt[ix],
                            module = AquaticSite_PAR$module,
                            lambda = AquaticSite_PAR$lambda[[ix]],
                            haz = AquaticSite_PAR$haz[ix])

                         }

                       })

                       #########################################
                       # Set Pointers
                       #########################################

                       for(ix in 1:self$FeedingSitesN){
                         private$FeedingSites[[ix]]$set_LandscapePointer(self)
                       }

                       for(ix in 1:self$AquaSitesN){
                         private$AquaSites[[ix]]$set_LandscapePointer(self)
                       }

                   },

                  #################################################################
                  # Getters & Setters
                  #################################################################

                  # FeedingSites
                  get_FeedingSites = function(ixS = NULL){
                    if(is.null(ixS)){
                      return(private$FeedingSites)
                    } else {
                      return(private$FeedingSites[[ixS]])
                    }
                  },
                  set_FeedingSites = function(FeedingSites, ixS = NULL){
                    if(is.null(ixS)){
                      private$FeedingSites = FeedingSites
                    } else {
                      private$FeedingSites[[ixS]] = FeedingSites
                    }
                  },

                  # AquaSites
                  get_AquaSites = function(ixS = NULL){
                    if(is.null(ixS)){
                      return(private$AquaSites)
                    } else {
                      return(private$AquaSites[[ixS]])
                    }
                  },
                  set_AquaSites = function(AquaSites, ixS = NULL){
                    if(is.null(ixS)){
                      private$AquaSites = AquaSites
                    } else {
                      private$AquaSites[[ixS]] = AquaSites
                    }
                  },

                  # SugarSites
                  get_SugarSites = function(ixS = NULL){
                    if(is.null(ixS)){
                      return(private$MatingSites)
                    } else {
                      return(private$MatingSites[[ixS]])
                    }
                  },
                  set_SugarSites = function(SugarSites, ixS = NULL){
                    if(is.null(ixS)){
                      private$SugarSites = SugarSites
                    } else {
                      private$SugarSites[[ixS]] = SugarSites
                    }
                  },

                  # MatingSites
                  get_MatingSites = function(ixS = NULL){
                    if(is.null(ixS)){
                      return(private$MatingSites)
                    } else {
                      return(private$MatingSites[[ixS]])
                    }
                  },
                  set_MatingSites = function(MatingSites, ixS = NULL){
                    if(is.null(ixS)){
                      private$MatingSites = MatingSites
                    } else {
                      private$MatingSites[[ixS]] = MatingSites
                    }
                  },

                  #################################################################
                  # Pointers
                  #################################################################

                  # TilePointer
                  get_TilePointer = function(){
                    return(private$TilePointer)
                  },
                  set_TilePointer = function(TilePointer){
                    private$TilePointer = TilePointer
                  },

                  # FemalePopPointer
                  get_FemalePopPointer = function(){
                    return(private$FemalePopPointer)
                  },
                  set_FemalePopPointer = function(FemalePopPointer){
                    private$FemalePopPointer = FemalePopPointer
                  },

                  # MalePopPointer
                  get_MalePopPointer = function(){
                    return(private$MalePopPointer)
                  },
                  set_MalePopPointer = function(MalePopPointer){
                    private$MalePopPointer = MalePopPointer
                  },

                  # HumansPointer
                  get_HumansPointer = function(){
                    return(private$HumansPointer)
                  },
                  set_HumansPointer = function(HumansPointer){
                    private$HumansPointer = HumansPointer
                  },

                  #################################################################
                  # Site-specific Functions
                  #################################################################

                  clear_RiskQ = function(){
                    for(ixF in 1:self$FeedingSitesN){
                      private$FeedingSites[[ixF]]$clear_RiskQ()
                    }
                  },

                  #################################################################
                  # Public Fields
                  #################################################################

                  # Number of Sites
                  FeedingSitesN = NULL,
                  AquaSitesN = NULL,
                  SugarSitesN = NULL,
                  MatingSitesN = NULL

                 ),

                 # private members
                 private = list(

                   # Site Types
                   FeedingSites = NULL,
                   AquaSites = NULL,
                   SugarSites = NULL,
                   MatingSites = NULL,

                   # Pointers
                   TilePointer = NULL,                    # point to the enclosing microsimulation TILE (MICRO)
                   FemalePopPointer = NULL,       # point to the MicroMosquitoPopFemale in this enclosing microsimulation TILE (MICRO)
                   MalePopPointer = NULL,         # point to the MicroMosquitoPopMale in this enclosing microsimulation TILE (MICRO)
                   HumansPointer = NULL                   # point to the HumanPop in this enclosing microsimulation TILE (MICRO)


                 )
)
