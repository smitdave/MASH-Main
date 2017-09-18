###############################################################################
#       __                    __
#      / /   ____ _____  ____/ /_____________ _____  ___
#     / /   / __ `/ __ \/ __  / ___/ ___/ __ `/ __ \/ _ \
#    / /___/ /_/ / / / / /_/ (__  ) /__/ /_/ / /_/ /  __/
#   /_____/\__,_/_/ /_/\__,_/____/\___/\__,_/ .___/\___/
#                                          /_/
#
#   MASH-MICRO
#   MICRO: Landscape Class Definition
#   MASH-MICRO Team
#   May 9, 2017
#
###############################################################################


#################################################################
# Landscape Definition
#################################################################

#' Landscape Class Definition
#'
#' im a class!
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: im an agument!
#'
#' @section **Methods**:
#'  * method: im a method!
#'
#' @section **Fields**:
#'  * field: im a field!
#'
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
                         # Generate Blood Feeding Sites
                         #########################################

                         private$FeedingSites = vector(mode="list",length=FeedingSite_PAR$nFeed)
                         private$FeedingSitesN = FeedingSite_PAR$nFeed
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
                         private$AquaSitesN = AquaticSite_PAR$nAqua
                         for(ix in 1:AquaticSite_PAR$nAqua){

                           private$AquaSites[[ix]] = AquaticSite$new(
                            ix = ix,
                            siteXY = c(AquaticSite_PAR$siteXY$x[ix],AquaticSite_PAR$siteXY$y[ix]),
                            searchWt = AquaticSite_PAR$searchWt[ix],
                            module = AquaticSite_PAR$module,
                            lambda = AquaticSite_PAR$lambda[[ix]],
                            haz = AquaticSite_PAR$haz[ix])

                         }

                         #########################################
                         # Generate Mating Sites
                         #########################################

                         if(!is.null(MatingSite_PAR)){

                         }

                         #########################################
                         # Generate Sugar Feeding Sites
                         #########################################

                         if(!is.null(SugarSite_PAR)){

                         }

                       })

                       #########################################
                       # Set Pointers
                       #########################################

                       for(ix in 1:private$FeedingSitesN){
                         private$FeedingSites[[ix]]$set_LandscapePointer(self)
                       }

                       for(ix in 1:private$AquaSitesN){
                         private$AquaSites[[ix]]$set_LandscapePointer(self)
                       }

                   }

                 ),

                 # private members
                 private = list(

                   # Site Types
                   FeedingSites = NULL,
                   AquaSites = NULL,
                   SugarSites = NULL,
                   MatingSites = NULL,

                   FeedingSitesN = numeric(1),
                   AquaSitesN = numeric(1),
                   SugarSitesN = numeric(1),
                   MatingSitesN = numeric(1),

                   # Pointers
                   TilePointer = NULL,                    # point to the enclosing microsimulation TILE (MICRO)
                   FemalePopPointer = NULL,       # point to the MosquitoPopFemale in this enclosing microsimulation TILE (MICRO)
                   MalePopPointer = NULL,         # point to the MosquitoPopMale in this enclosing microsimulation TILE (MICRO)
                   HumansPointer = NULL                   # point to the HumanPop in this enclosing microsimulation TILE (MICRO)

                 )
)
