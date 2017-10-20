# ####################################################################################
# #
# #   MASH
# #   R6-ified
# #   MICRO Tile Class Parameters
# #   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
# #   May 31, 2017
# #
# ####################################################################################
#
#
# ####################################################################################
# # Parameter Generation Functions
# ####################################################################################
#
# #' MICRO: Generate Parameters for \code{\link{MicroTile}} Object
# #'
# #' This function is a specific instantiation of a generic system to generate parameters for a
# #' chosen microsimulation Tile. Any user-specified function can be written to generate parameters, as long as the
# #' return list is in the same format.
# #'
# #' @param nFeed number of feeding sites
# #' @param nAqua number of aquatic habitats
# #' @param pointGen character to select spatial point pattern generation function
# #'  * "poisson": \code{\link{pointsPoisson}}
# #'  * "clustered": \code{\link{pointsClustered}}
# #'  * "overdispersed": \code{\link{pointsOverdispersed}}
# #'  * "lattice": \code{\link{pointsLattice}}
# #' @param module character
# #'  * "emerge": initialize parameters for Emerge module of Aquatic Ecology
# #'  * "EL4P": initialize parameters for EL4P module of Aquatic Ecology
# #' @param modulePars additional list of named parameters to be passed to Aquatic Ecology module specific parameter generating functions
# #'  * Emerge: for details see \code{\link{makeLambda_MicroEmerge}}
# #'  * EL4P:
# #' @param hazV mean value for feeding site vegetation landing hazard (if 0 it is set to 0 for all sites)
# #' @param hazW mean value for feeding site outside wall landing hazard (if 0 it is set to 0 for all sites)
# #' @param hazI mean value for feeding site indoor wall landing hazard (if 0 it is set to 0 for all sites)
# #' @param haz mean value for aquatic habitat landing hazard (if 0 it is set to 0 for all sites)
# #' @param searchFeed vector of searchWt for feeding sites (if \code{NULL} initialize to Gamma(1,1) distribution)
# #' @param searchAqua vector of searchWt for aquatic habitats (if \code{NULL} initialize to Gamma(1,1) distribution)
# #' @param enterP vector of house entry probabilities or single numeric value for all sites (if \code{NULL} initialize to Beta(9,1) distribution)
# #' @param xLim x-axis bounds for simulated points
# #' @param yLim y-axis bounds for simulated points
# #' @param aquaSD standard deviation of aquatic habitat scatter around feeding sites
# #' @param hhSize average number of humans at feeding sites
# #' @param hhMin minimum number of humans at feeding sites
# #' @param bWeight numeric value of biting weights on \code{\link{Human}} (if \code{NULL} biting weights are Gamma(1,1) distributed)
# #' @param ... additional named arguments for pointGen()
# #' @return a named list of parameters
# #' * Landscape_PAR: see \code{\link{Landscape.Parameters}} for details
# #' * HumanPop_PAR: see \code{\link{HumanPop.Parameters}} for details
# #' @md
# #'
# #' @export
# MICRO.Tile.Parameters <- function(
#     nFeed,
#     nAqua,
#     pointGen = "poisson",
#     module,
#     modulePars,
#     hazV = 0,
#     hazW = 0,
#     hazI = 0,
#     haz = 0,
#     searchFeed = NULL,
#     searchAqua = NULL,
#     enterP = NULL,
#     xLim = c(0,1),
#     yLim = c(0,1),
#     aquaSD = 0.025,
#     hhSize = 7,
#     hhMin = 2,
#     bWeight = NULL,
#     ...
#   ){
#
#     Landscape_PAR = Landscape.Parameters(nFeed=nFeed,nAqua=nAqua,pointGen=pointGen,module=module,modulePars=modulePars,
#                                           hazV=hazV,hazW=hazW,hazI=hazI,haz=haz,searchFeed=searchFeed,searchAqua=searchAqua,
#                                           enterP=enterP,xLim=xLim,yLim=yLim,aquaSD=aquaSD,...)
#     HumanPop_PAR = HumanPop.Parameters(nSite = nFeed, bWeight = bWeight, siteSize = hhSize, siteMin = hhMin)
#
#     MicroTile_PAR = list(Landscape_PAR=Landscape_PAR,HumanPop_PAR=HumanPop_PAR)
# }
