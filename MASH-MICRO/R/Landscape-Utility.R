###############################################################################
#       __                    __
#      / /   ____ _____  ____/ /_____________ _____  ___
#     / /   / __ `/ __ \/ __  / ___/ ___/ __ `/ __ \/ _ \
#    / /___/ /_/ / / / / /_/ (__  ) /__/ /_/ / /_/ /  __/
#   /_____/\__,_/_/ /_/\__,_/____/\___/\__,_/ .___/\___/
#                                          /_/
#
#   MASH-MICRO
#   MICRO: Landscape Utility
#   MASH-MICRO Team
#   May 9, 2017
#
###############################################################################


##########################################
# Plot Landscape
##########################################

#' Plot a MICRO Landscape
#'
#' Given object of class \code{\link{Landscape}}, produce a plot.
#'
#' @param Landscape \code{\link{Landscape}} object
#' @param xLim x-limits of landscape
#' @param yLim y-limits of landscape
#' @param offset additional space on edges of plot
#' @param bgCol background color of plotting surface
#' @param cex size of points
#' @export
MicroLandscapePlot_utility <- function(Landscape, xLim = c(0,1), yLim = c(0,1), offset = 0.05, bgCol = "grey20", cex = 0.75){

  # extract coordinates
  nCol = 0
  if(!is.null(Landscape$get_FeedingSites())){
    feedXY = t(vapply(X = Landscape$get_FeedingSites(),FUN = function(x){x$get_siteXY()},FUN.VALUE = numeric(2)))
    nCol = nCol + 1
  }
  if(!is.null(Landscape$get_AquaSites())){
    aquaXY = t(vapply(X = Landscape$get_AquaSites(),FUN = function(x){x$get_siteXY()},FUN.VALUE = numeric(2)))
    nCol = nCol + 1
  }
  if(!is.null(Landscape$get_SugarSites())){
    sugarXY = t(vapply(X = Landscape$get_SugarSites(),FUN = function(x){x$get_siteXY()},FUN.VALUE = numeric(2)))
    nCol = nCol + 1
  }
  if(!is.null(Landscape$get_MatingSites())){
    mateXY = t(vapply(X = Landscape$get_MatingSites(),FUN = function(x){x$get_siteXY()},FUN.VALUE = numeric(2)))
    nCol = nCol + 1
  }

  #set up plotting options
  defaultPars = par()
  par(bg=bgCol,mar=c(0,0,0,0),mgp=c(0,0,0))

  #colors
  setup_col = ggCol_utility(n=nCol)

  #set up empty grid
  plot(1,type="n",axes=F,frame.plot=F,ann=F,xlim=c(xLim[1]-offset,xLim[2]+offset),
       ylim=c(yLim[1]-offset,yLim[2]+offset))

  # plot
  legend = NULL
  if(exists(x = "feedXY")){
    points(x = feedXY, pch = 15, col = setup_col[1], cex = cex) #draw feeding sites
    legend = "Feeding Site"
  }
  if(exists(x = "aquaXY")){
    points(x = aquaXY, pch = 16, col = setup_col[2], cex = cex) #draw aquatic habitats
    legend = c(legend,"Aquatic Habitat")
  }
  if(exists(x = "sugarXY")){
    points(x = sugarXY, pch = 17, col = setup_col[3], cex = cex) #draw sugar sources
    legend = c(legend,"Sugar Source")
  }
  if(exists(x = "mateXY")){
    points(x = mateXY, pch = 18, col = setup_col[4], cex = cex) #draw mating sites
    legend = c(legend,"Mating Site")
  }

  legend(x = "topleft", legend = legend,pch = 15:18,
         col = setup_col, bty = "n", text.col = "grey80")

  # reset graphical parameters
  par(bg=defaultPars$bg,mar=defaultPars$mar,mgp=defaultPars$mgp)
}
