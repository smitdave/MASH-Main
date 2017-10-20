####################################################################################
#
#   MASH
#   R6-ified
#   MICRO Landscape Utility Functions
#   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
#   July 18, 2017
#
####################################################################################


##########################################
# Auxiliary Functions
##########################################

#' Sample Equally from Color Space
#'
#' This function is a low-level utility to sample at equal points from the color wheel to produce ggplot2 color scheme.
#'
#' @param n number of colors to sample
#' @param alpha opacity
#' @return a vector of colors in hex format
#' @examples
#' ggCol_utility(n=10, alpha = 0.5)
#' @export
ggCol_utility <- function(n, alpha = 1) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alpha)[1:n]
}

#' Sample Equally from Color Space with Offset
#'
#' This function is a low-level utility to sample at equal points from the color wheel to produce ggplot2 color scheme.
#' This is a modification of \code{\link{ggCol_utility}} with a simple offset where sampling begins at 1 + offset.
#'
#' @param n number of colors to sample
#' @param offset offset from 1
#' @param alpha opacity
#' @return a vector of colors in hex format
#' @examples
#' ggColOffset_utility(n=10, offset = 2, alpha = 0.5)
#' @export
ggColOffset_utility <- function(n,offset,alpha=1){
  hues = seq(15, 375, length = n + 1 + offset)
  hcl(h = hues,l = 65,c = 100, alpha = alpha)[(1+offset):(offset+n)]
}

#' Brighten or Darken Colors
#'
#' With input of hcl colors (hex code), brighten or darken by a factor
#'
#' @param color vector of hcl colors
#' @param factor factor to brighten or darken colors
#' @param bright logical variable to brighten or darken
#' @param alpha opacity
#' @return a vector of colors in hex format
#' @examples
#' colLuminosity_utility(color=MASH::ggCol_utility(n=5), factor = 1.15, bright = TRUE)
#' @export
colLuminosity_utility <- function(color,factor,bright,alpha=NULL){

  if(!is.logical(bright)){ #sanity check
    stop("i don't know if you wan't me to make your color brighter or darker!")
  }

  col = col2rgb(color,alpha=FALSE) #convert to rgba color space
  if(bright){
    col = col*factor
  } else {
    col = col/factor
  }
  if(!is.null(alpha)){ #adjust alpha if specified
    rbind(col,alpha = 255 * alpha)
  }
  col = rgb(t(as.matrix(apply(col, 1, function(x) if (x > 255) 255 else x))), maxColorValue=255)
  return(col)
}


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
