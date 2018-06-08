###############################################################################
#       _______ __
#      /_  __(_) /__
#       / / / / / _ \
#      / / / / /  __/
#     /_/ /_/_/\___/
#
#     Tile: Initialize landscape elements
#     MBITES Team
#     February 2018
#
###############################################################################

#' Add a Tile to MBITES Simulation
#'
#' @description
#'
#' To add a tile to MBITES, \code{\link{Tile_Initialize}} must be called to
#' initialize the landscape (collection of sites and associated resources) on a tile prior to
#' initializing human or mosquito populations. This function generates a \code{\link{Tile}} by calling its constructor
#' which automatically gets a new tile ID and registers the tile with \code{\link{MBITES_Globals}}.
#'
#' say something about the rest of things on the tile.
#'
#' @details
#'  1. Initialize Landscape:
#'    * \code{\link{Tile_Initialize}}: see the help file for the expected parameters
#'
#'  2. Initialize Humans:
#'
#'
#'  3. Initialize Mosquitoes:
#'
#'
#'  4. (Optional) Initialize Pathogens:
#'
#'
#'
#' @name Initialize Tile
NULL
#> NULL


###############################################################################
# Initialize LANDSCAPE
###############################################################################

#' Initialize Landscape
#'
#' Initialize \code{\link{Site}} objects and their resources on a tile. This function should be called once for each
#' \code{\link{Tile}} being initialized.
#'
#' @param landscape a \code{\link{list}} object where each element corresponds to a site
#'
#' @details
#' A description of the parameter format this function expects is given below:
#'
#'  **Site:**
#'    * \code{id}: integer site id
#'    * \code{xy}: numeric vector of length 2 (xy-coordinates)
#'    * \code{type}: integer site type (1 for homestead, if not 1 will assume it is a site without a structure)
#'    * \code{move}: numeric vector of movement probabilities
#'    * \code{move_id}: integer vector of id's of other sites corresponding to the movement probabilities in \code{move}
#'    * \code{sugar}: if not \code{NULL}, a list of parameters of length >0 for \code{\link{Sugar_Resource}} objects at this site, see below for details
#'    * \code{mate}: if not \code{NULL}, a list of parameters of length >0 for \code{\link{Mating_Resource}} objects at this site
#'    * \code{feed}: if not \code{NULL}, a list of parameters of length >0 for \code{\link{Feeding_Resource}} objects at this site
#'    * \code{aqua}: if not \code{NULL}, a list of parameters of length >0 for \code{\link{Aqua_Resource}} objects at this site
#'
#'  **Sugar resources:**
#'    * \code{w}: numeric weight; used by mosquitoes to select sugar resources if there are more than 1.
#'
#'  **Mating resources:**
#'    * \code{w}: numeric weight; used by mosquitoes to select mating resources if there are more than 1.
#'
#'  **Blood Feeding resources:**
#'    * \code{w}: numeric weight; used by mosquitoes to select blood feeding resources if there are more than 1.
#'    * \code{enterP}: numeric probability for mosquito to successfully enter the house
#'    * \code{zoo_id}: integer id of zoo host (can be \code{NULL})
#'    * \code{zoo_w}: numeric weight of zoo host (can be \code{NULL})
#'
#'  **Aquatic Habitat resources:**
#'    * \code{w}: numeric weight; used by mosquitoes to select aquatic habitat resources if there are more than 1.
#'
#'  **Aquatic Habitat 'Emerge' model additional parameters:**
#'    * \code{lambda}: either length 1 numeric or numeric vector of length 365 for daily emergence
#'
#'  **Aquatic Habitat 'EL4P' model additional parameters:**
#'
#' @export
Tile_Initialize <- function(landscape){

  n = length(landscape)
  pb = txtProgressBar(min = 0, max = n, initial = 0)
  tile = Tile$new() # make a new tile and register it with MBITES:::Globals
  tileID = tile$get_id() # id of the new tile

  # tiles need to be initialized in order (eg; from 1,... increasing)
  if(tileID != landscape[[1]]$tileID){
    stop("MBITES Globals registered a new tile with ID: ",tileID," but site has tile ID: ",landscape[[1]]$tileID,", please fix discrepancy\n")
  }

  for(s in 1:n){

    site_p = landscape[[s]] # parameters for this site

    # make the site
    site = Site$new(id=site_p$id,xy=site_p$xy,tileID=site_p$tileID,type=site_p$type,move=site_p$move,move_id=site_p$move_id,haz=site_p$haz)

    # add resources (if present)
    if(!is.null(site_p$sugar)){
      for(i in site_p$sugar){
        sugar = Sugar_Resource$new(w=i$w,site=site) # make a sugar resource
        site$add_sugar(sugar) # add it to the site
      }
    }

    if(!is.null(site_p$mate)){
      for(i in site_p$mate){
        mate = Mating_Resource$new(w=i$w,site=site) # make a mating resource
        site$add_mate(mate) # add it to the site
      }
    }

    if(!is.null(site_p$feed)){
      for(i in site_p$feed){
        feed = Feeding_Resource$new(w=i$w,site=site,enterP=i$enterP)
        if(!is.null(i$zoo_id)){
          feed$RiskQ$add2Q_zoo(i$zoo_id,i$zoo_w)
        }
        site$add_feed(feed)
      }
    }

    if(!is.null(site_p$aqua)){
      for(i in site_p$aqua){
        if(MBITES:::Parameters$get_aqua_model()=="emerge"){
          aqua = Aqua_Resource_Emerge$new(w=i$w,site=site,lambda=i$lambda)
          site$add_aqua(aqua)
        }
        if(MBITES:::Parameters$get_aqua_model()=="EL4P"){
          stop("EL4P model not finished\n")
        }
      }
    }

    MBITES:::Globals$get_tile(tileID)$get_sites()$assign(key=site_p$id,value=site)

    setTxtProgressBar(pb,s)
  }

}
