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


###############################################################################
# Initialize LANDSCAPE
###############################################################################

#' Initialize Landscape
#'
#' Initialize \code{\link{Site}} objects and their resources on a tile. This function should be called once for each
#' \code{\link{Tile}} being initialized.
#'
#' @param landscape a \code{\link{list}} object where each row corresponds to a site
#'
#' @details
#'
#'
#'
#' @export
Tile_Initialize <- function(landscape){

  n = length(landscape)
  pb = txtProgressBar(min = 0, max = n, initial = 0)
  tile = Tile$new() # make a new tile and register it with MBITES:::Globals
  tileID = tile$get_id() # id of the new tile

  for(s in 1:n){

    site_p = landscape[[s]] # parameters for this site

    # make the site
    site = Site$new(id=site_p$id,xy=site_p$xy,tileID=tileID,type=site_p$type,move=site_p$move,move_id=site_p$move_id)

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
        feed = Feeding_Resource$new(w=i$w,site=site,enter_p=i$enter_p)
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
