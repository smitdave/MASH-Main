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
#' Initialize \code{\link[MBITES]{Site}} objects and their resources on a tile.
#'
#' @param landscape a \code{\link{list}} object where each row corresponds to a site
#'
Tile_Initialize <- function(landscape){

  n = nrow(landscape)
  pb = txtProgressBar(min = 0, max = n, initial = 0)

  for(i in 1:n){

    site_p = landscape[[i]] # parameters for this site
    tile = site_p$tileID # the tile this site is in

    # make the site
    site = Site$new(id=site_p$id,xy=site_p$xy,tileID=tile,type=site_p$type,move=site_p$move,move_id=site_p$move_id)

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
        feed = Feeding_Resource$new(w=i$w,site=site)
        site$add_feed(feed)
      }
    }

    if(!is.null(site_p$aqua)){
      if(MBITES:::Parameters$get_aqua_model()=="emerge"){

      }
      if(MBITES:::Parameters$get_aqua_model()=="EL4P"){
        stop("EL4P model not finished\n")
      }
      # do check for EL4P or Emerge
    }

    MBITES:::Globals$get_tile(tile)$get_sites()$assign(key=site_p$id,value=site)

    setTxtProgressBar(pb,i)
  }
}
