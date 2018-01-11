###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MICRO
#   Search: MoveObject Class Methods
#   MASH Team
#   January 2018
#
###############################################################################


# if you stay at a 'site' because there is a non-zero weight to stay there for your resource
# then when you do stuff that involves your resource later in another bout function
# you choose which one based off the weights attached to each.
# SEARCH only handles how you move between sites

#' Query a \code{\link{MoveObject}}
#'
#' Given an input index and destination resource, return the index of the \code{\link{Site}}
#' that the mosquito moves to, and distance traveled. Movement within a site (peri-domestic movement) is
#' accounted for through weights to stay 'here'.
#'  * this method is bound to \code{MoveObject$move}
#'
#' @param ix integer indicating index of current \code{\link{Site}}
#' @param dest character in {f,l,m,s} indicating destination resource
#' @return list with elements \code{newIx} and \code{dist}
#'
move_MoveObject <- function(ix, dest){

  # probability to stay here based on destination (what resource I am seeking)
  here = switch(dest,
    f = {private$move_obj[[ix]]$here_f},
    l = {private$move_obj[[ix]]$here_l},
    m = {private$move_obj[[ix]]$here_m},
    s = {private$move_obj[[ix]]$here_s},
    {stop(paste0("invalid entry for 'dest': ",dest))}
  )

  r = runif(1)

  # here movement
  if(r <= here){
    return(list(newIx=ix,dist=private$move_obj[[ix]]$here_dist))
  # near movement
  } else if(r <= here + private$move_obj[[ix]]$near){
    newIx = sample_aux(x=private$move_obj[[ix]]$near_ix,size=1,replace=FALSE,prob=private$move_obj[[ix]]$near_pr)
    return(list(newIx=newIx,dist=private$move_obj[[ix]]$near_dist[newIx]))
  # far movement
  } else {
    stop(paste0("far movement not currently implemented; please check movement from site: ",ix))
  }
}

MoveObject$set(which = "public",name = "move",
    value = move_MoveObject, overwrite = TRUE
)
