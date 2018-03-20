###############################################################################
#         __  __
#        / / / /_  ______ ___  ____ _____
#       / /_/ / / / / __ `__ \/ __ `/ __ \
#      / __  / /_/ / / / / / / /_/ / / / /
#     /_/ /_/\__,_/_/ /_/ /_/\__,_/_/ /_/
#
#     NULL Human-Class
#     MBITES Team
#     February 2018
#
###############################################################################


###############################################################################
# Class definition
###############################################################################

#' NULL Human Class
#'
#' A \code{Human_NULL} can be used as a drop-in replacement when human dynamics do not need
#' to be simulated explicitly.
#'
#'
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * id: integer id
#'  * w: numeric biting weight
#'  * feedingID: id of the \code{\link{Feeding_Resource}} where my biting weight will be added
#'  * siteID: id of the \code{\link{Site}} where my feeding site resides
#'  * tileID: id of the \code{\link{Tile}} where I reside
#'
#' @section **Methods**:
#'  * method: i'm a method!
#'
#' @section **Fields**:
#'  * id: integer id
#'  * w: numeric biting weight
#'  * UNBITTEN: logical flag
#'  * mosquito_id: integer vector of mosquitoes that have bitten me
#'  * mosquito_t: numeric vector of times i was bitten
#'  * bloodFeed: \code{FALSE} corresponds to probing-only events, \code{TRUE} corresponds to combined probe-bloodfeed events.
#'
#' @export
Human_NULL <- R6::R6Class(classname = "Human_NULL",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(id,w,feedingID,siteID,tileID){
                     futile.logger::flog.trace("Human_NULL %i being born at self: %s , private: %s",id,pryr::address(self),pryr::address(private))

                     # basic parameters
                     private$id = id
                     private$w = w

                     # location fields
                     private$feedingID = feedingID
                     private$siteID = siteID
                     private$tileID = tileID

                     # add my risk to my home site
                     MBITES:::Globals$get_tile(tileID)$get_sites()$get(siteID)$get_feed(feedingID)$RiskQ$add2Q(id,w,1)
                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     futile.logger::flog.trace("Human_NULL %i being killed at self: %s , private: %s",private$id,pryr::address(self),pryr::address(private))
                   } # end destructor

                 ),

                 # private members
                 private = list(

                   # local fields
                   id                  = integer(1), # my id
                   w                   = numeric(1), # my biting weight

                   # location fields
                   feedingID           = integer(1),
                   siteID              = integer(1),
                   tileID              = integer(1),

                   # biting dynamics
                   UNBITTEN            = TRUE, # have i been bitten yet?
                   mosquito_id         = integer(1), # vector of mosquitoes that have bitten me
                   mosquito_t          = numeric(1), # vector of times i was bitten
                   bloodFeed           = logical(1) # F for probing-only events, T for blood feeding events

                 )
) # end Human_NULL class definition


###############################################################################
# Human methods for logging bites
###############################################################################

#' Push Host Probing Event to History
#'
#' If using the null human and pathogen model this function logs host probing events on this \code{\link{Human_NULL}}.
#'
#' @param m_id id of the \code{\link{Mosquito_Female}}
#' @param t time of the probing event
#'
pushProbe_Human_NULL <- function(m_id,t){
  if(private$UNBITTEN){
    private$mosquito_id = m_id
    private$mosquito_t = t
    private$probe = FALSE
    private$UNBITTEN = FALSE
  } else {
    private$mosquito_id = append(private$mosquito_id,m_id)
    private$mosquito_t = append(private$mosquito_t,t)
    private$probe = append(private$probe,FALSE)
  }
}

#' Push Host Blood feeding Event to History
#'
#' If using the null human and pathogen model this function logs host blood feeding events on this \code{\link{Human_NULL}}.
#'
pushFeed_Human_NULL <- function(){
  private$probeOnly[length(private$probeOnly)] = TRUE
}

Human_NULL$set(which = "public",name = "pushProbe",
    value = pushProbe_Human_NULL, overwrite = TRUE
)

Human_NULL$set(which = "public",name = "pushFeed",
    value = pushFeed_Human_NULL, overwrite = TRUE
)


###############################################################################
# Mosquito methods for probeHost and feedHost
# normally, the functions below would go in the PATHOGEN-XX-XX.R file
#' @include MBITES-Mosquito.R
###############################################################################

#' Null Host Probing
#'
#' This method fills in for pathogen model specific host probing (mosquito to human transmission) methods
#' if using the null human & pathogen model. It calls \code{\link{pushProbe_Human_NULL}} to log probing events.
#'
probeHost_NULL <- function(){
  MBITES:::Globals$get_tile(private$tileID)$get_human(private$hostID)$pushProbe(m_id=private$id,t=private$tNow)
}

#' Null Host Blood feeding
#'
#' This method fills in for pathogen model specific host blood feeding (human to mosquito transmission) methods
#' if using the null human & pathogen model. It calls \code{\link{pushFeed_Human_NULL}} to log probing events.
#'
feedHost_NULL <- function(){
  MBITES:::Globals$get_tile(private$tileID)$get_human(private$hostID)$pushFeed()
}

Mosquito_Female$set(which = "public",name = "probeHost",
    value = probeHost_NULL, overwrite = TRUE
)

Mosquito_Female$set(which = "public",name = "feedHost",
    value = feedHost_NULL, overwrite = TRUE
)
