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

#' HUMAN: Null Human
#'
#' The null human class (see \code{\link{Human_NULL}}) is a simple logging class to record biting events
#' when human dynamics are not of interest. It can only be used with the \code{\link{PathogenNull}} model.
#'
#'
#' @name HUMAN-Null
NULL
#> NULL

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
# export history and remove self.
###############################################################################

#' export history and remove self.
exit_Human_NULL <- function(){
  cat(jsonlite::toJSON(x = list(
          id = private$id,
          tile = private$tileID,
          site = private$siteID,
          bite_id = private$mosquito_id,
          bite_times = private$mosquito_t,
          bloodfeed_bool = private$bloodFeed
      ), pretty = MBITES:::Globals$pretty),",\n",sep="",file=MBITES:::Globals$get_human_out())
  # remove the human
  MBITES:::Globals$get_tile(private$tileID)$get_humans()$rm(private$id)
}

Human_NULL$set(which = "public",name = "exit",
    value = exit_Human_NULL, overwrite = TRUE
)


###############################################################################
# Interface with Tile-Simulation
###############################################################################

#' Null Human: Daily Event Queue Simulation
#'
#' For the null human model, the daily time step does nothing.
#'
oneDay_EventQ_Human_NULL <- function(){}

#' Null Human: Daily Activity Space Simulation
#'
#' Always add 100% of my risk to my home site.
#'
oneDay_ActivitySpace_Human_NULL <- function(){
  # add all my risk to my home site
  MBITES:::Globals$get_tile(private$tileID)$get_sites()$get(private$siteID)$get_feed(private$feedingID)$RiskQ$add2Q(private$id,private$w,1)
}

# set methods
Human_NULL$set(which = "public",name = "oneDay_EventQ",
    value = oneDay_EventQ_Human_NULL, overwrite = TRUE
)

Human_NULL$set(which = "public",name = "oneDay_ActivitySpace",
    value = oneDay_ActivitySpace_Human_NULL, overwrite = TRUE
)


###############################################################################
# Interface with Pathogen-NULL
###############################################################################

#' Null Human: Push Host Probing Event to History
#'
#' If using the null human and pathogen model this function logs host probing events on this \code{\link{Human_NULL}}.
#'  * This method is bound to \code{Human_NULL$pushProbe}
#'
#' @param m_id id of the \code{\link{Mosquito_Female}}
#' @param t time of the probing event
#'
pushProbe_Human_NULL <- function(m_id,t){
  if(private$UNBITTEN){
    private$mosquito_id = m_id
    private$mosquito_t = t
    private$bloodFeed = FALSE
    private$UNBITTEN = FALSE
  } else {
    private$mosquito_id = append(private$mosquito_id,m_id)
    private$mosquito_t = append(private$mosquito_t,t)
    private$bloodFeed = append(private$bloodFeed,FALSE)
  }
}

#' Null Human: Push Host Blood feeding Event to History
#'
#' If using the null human and pathogen model this function logs host blood feeding events on this \code{\link{Human_NULL}}.
#'  * This method is bound to \code{Human_NULL$pushFeed}
pushFeed_Human_NULL <- function(){
  private$bloodFeed[length(private$bloodFeed)] = TRUE
}

# set methods
Human_NULL$set(which = "public",name = "pushProbe",
    value = pushProbe_Human_NULL, overwrite = TRUE
)

Human_NULL$set(which = "public",name = "pushFeed",
    value = pushFeed_Human_NULL, overwrite = TRUE
)
