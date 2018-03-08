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
#'  * home: id of the \code{\link[MBITES]{Site}} where my biting weight will be added
#'
#' @section **Methods**:
#'  * method: i'm a method!
#'
#' @section **Fields**:
#'  * id: integer id
#'  * w: numeric biting weight
#'  * mosquito_id: integer vector of mosquitoes that have bitten me
#'  * mosquito_t: numeric vector of times i was bitten
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
                   initialize = function(id,w,home){
                     futile.logger::flog.trace("Human_NULL %i being born at self: %s , private: %s",id,pryr::address(self),pryr::address(private))

                     # basic parameters
                     private$id = id
                     private$w = w

                     # add my risk to my home site
                     MBITES:::Globals$get_tile()$get_site(home)$get_feed(1L)$RiskQ$add2Q(id,w,1)
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

                   # biting dynamics
                   UNBITTEN            = TRUE, # have i been bitten yet?
                   mosquito_id         = integer(1), # vector of mosquitoes that have bitten me
                   mosquito_t          = numeric(1) # vector of times i was bitten

                 )
) # end Human_NULL class definition

# pushes information from a bite into the human's history counter
pushBite_Human_NULL <- function(m_id,t){
  if(private$UNBITTEN){
    private$mosquito_id = m_id
    private$mosquito_t = t
    private$UNBITTEN = FALSE
  } else {
    private$mosquito_id = append(private$mosquito_id,m_id)
    private$mosquito_t = append(private$mosquito_t,t)
  }
}

Human_NULL$set(which = "public",name = "pushBite",
    value = pushBite_Human_NULL, overwrite = TRUE
)


# normally, the functions below would go in the PATHOGEN-XX-XX.R file

# this function fills in for pathogen specific probeHost methods if no pathogen model is used
probeHost_NULL <- function(){
  MBITES:::Globals$get_tile()$get_Humans()$get_Human(private$hostID)$pushBite(m_id=private$id,t=private$t_now)
}

# this function fills in for pathogen specific bloodFeed methods if no pathogen model is used
bloodFeed_NULL <- function(){

}
