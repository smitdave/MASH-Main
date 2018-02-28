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
                   initialize = function(id,w){
                     private$id = id
                     private$w = w
                   }, # end constructor

                   # begin destructor
                   finalize = function(){} # end destructor

                 ),

                 # private members
                 private = list(

                   # local fields
                   id                  = integer(1), # my id
                   w                   = numeric(1), # my biting weight

                   # biting dynamics
                   mosquito_id         = integer(1), # vector of mosquitoes that have bitten me
                   mosquito_t          = numeric(1) # vector of times i was bitten

                 )
) # end Human_NULL class definition
