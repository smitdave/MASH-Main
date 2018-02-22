###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Landscape-Resource-Aquatic Habitat-Aquatic Population (Emerge) pimpl
#     MBITES Team
#     February 2018
#
###############################################################################


#' Landscape Aquatic Population Emerge Model Class
#'
#' Class that implements the 'Emerge' model of aquatic ecology, inheriting the interface of \code{\link[MBITES]{Aqua_Base}}.
#'
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * none
#'
#' @section **Methods**:
#'  * add_egg: function that must take an EggQ (a \code{list} object) and add egg batches to the population; it should return a modified EggQ with those elements corresponding to 'processed' egg batches zeroed out
#'  * one_day: function with void return that runs one day of the specific aquatic population simulation implementation
#'  * get_imago: function that returns an ImagoQ (a \code{list} object) for imagos (adult mosquitoes) ready to emerge on that day
#'
#' @section **Fields**:
#'  * none
#'
#' @export
Aqua_Emerge <- R6::R6Class(classname = "Aqua_Emerge",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,
                 inherit = MBITES:::Aqua_Base,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(lambda){
                     if(length(lambda)<365 & length(lambda)>1){
                       stop(cat("length of provided lambda vector: ",length(lambda),", but require vector either >= 365 days or 1 day (constant emergence)"))
                     }
                     if(length(lambda)==1){
                       private$constant = TRUE
                     }
                   }, # end constructor

                   # add egg batches to aquatic population
                   add_egg = function(EggQ){
                     # if(MBITES:::MBITES_Pars$get_log_egg()){
                     #   # log eggs
                     # }
                   },

                   # one day of aquatic population
                   one_day = function(tNow){
                     if(private$constant){
                       lambda_t = rpois(n = 1, lambda = private$lambda)
                     } else {
                       lambda_now = private$lambda[floor(tNow)%%365+1]
                       lambda_t = rpois(n = 1, lambda = lambda_now)
                     }
                     if(lambda_t>0){

                     }
                   },

                   # get emerging imagos
                   get_imago = function(){

                   }

                 ),

                 # private members
                 private = list(
                   lambda            = numeric(1),
                   constant          = logical(1),
                   ImagoQ            = numeric(1)
                 )
)
