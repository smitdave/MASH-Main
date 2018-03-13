###############################################################################
#         __                    __
#        / /   ____ _____  ____/ /_____________ _____  ___
#       / /   / __ `/ __ \/ __  / ___/ ___/ __ `/ __ \/ _ \
#      / /___/ /_/ / / / / /_/ (__  ) /__/ /_/ / /_/ /  __/
#     /_____/\__,_/_/ /_/\__,_/____/\___/\__,_/ .___/\___/
#                                            /_/
#     Landscape-Resource-Aquatic Habitat-Aquatic Population (Emerge) pimpl
#     MBITES Team
#     February 2018
#
###############################################################################


#' Aquatic Ecology: Emerge Model Class
#'
#' Class that implements the 'Emerge' model of aquatic ecology, inheriting the interface of \code{\link[MBITES]{Aqua_Resource}}.
#'
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * lambda: either numeric or vector of at least length 365
#'  * site: a reference to a \code{\link[MBITES]{Site}} object
#'
#' @section **Methods**:
#'  * add_egg: function that must take an EggQ (a \code{list} object) and add egg batches to the population; it should return a modified EggQ with those elements corresponding to 'processed' egg batches zeroed out
#'  * one_day: function with void return that runs one day of the specific aquatic population simulation implementation
#'  * get_imago: function that returns an ImagoQ (a \code{list} object) for imagos (adult mosquitoes) ready to emerge on that day
#'
#' @section **Fields**:
#'  * lambda: numeric vector
#'  * constant: logical
#'
#' @export
Aqua_Resource_Emerge <- R6::R6Class(classname = "Aqua_Resource_Emerge",
                 portable = TRUE,
                 cloneable = FALSE,
                 lock_class = FALSE,
                 lock_objects = FALSE,
                 inherit = MBITES:::Aqua_Resource,

                 # public members
                 public = list(

                   # begin constructor
                   initialize = function(w, site, lambda){
                     futile.logger::flog.trace("Aqua_Resource_Emerge being born at: self %s , private %s",pryr::address(self),pryr::address(private))

                     if(length(lambda)<365 & length(lambda)>1){
                       stop(cat("length of provided lambda vector: ",length(lambda),", but require vector either >= 365 days or 1 day (constant emergence)"))
                     }

                     super$initialize(w,site) # construct base-class parts

                     if(length(lambda)==1){
                       private$constant = TRUE
                       private$lambda = lambda
                     } else {
                       private$constant = FALSE
                       private$lambda = lambda
                     }

                   }, # end constructor

                   # begin destructor
                   finalize = function(){
                     futile.logger::flog.trace("Aqua_Resource_Emerge being killed at: self %s , private %s",pryr::address(self),pryr::address(private))
                   } # end destructor

                 ), # end public

                 # private members
                 private = list(
                   lambda            = numeric(1),
                   constant          = logical(1) # boolean indicating constant emergence or lambda vector
                 ) # end private
) # end Aqua_Resource_Emerge


###############################################################################
# add egg batches to aquatic population
###############################################################################

#' Aquatic Ecology: Emerge - Add Eggs
#'
#' Add an egg batch from a successful oviposition attempt to this \code{\link{Aqua_Resource_Emerge}}.
#'
#'  * This method is bound to \code{Aqua_Resource_Emerge$add_egg()}.
#'
#' @param EggQ egg batch
#'
add_egg_Emerge <- function(EggQ){
  # if(MBITES:::MBITES_Pars$get_log_egg()){
  #   # log eggs
  # }
}

Aqua_Resource_Emerge$set(which = "public",name = "add_egg",
          value = add_egg_Emerge, overwrite = TRUE
)


###############################################################################
# one day of aquatic population dynamics
###############################################################################

#' Aquatic Ecology: Emerge - Daily Emergence
#'
#' Add a cohort of emerging imagos based on current value of \eqn{\lambda} by calling \code{add2Q} function of the imago closure object, see \code{\link[MBITES]{make_ImagoQ}}.
#'
#'  * This method is bound to \code{Aqua_Resource_Emerge$one_day()}.
#'
#'
one_day_Emerge <- function(){

  tNow = MBITES:::Globals$get_tNow()

  # sample number of emerging mosquitoes
  if(private$constant){
    lambda_t = rpois(n = 1, lambda = private$lambda)
  } else {
    lambda_now = private$lambda[floor(tNow)%%365+1]
    lambda_t = rpois(n = 1, lambda = lambda_now)
  }

  # if emerging mosquitoes, send them to the population
  if(lambda_t>0){
    self$ImagoQ$add2Q(N=lambda_t,time_e=tNow)
  }

} # end one_day

Aqua_Resource_Emerge$set(which = "public",name = "one_day",
          value = one_day_Emerge, overwrite = TRUE
)


###############################################################################
# get emerging imagos
###############################################################################

push_imago_Emerge <- function(){

  tNow = MBITES:::Globals$get_tNow() # time now
  tile_id = private$SiteP$get_tileID() # integer id of my tile
  tile = MBITES:::Globals$get_tile(tile_id) # tile reference

  imagos = self$ImagoQ$popQ(time_e=tNow) # emerging imagos

  # if there is are cohort(s) of imagos ready to go
  if(!is.na(imagos$female[1])){

    # iterate over those cohorts
    for(i in 1:length(imagos$imagos)){
      # female cohort
      if(imagos$female[i]){
        for(j in 1:imagos$imagos[i]){
          mosy = Mosquito_Female$new(tNow,private$SiteP,tile_id)
          tile$get_mosquitoes()$assign(key=mosy$get_id(),value=mosy)
        }
      # male cohort
      } else {
        mosy = Mosquito_Male$new(tNow,private$SiteP,tile_id)
        tile$get_mosquitoes()$assign(key=mosy$get_id(),value=mosy)
      }
    } # end loop over cohorts i

  } # end conditional
} # end get_imago


Aqua_Resource_Emerge$set(which = "public",name = "push_imago",
          value = push_imago_Emerge, overwrite = TRUE
)
