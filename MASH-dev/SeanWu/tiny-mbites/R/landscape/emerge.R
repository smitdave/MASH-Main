###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     emergence module
#     Sean Wu
#     June 2019
#
###############################################################################


oneDay_AquaticEcology <- function(site){
  if(site$has_l){
    one_day_Emerge(site)
    push_imago_Emerge(site)
  }
}


###############################################################################
# one day of aquatic population dynamics
###############################################################################

#' Aquatic Ecology: Emerge - Daily Emergence
#'
#' Add a cohort of emerging imagos based on current value of \eqn{\lambda} by calling \code{add2Q} function of the imago closure object, see \code{\link{make_ImagoQ}}.
#'
#'  * This method is bound to \code{Aqua_Resource_Emerge$one_day}
#'
one_day_Emerge <- function(site){

  tnow <- get("globals")$get_tnow()

  # sample number of emerging mosquitoes
  if(site$constant){
    lambda_t = rpois(n = 1, lambda = site$lambda)
  } else {
    lambda_now = site$lambda[floor(tNow)%%365+1]
    lambda_t = rpois(n = 1, lambda = lambda_now)
  }

  # if emerging mosquitoes, send them to the population
  if(lambda_t>0){
    site$ImagoQ$add2Q(lambda_t,tnow,TRUE)
  }

} # end one_day


###############################################################################
# get emerging imagos
###############################################################################

# this is called from oneDay_AquaticEcology
# site: a single site on the landscape
# mpop: the mosy pop (as an environment)
push_imago_Emerge <- function(site,mpop){

  tnow <- get("globals")$get_tnow()

  imagos = self$ImagoQ$popQ(time_e=tNow) # emerging imagos

  # if there is are cohort(s) of imagos ready to go
  if(!is.na(imagos$female[1]) & (imagos$imagos>0)){

    # iterate over those cohorts (i)
    for(i in 1:length(imagos$imagos)){
      # cohort (i) is female
      if(imagos$female[i]){
        for(j in 1:imagos$imagos[i]){
          mosy <- make_mosquito(bday=tnow,site=site$id,state="F",search=TRUE)
          assign(x = as.character(mosy$id),value = mosy,envir = mpop)
        }
      # cohort (i) is male
      } else {
        stop("error: no male mosquitos in release version of MBITES")
        # for(j in 1:imagos$imagos[i]){
        #   mosy = Mosquito_Male$new(tNow,private$SiteP,tile_id)
        #   tile$get_mosquitoes()$assign(key=mosy$get_id(),value=mosy)
        # }
      }
    } # end loop over cohorts i

  } # end conditional
} # end get_imago
