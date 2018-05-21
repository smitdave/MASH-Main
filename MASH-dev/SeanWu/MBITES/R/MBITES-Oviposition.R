###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MBITES-Oviposition
#   MBITES Team
#   March 2018
#
###############################################################################

#' MBITES: Oviposition
#'
#' During the \code{\link{mbites_updateState}} if the mosquito is gravid and passes refeeding check
#' (see \code{\link{mbites_checkRefeed}}) the mosquito will enter an oviposition bout (\code{\link{mbites_boutO}}) on the next launch.
#' During oviposition the mosquito first chooses a aquatic habitat at the site and then lays eggs.
#'
#'
#' @name MBITES-Oviposition
NULL
#> NULL


###############################################################################
# Sample Habitat
###############################################################################

#' MBITES: Choose an Aquatic Habitat
#'
#' Choose a \code{\link{Aqua_Resource}} for oviposition; if ovitraps, etc exist at this \code{\link{Site}}, those
#' pseudo-resources compete for the mosquito's attention.
#'  * this method is bound to \code{Mosquito_Female$chooseHabitat}
#'
mbites_chooseHabitat <- function(){
  # when ovitraps, etc exist, do the checks here
  private$aqua_resource = private$site$sample_aqua() # sample resources
  private$habitatID = private$aqua_resource$get_habitatID()
}

# set methods
Mosquito_Female$set(which = "public",name = "chooseHabitat",
    value = mbites_chooseHabitat, overwrite = TRUE
)


###############################################################################
# Emerge (set in MBITES_Setup)
###############################################################################

#' M-BITES: Lay Eggs for 'Emerge' \code{\link{MosquitoFemale}}
#'
#' During an oviposition bout \code{\link{mbites_boutO}}, lay eggs (this is just a filler to clear out the \code{batch} field of the mosquito; egg laying is not implemented in any modules relying on "Emerge" Aquatic Ecology module)
#'  * This method is bound to \code{MosquitoFemale$layEggs()}.
#'
mbites_layEggs_Emerge <- function(){
  if(runif(1) < MBITES:::Parameters$get_O_succeed()){
    private$eggT = 2e16
    private$gravid = FALSE
    private$batch = 0
    private$state = "B"
  }
}


###############################################################################
# EL4P (set in MBITES_Setup)
###############################################################################

#' M-BITES: Lay Eggs for 'EL4P' \code{\link{MosquitoFemale}}
#'
#' During an oviposition bout \code{\link{mbites_boutO}}, lay eggs for 'EL4P' module of Aquatic Ecology.
#'  * This method is bound to \code{MosquitoFemale$layEggs()}.
#'
mbites_layEggs_EL4P <- function(){
  if(runif(1) < MBITES:::Parameters$get_O_succeed()){
    private$eggT = 2e16
    private$gravid = FALSE
    private$aqua_resource$EggQ$add2Q(private$batch,private$tNow)
    private$batch = 0
    private$state = "B"
  }
}
