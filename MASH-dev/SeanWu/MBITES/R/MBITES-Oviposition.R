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
#' write about me!
#'
#'
#'
#' @name Oviposition
NULL
#> NULL


###############################################################################
# Emerge
###############################################################################

#' M-BITES: Lay Eggs for 'Emerge' \code{\link{MosquitoFemale}}
#'
#' During an oviposition bout \code{\link{mbites_boutO}}, lay eggs (this is just a filler to clear out the \code{batch} field of the mosquito; egg laying is not implemented in any modules relying on "Emerge" Aquatic Ecology module)
#'  * This method is bound to \code{MosquitoFemale$layEggs()}.
#'
mbites_layEggs_Emerge <- function(){
  if(runif(1) < MBITES:::Parameters$get_O_succeed()){
    private$batch = 0
    private$stateNew = "F"
    private$boutFail = 0L
  } else {
    private$boutFail = private$boutFail + 1L
  }
}


###############################################################################
# EL4P
###############################################################################

#' M-BITES: Lay Eggs for 'EL4P' \code{\link{MosquitoFemale}}
#'
#' During an oviposition bout \code{\link{mbites_boutO}}, lay eggs for 'EL4P' module of Aquatic Ecology.
#'  * This method is bound to \code{MosquitoFemale$layEggs()}.
#'
mbites_layEggs_EL4P <- function(){
  if(runif(1) < MBITES:::Parameters$get_O_succeed()){
    private$aqua_res$EggQ$add2Q(private$batch,private$tNow)
    private$batch = 0
    private$stateNew = "F"
    private$boutFail = 0L
  } else {
    private$boutFail = private$boutFail + 1L
  }
}
