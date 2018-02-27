###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MASH-MICRO
#   M-BITES: Egg Laying
#   MASH-MICRO Team
#   September 2017
#
###############################################################################


#' M-BITES: Choose a Habitat \code{MosquitoFemale}
#'
#' Returns the ID of the local aquatic habitat or a code for an
#' ovitrap.
#'
mbites_chooseHabitat<-function(){
  # To be written
}

#' M-BITES: Encounter an Ovitrap \code{MosquitoFemale}
#'
#' write me!
#'
mbites_ovitrap<-function(){
   # write me!
} 

#' M-BITES: Lay Eggs for 'Emerge' \code{\link{MosquitoFemale}}
#'
#' During an oviposition bout \code{\link{mbites_boutO}}, lay eggs (this is just a filler to clear out the \code{batch} field of the mosquito; egg laying is not implemented in any modules relying on "Emerge" Aquatic Ecology module)
#'  * This method is bound to \code{MosquitoFemale$layEggs()}.
#'
mbites_layEggs_Emerge <- function(){
  if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("O_succeed")){
    private$batch = 0
    private$stateNew = "F"
    private$boutFail = FALSE
  } else {
    private$boutFail = TRUE
  }
}

#' M-BITES: Lay Eggs for 'EL4P' \code{\link{MosquitoFemale}}
#'
#' During an oviposition bout \code{\link{mbites_boutO}}, lay eggs for 'EL4P' module of Aquatic Ecology.
#'  * This method is bound to \code{MosquitoFemale$layEggs()}.
#'
mbites_layEggs_EL4P <- function(){
  if(runif(1) < private$FemalePopPointer$get_MBITES_PAR("O_succeed")){
    if(private$periDomestic){
      private$LandscapePointer$get_FeedingSites(private$locNow)$get_periDomestic()$get_EggQ()$add_EggQ(N_new=private$batch,tOviposit_new=private$tNow,genotype_new=1L)
      private$periDomestic = FALSE
    } else {
      private$LandscapePointer$get_AquaSites(private$locNow)$get_EggQ()$add_EggQ(N_new=private$batch,tOviposit_new=private$tNow,genotype_new=1L)
    }
    private$batch = 0
    private$stateNew = "F"
    private$boutFail = FALSE
  } else {
    private$boutFail = TRUE
  }
}
