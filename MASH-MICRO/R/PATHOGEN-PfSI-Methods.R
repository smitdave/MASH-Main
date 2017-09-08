###############################################################################
#       ____  _________ ____
#      / __ \/ __/ ___//  _/
#     / /_/ / /_ \__ \ / /
#    / ____/ __/___/ // /
#   /_/   /_/  /____/___/
#
#   MASH-MICRO
#   MICRO: PfSI Methods
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################


###############################################################################
# MosquitoFemale PfSI Methods
###############################################################################

#' PfSI Helper Code for Pathogen Initialization in \code{\link{MosquitoFemale}}
#'
#' Initializes an empty PfSI pathogen in \code{\link{MosquitoFemale}} called during object initialization.
#'  * This method is bound to \code{MosquitoFemale$init_Pathogens()}
#'
#' @md
#' @export
init_Pathogens_MosquitoFemale_PfSI <- function(){
  private$Pathogens = MASHcpp::mosquitoPfSI(PfID_init = -1L, infected_init = FALSE)
}

#' PfSI Helper Code for Pathogen Initialization in \code{\link{MosquitoPopFemale}}
#'
#' Initializes empty PfSI pathogens in \code{\link{MosquitoPopFemale}} called during object initialization.
#'  * This method is bound to \code{MosquitoPopFemale$init_Pathogens()}
#'
#' @md
#' @export
init_Pathogens_MosquitoPopFemale_PfSI <- function(){
  private$pop$apply(tag="init_Pathogens",returnVal=FALSE)
}

#' PfSI \code{\link{MosquitoFemale}} Method: Host Probing
#'
#' The mosquito probes the host prior to successful feeding. Probing occurs during code\{MosquitoFemale$humanEncounter()}.
#' Mosquito to human pathogen transmission occurs during host probing.
#' If the PfSI infection has passed the EIP, \code{\link{probeHost_PfSI}} is called via the mosquito's pointer to the \code{\link{HumanPop}} class
#' to initiate the PfSI infection process.
#'  * This method is bound to \code{MosquitoFemale$probing()}
#'
#' @md
probing_PfSI <- function(){
  # if mosquito has active infection
  if(private$Pathogens$get_tInf() > -1){
    # update the moquito infection
    if(private$tNow > (private$Pathogens$get_tInf() + private$FemalePopPointer$get_MBITES_PAR("PfEIP"))){
      private$Pathogens$set_infected(TRUE)
      private$HumansPointer$get_Human(ixH = private$hostID)$probeHost_PfSI(tBite = private$tNow, mosquitoPfSI = private$Pathogens)
    }
  }
}

#' PfSI \code{\link{MosquitoFemale}} Method: Host Feeding
#'
#' The mosquito feeds on the host. Feeding occurs during code\{MosquitoFemale$humanEncounter()}.
#' Human to mosquito pathogen transmission occurs during host feeding.
#'  * This method is bound to \code{MosquitoFemale$feeding()}
#'
#' @md
feeding_PfSI <- function(){
  # if human is infected and mosquito not yet infected
  if(private$HumansPointer$get_Human(ixH = private$hostID)$get_Pathogens()$get_infected() & !private$Pathogens$get_infected()){
    if(runif(1) < private$HumansPointer$get_Human(ixH = private$hostID)$get_Pathogens()$get_c()){
      # if human to mosquito transmission successful set the PfSI object in the mosquito accordingly
      PfID = private$HumansPointer$get_Human(ixH = private$hostID)$get_Pathogens()$back_PfID()
      private$Pathogens$set_tInf(private$tNow)
      private$Pathogens$set_PfID(PfID)
    }
  }
}
