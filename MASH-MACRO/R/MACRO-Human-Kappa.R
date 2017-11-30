###############################################################################
#       __ __
#      / //_/___ _____  ____  ____ _
#     / ,< / __ `/ __ \/ __ \/ __ `/
#    / /| / /_/ / /_/ / /_/ / /_/ /
#   /_/ |_\__,_/ .___/ .___/\__,_/
#             /_/   /_/
#
#   MASH-MACRO
#   Kappa
#   MASH Team
#   November 2017
#
###############################################################################

###############################################################################
# bWeightHuman
###############################################################################

updatebWeightHuman_Human <- function(){
  private$TilePointer$get_Patch(private$patchID)$accumulate_bWeightHuman(private$bWeight)
}



###############################################################################
# Kappa
###############################################################################

#' PfSI Update my Kappa
#'
#' Update my personal contribution to \eqn{|Kappa} in the patch where I am currently residing.
#'
#'  * This method is bound to \code{Human$updateKappa}
#'
updateKappa_PfSI_Human <- function(){
  if(private$Pathogens$get_infected()){
    myKappa = private$bWeight * private$Pathogens$get_c()
    private$TilePointer$get_Patch(private$patchID)$accumulate_kappa(kappa = myKappa)
  }
}

#' PfMOI Update my Kappa
#'
#' Update my personal contribution to \eqn{|Kappa} in the patch where I am currently residing.
#'
#'  * This method is bound to \code{Human$updateKappa}
#'
updateKappa_PfMOI_Human <- function(){
  cat("write me\n")
}

#' Update Kappa
#'
#' Update contributions to \eqn{|Kappa} for all humans.
#'
#'  * This method is bound to \code{HumanPop$updateKappa}
#'
updateKappa_HumanPop <- function(){
  private$pop$apply(tag="updateKappa")
}

###############################################################################
# EIR
###############################################################################

#' Update my EIR
#'
#' Based on my current location, update my daily EIR.
#'
#'  * This method is bound to \code{Human$updateEIR}
#'
updateEIR_Human <- function(){
  myEIR = private$bWeight * (private$HumansPointer$get_MosquitoPointer()$get_f() * private$HumansPointer$get_MosquitoPointer()$get_Z(private$patchID))
  myEIR = myEIR / (private$TilePointer$get_Patch(private$patchID)$get_bWeightHuman() + private$TilePointer$get_Patch(private$patchID)$get_bWeightZoo() + private$TilePointer$get_Patch(private$patchID)$get_bWeightZootox()) # renormalize
  private$EIR = myEIR
}

#' Update EIR
#'
#' Update daily EIR for all humans.
#'
#'  * This method is bound to \code{HumanPop$updateEIR}
#'
updateEIR_HumanPop <- function(){
  private$pop$apply(tag="updateEIR")
}

###############################################################################
# Infectious Bites
###############################################################################

#' PfSI Queue my Daily Infectious Bites
#'
#' Based on my personal EIR value, queue infectious PfSI bites by calling \code{\link{add2Q_SimBitePfSI_Human}}
#'
#'   * This method is bound to \code{Human$queueInfectiousBites}
#'
queueInfectiousBites_PfSI_Human <- function(){
  mu = private$EIR
  nBites = rnbinom(n=1,mu=mu,size=0.1) # number of bites today
  if(nBites > 0){
    self$add2Q_SimBitePfSI(tEvent = private$TilePointer$get_tNow())
  }
}

#' PfMOI Queue my Daily Infectious Bites
#'
#' Based on my personal EIR value, queue infectious PfMOI bites by calling \code{\link{add2Q_SimBitePfMOI_Human}}
#'
#'   * This method is bound to \code{Human$queueInfectiousBites}
#'
queueInfectiousBites_PfMOI_Human <- function(){
  cat("write me\n")
}

#' Queue Daily Infectious Bites
#'
#' Based on personal EIR value, queue infectious bites for all humans.
#'
#'   * This method is bound to \code{HumanPop$queueInfectiousBites}
#'
queueInfectiousBites_HumanPop <- function(){
  self$updateEIR() # update EIR
  private$pop$apply(tag="queueInfectiousBites")
}

# Getters & Setters

#' Get \code{Human} EIR
#'
#' Return my personal EIR value.
#'
#'  * This method is bound to \code{Human$get_EIR}
#'
get_EIR_Human <- function(){
  return(private$EIR)
}

#' Set \code{Human} EIR
#'
#' Set my personal EIR value.
#'
#'  * This method is bound to \code{Human$set_EIR}
#'
#' @param EIR numeric value
#'
set_EIR_Human <- function(EIR){
  private$EIR = EIR
}
