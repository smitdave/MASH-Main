###############################################################################
#       ____  ______  _______  ____
#      / __ \/ __/  |/  / __ \/  _/
#     / /_/ / /_/ /|_/ / / / // /
#    / ____/ __/ /  / / /_/ // /
#   /_/   /_/ /_/  /_/\____/___/
#
#   MASH
#   PfMOI Methods
#   MASH Team
#   December 2017
#
###############################################################################


###################################################################
# PfMOI Output
###################################################################

#' PfMOI \code{HumanPop} Method: Initialize PfMOI Output
#'
#' Initialize output for PfMOI model.
#'  * This method is bound to \code{HumanPop$initialize_output_Pathogen()}
#'
initialize_output_PfMOI_HumanPop <- function(){
  writeLines(text = paste0(c("humanID","time","event","MOI","vectorID"),collapse = ","),con = private$conPathogen, sep = "\n")
}


###################################################################
# PfMOI Parameter Getters & Setters
###################################################################

#' PfMOI \code{Human} Method: Get PfMOI
#'
#' Get named element of PfMOI_PAR. See \code{\link{PfMOI.Parameters}} for definitions.
#'  * This method is bound to \code{Human$get_PfMOI_PAR()}
#'
Human_get_PfMOI_PAR <- function(ix){
  return(private$HumansPointer$get_PfMOI_PAR(ix))
}

#' PfMOI \code{HumanPop} Method: Get PfMOI_PAR
#'
#' Get named element of PfMOI_PAR. See \code{\link{PfMOI.Parameters}} for definitions.
#'  * This method is bound to \code{HumanPop$get_PfMOI_PAR()}
#'
#' @param ix name of parameter to return \code{\link{PfMOI.Parameters}}
#'
HumanPop_get_PfMOI_PAR <- function(ix){
  return(private$PfMOI_PAR[[ix]])
}

#' PfMOI \code{HumanPop} Method: Set PfMOI Paramters for a \code{\link{HumanPop}}
#'
#' Set PfMOI_PAR for a HumanPop; this is useful for simulating multiple populations with different parameter values.
#'  * This method is bound to \code{HumanPop$set_PfMOI_PAR()}
#'
#' @param PfMOI_PAR new parameter list, see \code{\link{PfMOI.Parameters}}
#'
HumanPop_set_PfMOI_PAR <- function(PfMOI_PAR){
  private$PfMOI_PAR = PfMOI_PAR
}


###################################################################
# Add PfMOI Utilities to 'HumanPop' Class
###################################################################

#' PfMOI \code{HumanPop} Method: Increment PfID
#'
#' Whenever a new liver-stage infection manifests in a human we increment the PfID counter and return the new PfID.
#' This method is bound to \code{HumanPop$increment_PfID()}
#'
PfMOI_increment_PfID <- function(){
  private$PfID = private$PfID + 1L
  return(private$PfID)
}

#' PfMOI \code{HumanPop} Method: Initialize PfMOI Infections
#'
#' Initialize PfMOI infections with given MOI for each human.
#'  * This method is bound to \code{HumanPop$initialize_Pathogens()}
#'
#' @param PfPAR integer vector of starting MOI for each human
#'
initialize_Pathogens_PfMOI_HumanPop <- function(PfPAR){

  private$PfID = 1L
  self$set_humanPfMOI()

  # private$pop$apply(tag = "initialize_PfMOI",returnVal=FALSE,PfPR=PfPAR)
  for(i in 1:private$N){
    if(PfPAR[i]<1){
      writeLines(text = paste0(c(private$myID,0.0,"S",0,"NULL"),collapse = ","),con = private$conPathogen, sep = "\n")
    } else {
      for(j in 1:PfPAR[i]){
        private$pop$get(as.character(i))$infectHumanPfMOI(tEvent = 0, PAR = list(vectorID="initInf"))
      }
    }
  }
}
