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
  } # end for
}


###################################################################
# Add PfMOI Pathogen Object to 'Human' & 'HumanPop' Class
###################################################################

#' PfMOI \code{Human} Method: Set Human-stage PfMOI Object
#'
#' Set the \code{\link[MASHcpp]{humanPfMOI}} object in a human.
#' This method is bound to \code{Human$set_humanPfMOI()}
#'
set_humanPfMOI_Human <- function(b=0.55,c=0.15,chemoprophylaxis=FALSE){
  private$Pathogens = MASHcpp::humanPfMOI(b=b,c=c,chemoprophylaxis=chemoprophylaxis)
}

#' PfMOI \code{HumanPop} Method: Set Human-stage PfMOI Object
#'
#' Set the \code{\link[MASHcpp]{humanPfMOI}} object in a human poulation.
#' This method is bound to \code{HumanPop$set_humanPfMOI()}
#'
#' @param b infected mosquito to human transmission efficiency
#' @param c infected human to mosquito transmission efficiency
set_humanPfMOI_HumanPop <- function(){
  private$pop$apply(tag="set_humanPfMOI",returnVal=FALSE,b=private$PfMOI_PAR$b,=c=private$PfMOI_PAR$c)
}


#################################################################
# PfMOI Event Timing
#################################################################

#' PfMOI \code{Human} Method: Duration of Infection
#'
#' How many days does a single PfMOI infection last? See \code{\link{PfMOI.Parameters}} parameter \code{DurationPf} for baseline clearance rate.
#' This function  calculates clearance rate under the assumption of independent clearance (M/M/1 queueing process for infection).
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttClearPf()}
#'
PfMOI_ttClearPf_Independent <- function(){
  return(rexp(n=1, rate=1/self$get_PfMOI_PAR("DurationPf")))
}

#' PfMOI \code{Human} Method: Duration of Infection with Pathogen Interaction
#'
#' How many days does a single PfMOI infection last? See \code{\link{PfMOI.Parameters}} parameter \code{DurationPf} for baseline clearance rate.
#' This function calculates clearance rate as a function of individual MOI modeling pathogen competition for host resources (faster clearance than expected under independent clearance)
#' or pathogen facilitation (slower clearance than expected under independent clearance).
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttClearPf()}
#'
#'  * See \url{https://doi.org/10.1186/1475-2875-8-87} for details of queueing model with pathogen interaction.
PfMOI_ttClearPf_Interaction <- function(){
  clearanceRate = (1/self$get_PfMOI_PAR("DurationPf")) ^ (private$Pathogens$get_MOI() * self$get_PfMOI_PAR("InteractionPf"))
  return(rexp(n=1, rate=clearanceRate))
}

#' PfMOI \code{Human} Method: Duration of Latency
#'
#' How many days after the infectious bite does a PfMOI infection start? See \code{\link{PfMOI.Parameters}} parameter \code{LatentPf} constant latent period.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttInfectPf()}
#'
PfMOI_ttInfectionPf <- function(){
  return(self$get_PfMOI_PAR("LatentPf"))
}

#' PfMOI \code{Human} Method: Timing of Fever Incident
#'
#' What is the timing of fever relative to the start of a PfMOI infection? See \code{\link{PfMOI.Parameters}} parameter \code{mnFeverPf} mean (on natural scale) and parameter \code{vrFeverPf} standard deviation (on natural scale) of log-normally distributed time to fever.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttFeverPf()}
#'
PfMOI_ttFeverPf <- function(){
  rlnorm(1,log(self$get_PfMOI_PAR("mnFeverPf")),self$get_PfMOI_PAR("vrFeverPf"))
}

#' PfMOI \code{Human} Method: Timing of Treatment
#'
#' What is the timing of treatment relative to the start of a fever incident? See \code{\link{PfMOI.Parameters}} parameter \code{mnTreatPf} for mean waiting time of exponentially distributed waiting period.
#' This method is called from \code{\link{}}
#' This method is bound to \code{Human$ttTreatPf()}
#'
PfMOI_ttTreatPf <- function(){
  rexp(1, 1/self$get_PfMOI_PAR("mnTreatPf"))
}

#' PfMOI \code{Human} Method: Duration of Protection from Chemoprophylaxis
#'
#' After administration of Chemoprophylaxis what is time to susceptibility? See \code{\link{PfMOI.Parameters}} parameter \code{mnChemoprophylaxisPf} constant timing period.
#' This method is called from \code{\link{event_endprophylaxisPfMOI}}
#' This method is bound to \code{Human$ttSusceptiblePf()}
#'
PfMOI_ttSusceptiblePf <- function(){
  return(self$get_PfMOI_PAR("mnChemoprophylaxisPf"))
}

#' PfMOI \code{Human} Method: Duration of protection by PE Vaccination
#'
#' After administration of PE Vaccination what is duration of protection (reduced infected mosquito to human transmission efficiency)? See \code{\link{PfMOI.Parameters}} parameter \code{mnPEPf} and \code{vrPEPf} for normally distributed duration of protection.
#' This method is called from \code{\link{event_pewanePfMOI}}
#' This method is bound to \code{Human$ttPEWanePf()}
#'
PfMOI_ttPEWanePf <- function(){
  return(rnorm(n = 1, mean = self$get_PfMOI_PAR("mnPEPf"), sd = self$get_PfMOI_PAR("vrPEPf")))
}

#' PfMOI \code{Human} Method: Duration of protection by GS Vaccination
#'
#' Duration of protection by GS Vaccination? See \code{\link{PfMOI.Parameters}} parameter \code{mnGSPf} and \code{vrGSPf} for normally distributed duration of protection.
#' This method is called from \code{\link{event_gswanePfMOI}}
#' This method is bound to \code{Human$ttGSWanePf()}
#'
PfMOI_ttGSWanePf <- function(){
  return(rnorm(n = 1, mean = self$get_PfMOI_PAR("mnGSPf"), sd = self$get_PfMOI_PAR("vrGSPf")))
}


###################################################################
# Add PfMOI Events to 'Human' Class
# 'XX' family of functions for human event queues
###################################################################

###################################################################
# PfMOI: Mosquito to Human infectious bite
# Add methods to 'Human' Class
###################################################################

#' PfMOI \code{Human} Method: Host Probing
#'
#' This method is called by a mosquito when she probes a human host, but may also be called by \code{\link{SimBitePfMOI}} as a filler.
#' If the biting mosquito is infectious, the method calls \code{\link{infectiousBite_PfMOI}}, otherwise does nothing.
#'  * This method is bound to \code{Human$probeHost_PfMOI()}
#'
#' @param tBite time of bite
#' @param mosquitoPfSI \code{\link[MASHcpp]{mosquitoPfMOI}} object passed from mosquito to human
#'
probeHost_PfMOI <- function(tBite, mosquitoPfMOI){

  infections = mosquitoPfMOI$get_PfID_EIP(tBite)

}
