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
#' Output has the following column structure: humanID, time, event, MOI, vectorID
#'  * This method is bound to \code{HumanPop$initialize_Pathogens()}
#'
#' @param PfPAR integer vector of starting MOI for each human
#'
initialize_Pathogens_PfMOI_HumanPop <- function(PfPAR){

  private$PfID = 1L
  self$set_humanPfMOI()

  for(i in 1:private$N){
    if(PfPAR[i]<1){
      # humanID, tEvent, State, MOI
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
  private$pop$apply(tag="set_humanPfMOI",returnVal=FALSE,b=private$PfMOI_PAR$b,c=private$PfMOI_PAR$c)
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

  infections = mosquitoPfMOI$get_infections(tBite)
  mosyID = mosquitoPfMOI$get_MosquitoID()
  N = self$get_PfMOI_PAR("MosyMaxI")

  for(i in 1:N){
    self$infectiousBite_PfMOI(tBite, PAR = list(PfID = infections[i], vectorID = mosyID))
  }
}

#' PfMOI \code{Human} Method: Infectious Bite on Human
#'
#' This method is called from \code{\link{probeHost_PfMOI}}.
#' If the infectious bite results in successful transmission, this function queues a human infection event, see \code{\link{add2Q_infectHumanPfMOI}}
#' This method is bound to \code{Human$infectiousBite_PfMOI()}
#'
#' @param tBite time of bite
#' @param PAR must be a list containing character \code{PfID} and \code{vectorID}
infectiousBite_PfMOI <- function(tBite, PAR){
  if(runif(1) < private$Pathogens$get_b()){
    tInfStart = tBite + self$ttInfectionPf()
    self$add2Q_infectHumanPfMOI(tEvent = tInfStart, PAR = PAR)
  }
}


###################################################################
# Start a PfMOI Infection
###################################################################

#' PfMOI \code{Human} Event: Add PfMOI Infection Event to Event Queue
#'
#' Add a PfMOI infection to the event queue.
#' This method is called from \code{\link{infectiousBite_PfMOI}}
#' This method adds event \code{\link{event_infectHumanPfMOI}} to the event queue.
#'  * This method is bound to \code{Human$add2Q_infectHumanPfMOI()}
#'
#' @param tEvent time of infection
#' @param PAR single clonal variant returned from \code{mosquitoPfMOI$get_InfectionIx()}
add2Q_infectHumanPfMOI <- function(tEvent, PAR = NULL){
  private$EventQueue$addEvent2Q(event = self$event_infectHumanPfMOI(tEvent = tEvent, PAR = PAR))
}

#' PfMOI \code{Human} Event: Generate PfMOI Infection Event
#'
#' Generate PfMOI infection event to place in event queue.
#' This method is called from \code{\link{add2Q_infectHumanPfMOI}}
#' This method is bound to \code{Human$event_infectHumanPfMOI()}
#'  * tag: \code{\link{infectHumanPfMOI}}
#' @md
#' @param tEvent time of infection
#' @param PAR single clonal variant returned from \code{mosquitoPfMOI$get_clone()}
event_infectHumanPfMOI <- function(tEvent, PAR){
  list(tEvent = tEvent, PAR = PAR, tag = "infectHumanPfMOI")
}

#' PfMOI \code{Human} Event: PfMOI Infection Event
#'
#' Simulate a PfMOI infection. If the human is not under chemoprophylaxis, begin queuing events for this clonal variant's infection process.
#' Specifically, a new PfID is drawn and a new infection is begun by calling \code{humanPfMOI$add_Infection}
#' A bernoulli event is drawn to determine if this infection produces fever; if so \code{\link{add2Q_feverPfMOI}} is called.
#' The end of this PfMOI infection is queued by \code{\link{add2Q_endPfMOI}}
#'
#'  * This method is bound to \code{Human$infectHumanPfMOI()}
#'
#' @param tEvent time of infection
#' @param PAR must be a list containing character \code{PfID} and \code{vectorID}
infectHumanPfMOI <- function(tEvent, PAR){
  if(!private$Pathogens$get_chemoprophylaxis()){
    PfID = private$HumansPointer$increment_PfID()
    private$Pathogens$add_Infection(PfID,tEvent)
    # track event
    MOI = private$Pathogens$get_MOI()
    writeLines(text = paste0(c(private$myID,tEvent,"I",MOI,PAR$vectorID),collapse = ","),con = private$HumansPointer$get_conPathogen(), sep = "\n")
    # queue fever
    if(runif(1) < self$get_PfMOI_PAR("FeverPf")){
      self$add2Q_feverPfMOI(tEvent = tEvent)
    }
    self$add2Q_endPfMOI(tEvent = tEvent, PAR = list(PfID = PfID))
  }
}


###################################################################
# End a PfMOI Infection
###################################################################

#' PfMOI \code{Human} Event: Add PfMOI Clearance Event to Event Queue
#'
#' Add PfMOI infection clearance event to the event queue.
#' This method is called from \code{\link{infectHumanPfMOI}}
#' This method adds event \code{\link{event_endPfMOI}} to the event queue.
#' This method is bound to \code{Human$add2Q_endPfMOI()}
#'  * tEvent: treatment time is calculated as tTreat = tEvent + \code{\link{PfMOI_ttClearPf}}
#'
#' @param tEvent time of infection
#' @param PAR named list
#'  * PfID: PfID of the infection to end
#'
add2Q_endPfMOI <- function(tEvent, PAR = NULL){
  tEnd = tEvent + self$ttClearPf()
  private$EventQueue$addEvent2Q(event = self$event_endPfMOI(tEvent = tEnd, PAR = PAR))
}

#' PfMOI \code{Human} Event: Generate PfMOI Clearance Event
#'
#' Generate PfMOI clearance event to place in event queue.
#' This method is called from \code{\link{add2Q_endPfMOI}}
#' This method is bound to \code{Human$event_endPfMOI()}
#'  * tag: \code{\link{endPfMOI}}
#'
#' @param tEvent time of clearance
#' @param PAR named list
#'  * PfID: PfID of the infection to end
#'
event_endPfMOI = function(tEvent, PAR = NULL){
  list(tEvent = tEvent, PAR = PAR, tag = "endPfMOI")
}

#' PfMOI \code{Human} Event: PfMOI Infection Event
#'
#' Clear a PfMOI infection corresponding to the given PfID.
#' This method is bound to \code{Human$endPfMOI()}
#' @param tEvent time of clearance
#' @param PAR named list
#'  * PfID: PfID of the infection to end
#'
endPfMOI <- function(tEvent, PAR){
  private$Pathogens$clear_Infection(PAR$PfID)
  MOI = private$Pathogens$get_MOI()
  writeLines(text = paste0(c(private$myID,tEvent,"C",MOI,"NULL"),collapse = ","),con = private$HumansPointer$get_conPathogen(), sep = "\n")
}


###################################################################
# Fever
###################################################################

#' PfMOI \code{Human} Event: Add PfMOI Fever Event to Event Queue
#'
#' Add PfMOI fever event to the event queue.
#' This method is called from \code{\link{infectHumanPfMOI}}
#' This method adds event \code{\link{event_feverPfMOI}} to the event queue.
#' This method is bound to \code{Human$add2Q_feverPfMOI()}
#'  * tEvent: treatment time is calculated as tTreat = tEvent + \code{\link{PfMOI_ttFeverPf}}
#'
#' @param tEvent time of fever
#' @param PAR \code{NULL}
#' @md
add2Q_feverPfMOI <- function(tEvent, PAR = NULL){
  tFever = tEvent + self$ttFeverPf()
  private$EventQueue$addEvent2Q(event = self$event_feverPfMOI(tEvent = tFever, PAR = PAR))
}

#' PfMOI \code{Human} Event: Generate PfMOI Fever Event
#'
#' Generate PfMOI fever event to place in event queue.
#' This method is called from \code{\link{add2Q_feverPfMOI}}
#' This method is bound to \code{Human$event_feverPfMOI()}
#'  * tag: \code{\link{feverPfMOI}}
#'
#' @param tEvent time of fever
#' @param PAR \code{NULL}
#'
event_feverPfMOI <- function(tEvent, PAR = NULL){
  list(tEvent = tEvent, PAR = PAR, tag = "feverPfMOI")
}

#' PfMOI \code{Human} Event: PfMOI Infection Event
#'
#' Clear a PfMOI infection corresponding to the given PfID.
#' This method is bound to \code{Human$endPfMOI()}
#'
#' @param tEvent time of clearance
#' @param PAR \code{NULL}
#'
feverPfMOI <- function(tEvent, PAR){
  # track event
  MOI = private$Pathogens$get_MOI()
  writeLines(text = paste0(c(private$myID,tEvent,"F",MOI,"NULL"),collapse = ","),con = private$HumansPointer$get_conPathogen(), sep = "\n")
  # queue treatment
  if(runif(1) < self$get_PfMOI_PAR("TreatPf")){
    self$add2Q_treatPfMOI(tEvent = tEvent)
  }
}


###################################################################
# Treatment
###################################################################

#' PfMOI \code{Human} Event: Add PfMOI Treatment Event to Event Queue
#'
#' Add PfMOI treatment event to the event queue.
#' This method is called from \code{\link{feverPfMOI}}
#' This method adds event \code{\link{event_treatPfMOI}} to the event queue.
#' This method is bound to \code{Human$add2Q_treatPfMOI()}
#'  * tEvent: treatment time is calculated as tTreat = tEvent + \code{\link{PfMOI_ttTreatPf}}
#'
#' @param tEvent time of fever
#' @param PAR \code{NULL}
#'
add2Q_treatPfMOI <- function(tEvent, PAR = NULL){
  tTreat = tEvent + self$ttTreatPf()
  private$EventQueue$addEvent2Q(event = self$event_treatPfMOI(tEvent = tTreat, PAR = PAR))
}

#' PfMOI \code{Human} Event: Generate PfMOI Treatment Event
#'
#' Generate PfMOI treatment event to place in event queue.
#' This method is called from \code{\link{add2Q_treatPfMOI}}
#' This method is bound to \code{Human$event_treatPfMOI()}
#'  * tag: \code{\link{treatPfMOI}}
#'
#' @param tEvent time of treatment
#' @param PAR \code{NULL}
#'
event_treatPfMOI <- function(tEvent, PAR = NULL){
  list(tEvent = tEvent, PAR = PAR, tag = "treatPfMOI")
}

#' PfMOI \code{Human} Event: PfMOI Treatment Event
#'
#' Simulate a PfMOI treatment event. If the human is infected, set susceptible and track history; also initiate period of chemoprophylaxis, see \code{\link{add2Q_endprophylaxisPfMOI}}
#' This method is bound to \code{Human$treatPfMOI()}
#'
#' @param tEvent time of treatment
#' @param PAR \code{NULL}
#'
treatPfMOI <- function(tEvent, PAR){

  # clear all infections
  private$Pathogens$clear_Infections()
  private$Pathogens$set_chemoprophylaxis(TRUE)

  # remove future fever and clearance events
  private$EventQueue$rmTagFromQ(tag = "endPfMOI")
  private$EventQueue$rmTagFromQ(tag = "feverPfMOI")

  # track event
  MOI = private$Pathogens$get_MOI()
  writeLines(text = paste0(c(private$myID,tEvent,"P",MOI,"NULL"),collapse = ","),con = private$HumansPointer$get_conPathogen(), sep = "\n")

  # Initiate a period of protection from chemoprophylaxis
  self$add2Q_endprophylaxisPfMOI(tEvent)
}


###################################################################
# End of Chemoprophylaxis
###################################################################

#' PfMOI \code{Human} Event: Add PfMOI End of Chemoprophylaxis Event to Event Queue
#'
#' Add PfMOI end of chemoprophylaxis event to the event queue.
#' This method is called from \code{\link{treatPfMOI}}
#' This method adds event \code{\link{event_endprophylaxisPfMOI}} to the event queue.
#' This method is bound to \code{Human$add2Q_endprophylaxisPfMOI()}
#'  * tEvent: treatment time is calculated as tSusceptible = tEvent + \code{\link{PfMOI_ttSusceptiblePf}}
#'
#' @param tEvent time of event
#' @param PAR \code{NULL}
#'
add2Q_endprophylaxisPfMOI <- function(tEvent, PAR = NULL){
  tSusceptible = tEvent + self$ttSusceptiblePf()
  private$EventQueue$addEvent2Q(event = self$event_endprophylaxisPfMOI(tEvent = tSusceptible, PAR = PAR))
}

#' PfMOI \code{Human} Event: Generate PfMOI End of Chemoprophylaxis Event
#'
#' Generate PfMOI end of chemoprophylaxis event to place in event queue.
#' This method is called from \code{\link{add2Q_endprophylaxisPfMOI}}
#' This method is bound to \code{Human$event_endprophylaxisPfMOI()}
#'  * tag: \code{\link{endprophylaxisPfMOI}}
#'
#' @param tEvent time to end chemoprophylaxis
#' @param PAR \code{NULL}
#'
event_endprophylaxisPfMOI <- function(tEvent, PAR = NULL){
  list(tEvent = tEvent, PAR = PAR, tag = "endprophylaxisPfMOI")
}

#' PfMOI \code{Human} Event: PfMOI End of Chemoprophylaxis Event
#'
#' End PfMOI chemoprophylaxis protection.
#' This method is bound to \code{Human$endprophylaxisPfMOI()}
#' @param tEvent time to end chemoprophylaxis
#' @param PAR \code{NULL}
#'
endprophylaxisPfMOI <- function(tEvent, PAR){

  # End Prophylaxis
  private$Pathogens$set_chemoprophylaxis(FALSE)

  # track event
  MOI = private$Pathogens$get_MOI()
  writeLines(text = paste0(c(private$myID,tEvent,"S",MOI,"NULL"),collapse = ","),con = private$HumansPointer$get_conPathogen(), sep = "\n")
}


###################################################################
# HUMAN PE vaccination functions
###################################################################

#' PfMOI \code{Human} Event: Add PfMOI PE Vaccination Event to Event Queue
#'
#' Add PfMOI PE vaccination event to the event queue. PE vaccination targets mosquito to human transmission,
#' blocking sporozoites when they enter the bloodstream from an infectious bite.
#' This method is called from \code{\link{queueVaccination_SimBitePfMOI}}
#' This method adds event \code{\link{event_pevaccinatePfMOI}} to the event queue.
#' This method is bound to \code{Human$add2Q_pevaccinatePfMOI()}
#'
#' @param tEvent time of vaccination
#' @param PAR \code{NULL}
add2Q_pevaccinatePfMOI <- function(tEvent, PAR = NULL){
  private$EventQueue$addEvent2Q(event = self$event_pevaccinatePfMOI(tEvent = tEvent, PAR = PAR))
}

#' PfMOI \code{Human} Event: Generate PfMOI PE Vaccination Event
#'
#' Generate PfMOI PE vaccination event to place in event queue.
#' This method is called from \code{\link{add2Q_pevaccinatePfMOI}}
#' This method is bound to \code{Human$event_pevaccinatePfMOI()}
#'  * tag: \code{\link{pevaccinatePfMOI}}
#' @md
#' @param tEvent begin PE protection
#' @param PAR \code{NULL}
event_pevaccinatePfMOI <- function(tEvent, PAR = NULL){
  list(tEvent = tEvent, PAR = PAR, tag = "pevaccinatePfMOI")
}

#' PfMOI \code{Human} Event: PfMOI PE Vaccination Event
#'
#' Begin PfMOI PE vaccination protection.
#' This method is bound to \code{Human$endprophylaxisPfMOI()}
#'  * protection: infected mosquito to human transmission efficiency is modified by \code{peBlockPf}, see \code{\link{PfMOI.Parameters}}
#'  * waning efficacy: queue \code{\link{add2Q_pewanePfMOI}}
#' @md
#' @param tEvent begin PE protection
#' @param PAR \code{NULL}
pevaccinatePfMOI <- function(tEvent, PAR){
  if(runif(1) < self$get_PfMOI_PAR("PEProtectPf")){
    private$Pathogens$set_b(self$get_PfMOI_PAR("Pf_b") * (1-self$get_PfMOI_PAR("peBlockPf")))
    self$add2Q_pewanePfMOI(tEvent = tEvent)
    private$Pathogens$track_history(tEvent = tEvent, event = "PEvaxx")
  }
}

#' PfMOI \code{Human} Event: Add PfMOI PE Waning Protection Event to Event Queue
#'
#' Add PfMOI PE waning protection event to the event queue.
#' This method is called from \code{\link{pevaccinatePfMOI}}
#' This method adds event \code{\link{event_pewanePfMOI}} to the event queue.
#' This method is bound to \code{Human$add2Q_pewanePfMOI()}
#'
#' @param tEvent time of vaccination
#' @param PAR \code{NULL}
add2Q_pewanePfMOI <- function(tEvent, PAR = NULL){
  private$EventQueue$addEvent2Q(event = self$event_pewanePfMOI(tEvent = tEvent, PAR = PAR))
}

#' PfMOI \code{Human} Event: Generate PfMOI PE Waning Protection Event
#'
#' Generate PfMOI PE waning protection event to place in event queue.
#' This method is called from \code{\link{add2Q_pevaccinatePfMOI}}
#' This method is bound to \code{Human$event_pevaccinatePfMOI()}
#'  * tag: \code{\link{pevaccinatePfMOI}}
#'  * tEvent: loss of efficacy is calculated as tWane = tEvent + \code{\link{PfMOI_ttPEWanePf}}
#' @md
#' @param tEvent end PE protection
#' @param PAR \code{NULL}
event_pewanePfMOI <- function(tEvent, PAR = NULL){
  tWane = tEvent + self$ttPEWanePf()
  list(tEvent = tWane, PAR = PAR, tag = "pewanePfMOI")
}

#' PfMOI \code{Human} Event: PfMOI PE Waning Protection Event
#'
#' End PfMOI PE protection.
#' This method is bound to \code{Human$pewanePfMOI()}
#'  * protection: infected mosquito to human transmission efficiency is set back to \code{Pf_b}, see \code{\link{PfMOI.Parameters}}
#' @md
#' @param tEvent end PE protection
#' @param PAR \code{NULL}
pewanePfMOI <- function(tEvent, PAR){
  private$Pathogens$set_b(self$get_PfMOI_PAR("Pf_b"))
  private$Pathogens$track_history(tEvent = tEvent, event = "PEwane")
}
