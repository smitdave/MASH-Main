###############################################################################
#       ____  _________ ____
#      / __ \/ __/ ___//  _/
#     / /_/ / /_ \__ \ / /
#    / ____/ __/___/ // /
#   /_/   /_/  /____/___/
#
#   MASH-MACRO
#   PfSI Methods
#   MASH Team
#   November 2017
#
###############################################################################


###################################################################
# PfSI Parameter Getters & Setters
###################################################################

#' PfSI \code{Human} Method: Get PfSI_PAR
#'
#' Get named element of PfSI_PAR. See \code{\link{PfSI.Parameters}} for definitions.
#'  * This method is bound to \code{Human$get_PfSI_PAR()}
#'
Human_get_PfSI_PAR <- function(ix){
  return(private$HumansPointer$get_PfSI_PAR(ix))
}

#' PfSI \code{HumanPop} Method: Get PfSI_PAR
#'
#' Get named element of PfSI_PAR. See \code{\link{PfSI.Parameters}} for definitions.
#'  * This method is bound to \code{HumanPop$get_PfSI_PAR()}
#'
#' @param ix name of parameter to return \code{\link{PfSI.Parameters}}
#'
HumanPop_get_PfSI_PAR <- function(ix){
  return(private$PfSI_PAR[[ix]])
}

#' PfSI \code{HumanPop} Method: Set PfSI Paramters for a \code{\link{HumanPop}}
#'
#' Set PfSI_PAR for a HumanPop; this is useful for simulating multiple populations with different parameter values.
#'  * This method is bound to \code{HumanPop$set_PfSI_PAR()}
#'
#' @param PfSI_PAR new parameter list, see \code{\link{PfSI.Parameters}}
#'
HumanPop_set_PfSI_PAR <- function(PfSI_PAR){
  private$PfSI_PAR = PfSI_PAR
}


###################################################################
# Add PfSI Utilities to 'HumanPop' Class
###################################################################

#' PfSI \code{HumanPop} Method: Increment PfID
#'
#' Whenever a new liver-stage infection manifests in a human we increment the PfID counter and return the new PfID.
#'  * This method is bound to \code{HumanPop$increment_PfID()}
#'
PfSI_increment_PfID <- function(){
  private$PfID = private$PfID + 1L
  return(private$PfID)
}

#' PfSI \code{HumanPop} Method: Initialize PfSI Infections
#'
#' Initialize PfSI infections with parasite prevalence PfPR in a human population.
#'  * This method is bound to \code{HumanPop$init_PfSI()}
#'
#' @param PfPR numeric; prevalence
#'
init_PfSI_HumanPop <- function(PfPR){

  private$PfID = 1L
  self$set_humanPfSI()

  private$pop$apply(tag = "init_PfSI",returnVal=FALSE,PfPR=PfPR)
}

#' PfSI \code{Human} Method: Initialize PfSI Infection
#'
#' Initialize infection with given probability for a single Human. This must be called after \code{\link{Human_set_humanPfSI}} is used to initialize the pathogen object.
#'  * This method is bound to \code{Human$init_PfSI()}
#'
#' @param PfPR numeric; probability I am infected
#'
init_PfSI_Human <- function(PfPR){
  if(runif(1) < PfPR[private$patchID]){
    self$infectHumanPfSI(tEvent = 0, PAR = list(vectorID="initInf"))
  } else {
    self$get_Pathogens()$track_history(tEvent = 0, event = "S")
  }
}


###################################################################
# Add PfSI Pathogen Object to 'Human' & 'HumanPop' Class
###################################################################

#' PfSI \code{Human} Method: Set Human-stage PfSI Object
#'
#' Set the \code{\link[MASHcpp]{humanPfSI}} object in a human.
#'  * This method is bound to \code{Human$set_humanPfSI()}
#'
Human_set_humanPfSI <- function(PfID, tInf = -1L, b = 0.55, c = 0.15, infected = FALSE, chemoprophylaxis = FALSE){
  private$Pathogens = MASHcpp::humanPfSI(PfID, tInf, b, c, infected, chemoprophylaxis)
}

#' PfSI \code{HumanPop} Method: Set Human-stage PfSI Object
#'
#' Set the \code{\link[MASHcpp]{humanPfSI}} object in a human poulation; this does not initialize infection, only sets the object.
#'  * This method is bound to \code{HumanPop$set_humanPfSI()}
#'
#' @param b infected mosquito to human transmission efficiency
#' @param c infected human to mosquito transmission efficiency
HumanPop_set_humanPfSI <- function(){

  # set pathogens
  private$pop$apply(tag = "set_humanPfSI",returnVal=FALSE,PfID=-1L,b=private$PfSI_PAR$Pf_b,c=private$PfSI_PAR$Pf_c)

}


###################################################################
# Add PfSI Timing Functions to 'Human' Class
###################################################################

#' PfSI \code{Human} Method: Duration of Infection
#'
#' How many days does a PfSI infection last? See \code{\link{PfSI.Parameters}} parameter \code{DurationPf} for mean of exponentially distributed infection.
#' This method is called from \code{\link{event_endPfSI}}
#'  * This method is bound to \code{Human$ttClearPf()}
#'
PfSI_ttClearPf <- function(){
  return(rexp(n = 1, rate = 1/self$get_PfSI_PAR("DurationPf")))
}

#' PfSI \code{Human} Method: Duration of Latency
#'
#' How many days after the infectious bite does a PfSI infection start? See \code{\link{PfSI.Parameters}} parameter \code{LatentPf} constant latent period.
#' This method is called from \code{\link{infectiousBite_PfSI}}
#'  * This method is bound to \code{Human$ttInfectionPf()}
#'
PfSI_ttInfectionPf <- function(){
  return(self$get_PfSI_PAR("LatentPf"))
}

#' PfSI \code{Human} Method: Timing of Fever Incident
#'
#' What is the timing of fever relative to the start of a PfSI infection? See \code{\link{PfSI.Parameters}} parameter \code{mnFeverPf} constant timing period.
#' This method is called from \code{\link{event_feverPfSI}}
#'  * This method is bound to \code{Human$ttFeverPf()}
#'
PfSI_ttFeverPf <- function(){
  return(self$get_PfSI_PAR("mnFeverPf"))
}

#' PfSI \code{Human} Method: Timing of Treatment
#'
#' What is the timing of treatment relative to the start of a fever incident? See \code{\link{PfSI.Parameters}} parameter \code{mnTreatPf} constant timing period.
#' This method is called from \code{\link{event_treatPfSI}}
#'  * This method is bound to \code{Human$ttTreatPf()}
#'
PfSI_ttTreatPf <- function(){
  return(self$get_PfSI_PAR("mnTreatPf"))
}

#' PfSI \code{Human} Method: Duration of Protection from Chemoprophylaxis
#'
#' After administration of Chemoprophylaxis what is time to susceptibility? See \code{\link{PfSI.Parameters}} parameter \code{mnChemoprophylaxisPf} constant timing period.
#' This method is called from \code{\link{event_endprophylaxisPfSI}}
#'  * This method is bound to \code{Human$ttSusceptiblePf()}
#'
PfSI_ttSusceptiblePf <- function(){
  return(self$get_PfSI_PAR("mnChemoprophylaxisPf"))
}

#' PfSI \code{Human} Method: Duration of protection by PE Vaccination
#'
#' After administration of PE Vaccination what is duration of protection (reduced infected mosquito to human transmission efficiency)? See \code{\link{PfSI.Parameters}} parameter \code{mnPEPf} and \code{vrPEPf} for normally distributed duration of protection.
#' This method is called from \code{\link{event_pewanePfSI}}
#'  * This method is bound to \code{Human$ttPEWanePf()}
#'
PfSI_ttPEWanePf <- function(){
  return(rnorm(n = 1, mean = self$get_PfSI_PAR("mnPEPf"), sd = self$get_PfSI_PAR("vrPEPf")))
}

#' PfSI \code{Human} Method: Duration of protection by GS Vaccination
#'
#' Duration of protection by GS Vaccination? See \code{\link{PfSI.Parameters}} parameter \code{mnGSPf} and \code{vrGSPf} for normally distributed duration of protection.
#' This method is called from \code{\link{event_gswanePfSI}}
#'  * This method is bound to \code{Human$ttGSWanePf()}
#'
PfSI_ttGSWanePf <- function(){
  return(rnorm(n = 1, mean = self$get_PfSI_PAR("mnGSPf"), sd = self$get_PfSI_PAR("vrGSPf")))
}


###################################################################
# Add PfSI Events to 'Human' Class
# 'XX' family of functions for human event queues
###################################################################

###################################################################
# PfSI: Mosquito to Human infectious bite
# Add methods to 'Human' Class
###################################################################

#' PfSI \code{Human} Method: Host Probing
#'
#' This method is called by a mosquito when she probes a human host, but may also be called by \code{\link{SimBitePfSI}} as a filler.
#' If the biting mosquito is infectious, the method calls \code{\link{infectiousBite_PfSI}}, otherwise does nothing.
#'  * This method is bound to \code{Human$probeHost_PfSI()}
#'
#' @param tBite time of bite
#' @param mosquitoPfSI \code{\link{mosquitoPfSI}} object passed from mosquito to human
#'
probeHost_PfSI <- function(tBite, mosquitoPfSI){
  if(mosquitoPfSI$get_infected()){
    PAR = list(vectorID = mosquitoPfSI$get_MosquitoID())
    self$infectiousBite_PfSI(tBite, PAR)
  }

}

#' PfSI \code{Human} Method: Infectious Bite on Human
#'
#' This method is called from \code{\link{probeHost_PfSI}}.
#' If the infectious bite results in successful transmission, this function queues a human infection event, see \code{\link{add2Q_infectHumanPfSI}}
#'  * This method is bound to \code{Human$infectiousBite_PfSI()}
#'
#' @param tBite time of bite
#' @param mosquitoPfSI \code{\link{mosquitoPfSI}} object passed from mosquito to human
#'
infectiousBite_PfSI <- function(tBite, PAR){
  if(runif(1) < private$Pathogens$get_b()){
    tInfStart = tBite + self$ttInfectionPf()
    self$add2Q_infectHumanPfSI(tEvent = tInfStart, PAR = PAR)
  }
}


###################################################################
# Start a PfSI Infection
###################################################################

#' PfSI \code{Human} Event: Add PfSI Infection Event to Event Queue
#'
#' Add a PfSI infection to the event queue.
#' This method is called from \code{\link{infectiousBite_PfSI}}
#' This method adds event \code{\link{event_infectHumanPfSI}} to the event queue.
#'  * This method is bound to \code{Human$add2Q_infectHumanPfSI()}
#'
#' @param tEvent time of infection
#' @param PAR \code{NULL}
#'
add2Q_infectHumanPfSI <- function(tEvent, PAR = NULL){
  private$EventQueue$addEvent2Q(event = self$event_infectHumanPfSI(tEvent = tEvent, PAR = PAR))
}

#' PfSI \code{Human} Event: Generate PfSI Infection Event
#'
#' Generate PfSI infection event to place in event queue.
#' This method is called from \code{\link{add2Q_infectHumanPfSI}}
#' This method is bound to \code{Human$event_infectHumanPfSI()}
#'  * tag: \code{\link{infectHumanPfSI}}
#
#' @param tEvent time of infection
#' @param PAR \code{NULL}
#'
event_infectHumanPfSI <- function(tEvent, PAR = NULL){
  return(
    list(tEvent = tEvent, PAR = PAR, tag = "infectHumanPfSI")
  )
}

#' PfSI \code{Human} Event: PfSI Infection Event
#'
#' Simulate a PfSI infection. If the human is not under chemoprophylaxis or already infected, begin an infection.
#' This method is bound to \code{Human$infectHumanPfSI()}
#'  * A Bernoulli event is drawn to determine if this infection produces fever; if so \code{\link{add2Q_feverPfSI}} is called.
#'  * The end of this PfSI infection is queued by \code{\link{add2Q_endPfSI}}
#'
#' @param tEvent time of infection
#' @param PAR must be a list containing character \code{vectorID}
#'
infectHumanPfSI <- function(tEvent, PAR){
  if(!private$Pathogens$get_infected() & !private$Pathogens$get_chemoprophylaxis()){
    private$Pathogens$track_history(tEvent = tEvent, event = "I") # track history
    private$Pathogens$set_infected(TRUE)
    # increment PfID
    private$Pathogens$push_PfID(private$HumansPointer$increment_PfID())
    private$Pathogens$push_vectorInf(PAR$vectorID)
    if(runif(1) < self$get_PfSI_PAR("FeverPf")){
        self$add2Q_feverPfSI(tEvent = tEvent)
    }
    self$add2Q_endPfSI(tEvent = tEvent)
  }
}

###################################################################
# End an PfSI Infection
###################################################################

#' PfSI \code{Human} Event: Add PfSI Clearance Event to Event Queue
#'
#' Add PfSI infection clearance event to the event queue.
#' This method is called from \code{\link{infectHumanPfSI}}
#' This method adds event \code{\link{event_endPfSI}} to the event queue.
#'  * This method is bound to \code{Human$add2Q_endPfSI()}
#'
#' @param tEvent time of event
#' @param PAR \code{NULL}
add2Q_endPfSI <- function(tEvent, PAR = NULL){
  private$EventQueue$addEvent2Q(event = self$event_endPfSI(tEvent = tEvent, PAR = PAR))
}

#' PfSI \code{Human} Event: Generate PfSI Clearance Event
#'
#' Generate PfSI clearance event to place in event queue.
#' This method is called from \code{\link{add2Q_endPfSI}}
#' This method is bound to \code{Human$event_endPfSI()}
#'  * tag: \code{\link{endPfSI}}
#'  * tEvent: clearance time is calculated as tEnd = tEvent + \code{\link{PfSI_ttClearPf}}
#'
#' @param tEvent time of clearance
#' @param PAR \code{NULL}
event_endPfSI <- function(tEvent, PAR = NULL){
  tEnd = tEvent + self$ttClearPf()
  return(
    list(tEvent = tEnd, PAR = PAR, tag = "endPfSI")
  )
}

#' PfSI \code{Human} Event: PfSI Clearance Event
#'
#' Clear a PfSI infection. If the human is infected, set susceptible and track history.
#'  * This method is bound to \code{Human$endPfSI()}
#' @param tEvent time of clearance
#' @param PAR \code{NULL}
endPfSI <- function(tEvent, PAR){
  if(private$Pathogens$get_infected()){
    private$EventQueue$rmTagFromQ("feverPfSI")
    private$Pathogens$track_history(tEvent = tEvent, event = "S") # track history
    private$Pathogens$set_infected(FALSE)
  }
}

###################################################################
# Fever
###################################################################

#' PfSI \code{Human} Event: Add PfSI Fever Event to Event Queue
#'
#' Add PfSI fever event to the event queue.
#' This method is called from \code{\link{infectHumanPfSI}}
#' This method adds event \code{\link{event_feverPfSI}} to the event queue.
#'  * This method is bound to \code{Human$add2Q_feverPfSI()}
#'
#' @param tEvent time of event
#' @param PAR \code{NULL}
add2Q_feverPfSI <- function(tEvent, PAR = NULL){
  private$EventQueue$addEvent2Q(event = self$event_feverPfSI(tEvent = tEvent, PAR = PAR))
}

#' PfSI \code{Human} Event: Generate PfSI Fever Event
#'
#' Generate PfSI dever event to place in event queue.
#' This method is called from \code{\link{add2Q_feverPfSI}}
#' This method is bound to \code{Human$event_feverPfSI()}
#'  * tag: \code{\link{feverPfSI}}
#'  * tEvent: fever time is calculated as tFever = tEvent + \code{\link{PfSI_ttFeverPf}}
#'
#' @param tEvent time of clearance
#' @param PAR \code{NULL}
event_feverPfSI <- function(tEvent, PAR = NULL){
  tFever = tEvent + self$ttFeverPf()
  return(
    list(tEvent = tFever, PAR = PAR, tag = "feverPfSI")
  )
}

#' PfSI \code{Human} Event: PfSI Fever Event
#'
#' Simulate a PfSI fever event.
#' This method is bound to \code{Human$feverPfSI()}
#'  * \code{\link{add2Q_treatPfSI}}: fever may initiate treatment with probability \code{TreatPf}, see \code{\link{PfSI.Parameters}}
#'
#' @param tEvent time of fever
#' @param PAR \code{NULL}
feverPfSI <- function(tEvent, PAR){
  private$Pathogens$track_history(tEvent = tEvent, event = "F")
  if(runif(1) < self$get_PfSI_PAR("TreatPf")){
    self$add2Q_treatPfSI(tEvent = tEvent)
  }
}


###################################################################
# Treatment
###################################################################

#' PfSI \code{Human} Event: Add PfSI Treatment Event to Event Queue
#'
#' Add PfSI treatment event to the event queue.
#' This method is called from \code{\link{feverPfSI}}
#' This method adds event \code{\link{event_treatPfSI}} to the event queue.
#'  * This method is bound to \code{Human$add2Q_treatPfSI()}
#'
#' @param tEvent time of fever
#' @param PAR \code{NULL}
add2Q_treatPfSI <- function(tEvent, PAR = NULL){
  private$EventQueue$addEvent2Q(event = self$event_treatPfSI(tEvent = tEvent, PAR = PAR))
}

#' PfSI \code{Human} Event: Generate PfSI Treatment Event
#'
#' Generate PfSI treatment event to place in event queue.
#' This method is called from \code{\link{add2Q_treatPfSI}}
#' This method is bound to \code{Human$event_treatPfSI()}
#'  * tag: \code{\link{treatPfSI}}
#'  * tEvent: treatment time is calculated as tTreat = tEvent + \code{\link{PfSI_ttTreatPf}}
#'
#' @param tEvent time of treatment
#' @param PAR \code{NULL}
event_treatPfSI <- function(tEvent, PAR = NULL){
  tTreat = tEvent + self$ttTreatPf()
  return(
    list(tEvent = tTreat, PAR = PAR, tag = "treatPfSI")
  )
}

#' PfSI \code{Human} Event: PfSI Treatment Event
#'
#' Simulate a PfSI treatment event. If the human is infected, set susceptible and track history; also initiate period of chemoprophylaxis, see \code{\link{add2Q_endprophylaxisPfSI}}
#'  * This method is bound to \code{Human$treatPfSI()}
#' @param tEvent time of treatment
#' @param PAR \code{NULL}
treatPfSI <- function(tEvent, PAR){
  # treat
  if(private$Pathogens$get_infected()){
    private$Pathogens$set_infected(FALSE)
    private$Pathogens$track_history(tEvent = tEvent, event = "S")
  }
  private$Pathogens$set_chemoprophylaxis(TRUE)
  private$Pathogens$track_history(tEvent = tEvent, event = "P")
  # Initiate a period of protection from chemoprophylaxis
  self$add2Q_endprophylaxisPfSI(tEvent = tEvent)

}


###################################################################
# End of Chemoprophylaxis
###################################################################

#' PfSI \code{Human} Event: Add PfSI End of Chemoprophylaxis Event to Event Queue
#'
#' Add PfSI end of chemoprophylaxis event to the event queue.
#' This method is called from \code{\link{treatPfSI}}
#' This method adds event \code{\link{event_endprophylaxisPfSI}} to the event queue.
#'  * This method is bound to \code{Human$add2Q_endprophylaxisPfSI()}
#'
#' @param tEvent time of event
#' @param PAR \code{NULL}
add2Q_endprophylaxisPfSI <- function(tEvent, PAR = NULL){
  private$EventQueue$addEvent2Q(event = self$event_endprophylaxisPfSI(tEvent = tEvent, PAR = PAR))
}

#' PfSI \code{Human} Event: Generate PfSI End of Chemoprophylaxis Event
#'
#' Generate PfSI end of chemoprophylaxis event to place in event queue.
#' This method is called from \code{\link{add2Q_endprophylaxisPfSI}}
#' This method is bound to \code{Human$event_endprophylaxisPfSI()}
#'  * tag: \code{\link{endprophylaxisPfSI}}
#'  * tEvent: treatment time is calculated as tSusceptible = tEvent + \code{\link{PfSI_ttSusceptiblePf}}
#'
#' @param tEvent time to end chemoprophylaxis
#' @param PAR \code{NULL}
event_endprophylaxisPfSI <- function(tEvent, PAR = NULL){
  tSusceptible = tEvent + self$ttSusceptiblePf()
  return(
    list(tEvent = tSusceptible, PAR = PAR, tag = "endprophylaxisPfSI")
  )
}

#' PfSI \code{Human} Event: PfSI End of Chemoprophylaxis Event
#'
#' End PfSI chemoprophylaxis protection.
#'  * This method is bound to \code{Human$endprophylaxisPfSI()}
#' @param tEvent time to end chemoprophylaxis
#' @param PAR \code{NULL}
endprophylaxisPfSI <- function(tEvent, PAR){
  # End Prophylaxis
  private$Pathogens$track_history(tEvent = tEvent, event = "S")
  private$Pathogens$set_chemoprophylaxis(FALSE)

}


###################################################################
# HUMAN PE vaccination functions
###################################################################

#' PfSI \code{Human} Event: Add PfSI PE Vaccination Event to Event Queue
#'
#' Add PfSI PE vaccination event to the event queue.
#' This method is called from \code{\link{queueVaccination_SimBitePfSI}}
#' This method adds event \code{\link{event_pevaccinatePfSI}} to the event queue.
#'  * This method is bound to \code{Human$add2Q_pevaccinatePfSI()}
#'
#' @param tEvent time of vaccination
#' @param PAR \code{NULL}
add2Q_pevaccinatePfSI <- function(tEvent, PAR = NULL){
  private$EventQueue$addEvent2Q(event = self$event_pevaccinatePfSI(tEvent = tEvent, PAR = PAR))
}

#' PfSI \code{Human} Event: Generate PfSI PE Vaccination Event
#'
#' Generate PfSI PE vaccination event to place in event queue.
#' This method is called from \code{\link{add2Q_pevaccinatePfSI}}
#' This method is bound to \code{Human$event_pevaccinatePfSI()}
#'  * tag: \code{\link{pevaccinatePfSI}}
#'
#' @param tEvent begin PE protection
#' @param PAR \code{NULL}
event_pevaccinatePfSI <- function(tEvent, PAR = NULL){
  return(
    list(tEvent = tEvent, PAR = PAR, tag = "pevaccinatePfSI")
  )
}

#' PfSI \code{Human} Event: PfSI PE Vaccination Event
#'
#' Begin PfSI PE vaccination protection.
#' This method is bound to \code{Human$endprophylaxisPfSI()}
#'  * protection: infected mosquito to human transmission efficiency is modified by \code{peBlockPf}, see \code{\link{PfSI.Parameters}}
#'  * waning efficacy: queue \code{\link{add2Q_pewanePfSI}}
#'
#' @param tEvent begin PE protection
#' @param PAR \code{NULL}
pevaccinatePfSI <- function(tEvent, PAR){
  if(runif(1) < self$get_PfSI_PAR("PEProtectPf")){
    private$Pathogens$track_history(tEvent = tEvent, event = "PEvaxx")
    private$Pathogens$set_b(self$get_PfSI_PAR("Pf_b") * (1-self$get_PfSI_PAR("peBlockPf")))
    self$add2Q_pewanePfSI(tEvent = tEvent)
  }
}

#' PfSI \code{Human} Event: Add PfSI PE Waning Protection Event to Event Queue
#'
#' Add PfSI PE waning protection event to the event queue.
#' This method is called from \code{\link{pevaccinatePfSI}}
#' This method adds event \code{\link{event_pewanePfSI}} to the event queue.
#'  * This method is bound to \code{Human$add2Q_pewanePfSI()}
#'
#' @param tEvent time of vaccination
#' @param PAR \code{NULL}
add2Q_pewanePfSI <- function(tEvent, PAR = NULL){
  private$EventQueue$addEvent2Q(event = self$event_pewanePfSI(tEvent = tEvent, PAR = PAR))
}

#' PfSI \code{Human} Event: Generate PfSI PE Waning Protection Event
#'
#' Generate PfSI PE waning protection event to place in event queue.
#' This method is called from \code{\link{add2Q_pevaccinatePfSI}}
#' This method is bound to \code{Human$event_pevaccinatePfSI()}
#'  * tag: \code{\link{pevaccinatePfSI}}
#'  * tEvent: loss of efficacy is calculated as tWane = tEvent + \code{\link{PfSI_ttPEWanePf}}
#'
#' @param tEvent end PE protection
#' @param PAR \code{NULL}
event_pewanePfSI <- function(tEvent, PAR = NULL){
  tWane = tEvent + self$ttPEWanePf()
  return(
    list(tEvent = tWane, PAR = PAR, tag = "pewanePfSI")
  )
}

#' PfSI \code{Human} Event: PfSI PE Waning Protection Event
#'
#' End PfSI PE protection.
#' This method is bound to \code{Human$pewanePfSI()}
#'  * protection: infected mosquito to human transmission efficiency is set back to \code{Pf_b}, see \code{\link{PfSI.Parameters}}
#'
#' @param tEvent end PE protection
#' @param PAR \code{NULL}
pewanePfSI <- function(tEvent, PAR){
  private$Pathogens$track_history(tEvent = tEvent, event = "PEwane")
  private$Pathogens$set_b(self$get_PfSI_PAR("Pf_b"))
}

###################################################################
# HUMAN GS vaccination functions
###################################################################

#' PfSI \code{Human} Event: Add PfSI GS Vaccination Event to Event Queue
#'
#' Add PfSI GS vaccination event to the event queue.
#' This method is called from \code{\link{queueVaccination_SimBitePfSI}}
#' This method adds event \code{\link{event_gsvaccinatePfSI}} to the event queue.
#'  * This method is bound to \code{Human$add2Q_gsvaccinatePfSI()}
#'
#' @param tEvent time of vaccination
#' @param PAR \code{NULL}
add2Q_gsvaccinatePfSI <- function(tEvent, PAR = NULL){
  private$EventQueue$addEvent2Q(event = self$event_gsvaccinatePfSI(tEvent = tEvent, PAR = PAR))
}

#' PfSI \code{Human} Event: Generate PfSI GS Vaccination Event
#'
#' Generate PfSI GS vaccination event to place in event queue.
#' This method is called from \code{\link{add2Q_gsvaccinatePfSI}}
#' This method is bound to \code{Human$event_gsvaccinatePfSI()}
#'  * tag: \code{\link{gsvaccinatePfSI}}
#'
#' @param tEvent begin gs protection
#' @param PAR \code{NULL}
event_gsvaccinatePfSI <- function(tEvent, PAR = NULL){
  return(
    list(tEvent = tEvent, PAR = PAR, tag = "gsvaccinatePfSI")
  )
}

#' PfSI \code{Human} Event: PfSI GS Vaccination Event
#'
#' Begin PfSI GS vaccination protection.
#' This method is bound to \code{Human$endprophylaxisPfSI()}
#'  * protection: infected human to mosquito transmission efficiency is modified by \code{gsBlockPf}, see \code{\link{PfSI.Parameters}}
#'  * waning efficacy: queue \code{\link{add2Q_gswanePfSI}}
#'
#' @param tEvent begin gs protection
#' @param PAR \code{NULL}
gsvaccinatePfSI <- function(tEvent, PAR){
  if(runif(1) < self$get_PfSI_PAR("GSProtectPf")){
    private$Pathogens$track_history(tEvent = tEvent, event = "GSvaxx")
    private$Pathogens$set_c(self$get_PfSI_PAR("Pf_c") * (1-self$get_PfSI_PAR("gsBlockPf")))
    self$add2Q_gswanePfSI(tEvent = tEvent)
  }
}

#' PfSI \code{Human} Event: Add PfSI GS Waning Protection Event to Event Queue
#'
#' Add PfSI GS waning protection event to the event queue.
#' This method is called from \code{\link{gsvaccinatePfSI}}
#' This method adds event \code{\link{event_gswanePfSI}} to the event queue.
#'  * This method is bound to \code{Human$add2Q_gswanePfSI()}
#'
#' @param tEvent time of vaccination
#' @param PAR \code{NULL}
add2Q_gswanePfSI <- function(tEvent, PAR = NULL){
  private$EventQueue$addEvent2Q(event = self$event_gswanePfSI(tEvent = tEvent, PAR = PAR))
}

#' PfSI \code{Human} Event: Generate PfSI GS Waning Protection Event
#'
#' Generate PfSI GS waning protection event to place in event queue.
#' This method is called from \code{\link{add2Q_gsvaccinatePfSI}}
#' This method is bound to \code{Human$event_gsvaccinatePfSI()}
#'  * tag: \code{\link{gsvaccinatePfSI}}
#'  * tEvent: loss of efficacy is calculated as tWane = tEvent + \code{\link{PfSI_ttGSWanePf}}
#'
#' @param tEvent end gs protection
#' @param PAR \code{NULL}
event_gswanePfSI <- function(tEvent, PAR = NULL){
  tWane = tEvent + self$ttGSWanePf()
  return(
    list(tEvent = tWane, PAR = PAR, tag = "gswanePfSI")
  )
}

#' PfSI \code{Human} Event: PfSI GS Waning Protection Event
#'
#' End PfSI GS protection.
#' This method is bound to \code{Human$gswanePfSI()}
#'  * protection: infected human to mosquito transmission efficiency is set back to \code{Pf_c}, see \code{\link{PfSI.Parameters}}
#'
#' @param tEvent end GS protection
#' @param PAR \code{NULL}
gswanePfSI <- function(tEvent, PAR){
  private$Pathogens$track_history(tEvent = tEvent, event = "GSwane")
  private$Pathogens$set_c(self$get_PfSI_PAR("Pf_c"))
}


###################################################################
# PfSI Diagnostics
###################################################################

#' PfSI \code{Human} Method: PfSI Rapid Diagnostic Test
#'
#' Administer RDT to this human.
#'  * if infected: true positive is detected with probability \code{rdtSensPf}, see \code{\link{PfSI.Parameters}}
#'  * if susceptible: false positive is detected with probability \code{rdtSpecPf}, see \code{\link{PfSI.Parameters}}
#'
rdtTest_PfSI <- function(tEvent, PAR){
  if(private$Pathogens$get_infected()){
    return(runif(1) < self$get_PfSI_PAR("rdtSensPf"))
  } else {
    return(runif(1) < self$get_PfSI_PAR("rdtSpecPf"))
  }
}

#' PfSI \code{Human} Method: PfSI Light Microscopy Test
#'
#' Administer light microscopy to this human.
#'  * if infected: true positive is detected with probability \code{lmSensPf}, see \code{\link{PfSI.Parameters}}
#'  * if susceptible: false positive is detected with probability \code{lmSpecPf}, see \code{\link{PfSI.Parameters}}
#'
lmTest_PfSI <- function(tEvent, PAR){
  if(private$Pathogens$Pf$get_infected()){
    return(runif(1) < self$get_PfSI_PAR("lmSensPf"))
  } else {
    return(runif(1) < self$get_PfSI_PAR("lmSpecPf"))
  }
}
