###############################################################################
#       ____  _________ ____
#      / __ \/ __/ ___//  _/
#     / /_/ / /_ \__ \ / /
#    / ____/ __/___/ // /
#   /_/   /_/  /____/___/
#
#   MASH-MICRO
#   MICRO: PfSI Setup
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################


#' Initialize PfSI Pathogen Module for MICRO
#'
#' Generate a list of parameters PfSI_PAR in \code{\link{Human}} and public methods in \code{\link{Human}} for PfSI infection model; also defines public methods
#' in \code{\link{MosquitoFemale}} for PfSI infection model.
#'
#' @param overwrite overwrite existing methods and fields?
#' @param Pf_c 0.15; transmission efficiency: infected human to mosquito
#' @param Pf_b 0.55; transmission efficiency: infected mosquito to human
#' @param DurationPf 200; duration of infection (How many days does the infection last?)
#' @param LatentPf 10; latency (How many days after the infectious bite does the infection start?)
#' @param FeverPf 0.3; probability of fever
#' @param mnFeverPf 10; mean of Gaussian-distributed timing of fever incident (relative to start of the infection)
#' @param vrFeverPf 0.1; standard deviation of Gaussian-distributed timing of fever incident (relative to start of the infection)
#' @param TreatPf 0.5; probability of treatment
#' @param mnTreatPf 1; timing of treatment (relative to start of the fever)
#' @param mnChemoprophylaxisPf 32; Prophylaxis, time to susceptibility (duration of protection due to chemoprophylaxis)
#' @param PEProtectPf 0.99; proportion protected by PE vaccination (probability vaccination successful)
#' @param peBlockPf 1; proportion of infections blocked by PE vaccination
#' @param mnPEPf 270; mean duration of protection from PE vaccination
#' @param vrPEPf 50; standard deviation of protection from PE vaccination
#' @param GSProtectPf 1; proportion protected by GS vaccination (probability vaccination successful)
#' @param gsBlockPf 0.9; proportion of infections blocked by GS vaccination
#' @param mnGSPf 180; mean duration of protection from GS vaccination
#' @param vrGSPf 20; standard deviation of protection from GS vaccination
#' @param rdtSensPf 0.9; RDT sensitivity
#' @param rdtSpecPf 0.1; RDT specificity
#' @param lmSensPf 0.9; Light Microscopy sensitivity
#' @param lmSpecPf 0.1; Light Microscopy specificity
#' @return Defines a field (list) PfSI_PAR in \code{\link{HumanPop}} and public methods in \code{\link{Human}}
#' @examples
#' PfSI.MICRO.Setup()
#' @export
PfSI.MICRO.Setup <- function(

  overwrite = TRUE,

  ########################################
  #  Parameters
  ########################################

  # Transmission efficiency
  Pf_c   = 0.15,
  Pf_b   = 0.55,

  # Duration of Infection
  # (How many days does the infection last?)
  DurationPf = 200,

  # Latency:
  # (How many days after the infectious
  #  bite does the infection start?)
  LatentPf = 10,

  # Probability of Fever
  FeverPf = 0.3,

  # Timing of Fever Incident
  # (relative to start of the infection)
  mnFeverPf = 10,
  vrFeverPf = .1,

  # Probability of Treating
  TreatPf = .5,

  # Timing of Treatment
  # (relative to start of the fever)
  mnTreatPf = 1,

  # Prophylaxis, time to susceptibility
  mnChemoprophylaxisPf = 32,

  # Proportion Protected by PE Vaccination
  PEProtectPf = .99,

  # Proportion of infections Blocked
  peBlockPf = 1,
  mnPEPf = 270,
  vrPEPf = 50,

  # Proportion Protected by GS Vaccination
  GSProtectPf = 1,

  # Proportion of infections Blocked
  gsBlockPf = .9,
  mnGSPf = 180,
  vrGSPf = 20,

  #  RDT Probe and Accuracy
  rdtSensPf  = .9,
  rdtSpecPf  = .1,

  #  Light Microscopy Probe and Sensitivity
  lmSensPf  = .9,
  lmSpecPf  = .1

){

  cat("initializing PfSI Pathogen module\n",sep="")

  ###################################################################
  # PfSI MICRO Mosquito methods
  ###################################################################

  ###################################################################
  # Add PfSI Pathogen Object to 'MosquitoFemale' & 'MosquitoPopFemale' Class
  ###################################################################

  MosquitoFemale$set(which = "public",name = "probing",
            value = probing_PfSI,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "feeding",
            value = feeding_PfSI,
            overwrite = overwrite
  )

  MosquitoFemale$set(which = "public",name = "init_Pathogens",
            value = init_Pathogens_MosquitoFemale_PfSI,
            overwrite = overwrite
  )

  MosquitoPopFemale$set(which = "public",name = "init_Pathogens",
            value = init_Pathogens_MosquitoPopFemale_PfSI,
            overwrite = overwrite
  )

  ###################################################################
  # Add PfSI Parameters to 'HumanPop' Class
  ###################################################################

  # PfSI_PAR: list of PfSI parameters added to private field of 'HumanPop' class
  MASHmacro::HumanPop$set(which = "private",name = "PfSI_PAR",
            value = list(
              Pf_c   = Pf_c,
              Pf_b   = Pf_b,
              DurationPf = DurationPf,
              LatentPf = LatentPf,
              FeverPf = FeverPf,
              mnFeverPf = mnFeverPf,
              vrFeverPf = vrFeverPf,
              TreatPf = TreatPf,
              mnTreatPf = mnTreatPf,
              mnChemoprophylaxisPf = mnChemoprophylaxisPf,
              PEProtectPf = PEProtectPf,
              peBlockPf = peBlockPf,
              mnPEPf = mnPEPf,
              vrPEPf = vrPEPf,
              GSProtectPf = GSProtectPf,
              gsBlockPf = gsBlockPf,
              mnGSPf = mnGSPf,
              vrGSPf = vrGSPf,
              rdtSensPf = rdtSensPf,
              rdtSpecPf = rdtSpecPf,
              lmSensPf = lmSensPf,
              lmSpecPf = lmSpecPf
            ),
            overwrite = overwrite
  )

  # getter for PfSI_PAR: ix should be a character eg 'Pf_b'
  MASHmacro::Human$set(which = "public",name = "get_PfSI_PAR",
            value = MASHmacro:::Human_get_PfSI_PAR,
            overwrite = overwrite
  )

  MASHmacro::HumanPop$set(which = "public",name = "get_PfSI_PAR",
            value = MASHmacro:::HumanPop_get_PfSI_PAR,
            overwrite = overwrite
  )

  # set PfSI_PAR for a HumanPop; this is useful for simulating multiple populations with different parameter values
  MASHmacro::HumanPop$set(which = "public",name = "set_PfSI_PAR",
            value = MASHmacro:::HumanPop_set_PfSI_PAR,
            overwrite = overwrite
  )

  ###################################################################
  # Add PfSI Utilities to 'HumanPop' Class
  ###################################################################

  # PfID counter
  MASHmacro::HumanPop$set(which = "private",name = "PfID",
            value = 0L,
            overwrite = overwrite
  )

  # whenever a new liver-stage infection manifests in a human we increment the PfID counter and return the new PfID
  MASHmacro::HumanPop$set(which = "public",name = "increment_PfID",
            value = MASHmacro:::PfSI_increment_PfID,
            overwrite = overwrite
  )

  # initialize PfSI infections with patch parasite prevalence PfPR
  MASHmacro::HumanPop$set(which = "public",name = "init_PfSI",
            value = MASHmacro:::init_PfSI,
            overwrite = overwrite
  )

  # history getter
  MASHmacro::HumanPop$set(which = "public",name = "get_PfSI_history",
            value = MASHmacro:::HumanPop_get_PfSI_history,
            overwrite = overwrite
  )


  ###################################################################
  # Add PfSI Pathogen Object to 'Human' & 'HumanPop' Class
  ###################################################################

  MASHmacro::Human$set(which = "public",name = "set_humanPfSI",
            value = MASHmacro:::Human_set_humanPfSI,
            overwrite = overwrite
  )

  MASHmacro::HumanPop$set(which = "public",name = "set_humanPfSI",
            value = MASHmacro:::HumanPop_set_humanPfSI,
            overwrite = overwrite
  )

  ###################################################################
  # PfSI: Mosquito to Human infectious bite
  # Add methods to 'Human' Class
  ###################################################################

  # probeHost_PfSI:
  # arguments are tBite (time of bite)
  # mosquitoPfSI; the mosquitoPfSI R6 object passed from the biting mosquito
  MASHmacro::Human$set(which = "public",name = "probeHost_PfSI",
            value = MASHmacro:::probeHost_PfSI,
            overwrite = overwrite
  )

  # infectiousBite_PfSI
  MASHmacro::Human$set(which = "public",name = "infectiousBite_PfSI",
            value = MASHmacro:::infectiousBite_PfSI,
            overwrite = overwrite
  )

  ###################################################################
  # Add PfSI Timing Functions to 'Human' Class
  ###################################################################

  # Duration of Infection
  # (How many days does the infection last?)
  MASHmacro::Human$set(which = "public",name = "ttClearPf",
            value = MASHmacro:::PfSI_ttClearPf,
            overwrite = overwrite
  )

  # Latency:
  # (How many days after the infectious
  #  bite does the infection start?)
  MASHmacro::Human$set(which = "public",name = "ttInfectionPf",
            value = MASHmacro:::PfSI_ttInfectionPf,
            overwrite = overwrite
  )

  # Timing of Fever Incident
  # (relative to start of the infection)
  MASHmacro::Human$set(which = "public",name = "ttFeverPf",
            value = MASHmacro:::PfSI_ttFeverPf,
            overwrite = overwrite
  )

  # Timing of Treatment
  # (relative to start of the fever)
  MASHmacro::Human$set(which = "public",name = "ttTreatPf",
            value = MASHmacro:::PfSI_ttTreatPf,
            overwrite = overwrite
  )

  # Prophylaxis, time to susceptibility
  MASHmacro::Human$set(which = "public",name = "ttSusceptiblePf",
            value = MASHmacro:::PfSI_ttSusceptiblePf,
            overwrite = overwrite
  )

  # Duration of protection by PE Vaccination
  MASHmacro::Human$set(which = "public",name = "ttPEWanePf",
            value = MASHmacro:::PfSI_ttPEWanePf,
            overwrite = overwrite
  )

  # Duration of protection Blocked by GS Vaccination
  MASHmacro::Human$set(which = "public",name = "ttGSWanePf",
            value = MASHmacro:::PfSI_ttGSWanePf,
            overwrite = overwrite
  )


  ###################################################################
  # Add PfSI Events to 'Human' Class
  # 'XX' family of functions for human event queues
  ###################################################################


  ###################################################################
  # Start a PfSI Infection
  ###################################################################

  # add2Q_infectHumanPfSI
  MASHmacro::Human$set(which = "public",name = "add2Q_infectHumanPfSI",
            value = MASHmacro:::add2Q_infectHumanPfSI,
            overwrite = overwrite
  )

  # event_infectHumanPfSI: begin a PfSI infection
  MASHmacro::Human$set(which = "public",name = "event_infectHumanPfSI",
            value = MASHmacro:::event_infectHumanPfSI,
            overwrite = overwrite
  )

  # infectHumanPfSI
  MASHmacro::Human$set(which = "public",name = "infectHumanPfSI",
            value = MASHmacro:::infectHumanPfSI,
            overwrite = overwrite
  )

  ###################################################################
  # End an PfSI Infection
  ###################################################################

  # add2Q_endPfSI
  MASHmacro::Human$set(which = "public",name = "add2Q_endPfSI",
            value = MASHmacro:::add2Q_endPfSI,
            overwrite = overwrite
  )

  # event_endPfSI: end a PfSI infection
  MASHmacro::Human$set(which = "public",name = "event_endPfSI",
            value = MASHmacro:::event_endPfSI,
            overwrite = overwrite
  )

  # endPfSI
  MASHmacro::Human$set(which = "public",name = "endPfSI",
            value = MASHmacro:::endPfSI,
            overwrite = overwrite
  )

  ###################################################################
  # Fever
  ###################################################################

  MASHmacro::Human$set(which = "public",name = "add2Q_feverPfSI",
            value = MASHmacro:::add2Q_feverPfSI,
            overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "event_feverPfSI",
            value = MASHmacro:::event_feverPfSI,
            overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "feverPfSI",
            value = MASHmacro:::feverPfSI,
            overwrite = overwrite
  )

  ###################################################################
  # Treatment
  ###################################################################

  MASHmacro::Human$set(which = "public",name = "add2Q_treatPfSI",
            value = MASHmacro:::add2Q_treatPfSI,
            overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "event_treatPfSI",
            value = MASHmacro:::event_treatPfSI,
            overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "treatPfSI",
            value = MASHmacro:::treatPfSI,
            overwrite = overwrite
  )

  ###################################################################
  # End of Chemoprophylaxis
  ###################################################################

  MASHmacro::Human$set(which = "public",name = "add2Q_endprophylaxisPfSI",
            value = MASHmacro:::add2Q_endprophylaxisPfSI,
            overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "event_endprophylaxisPfSI",
            value = MASHmacro:::event_endprophylaxisPfSI,
            overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "endprophylaxisPfSI",
            value = MASHmacro:::endprophylaxisPfSI,
            overwrite = overwrite
  )

  ###################################################################
  # HUMAN PE vaccination functions
  ###################################################################

  # vaccination
  MASHmacro::Human$set(which = "public",name = "add2Q_pevaccinatePfSI",
            value = MASHmacro:::add2Q_pevaccinatePfSI,
            overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "event_pevaccinatePfSI",
            value = MASHmacro:::event_pevaccinatePfSI,
            overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "pevaccinatePfSI",
            value = MASHmacro:::pevaccinatePfSI,
            overwrite = overwrite
  )

  # waning protection
  MASHmacro::Human$set(which = "public",name = "add2Q_pewanePfSI",
            value = MASHmacro:::add2Q_pewanePfSI,
            overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "event_pewanePfSI",
            value = MASHmacro:::event_pewanePfSI,
            overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "pewanePfSI",
            value = MASHmacro:::pewanePfSI,
            overwrite = overwrite
  )

  ###################################################################
  # HUMAN GS vaccination functions
  ###################################################################

  # vaccination
  MASHmacro::Human$set(which = "public",name = "add2Q_gsvaccinatePfSI",
            value = MASHmacro:::add2Q_gsvaccinatePfSI,
            overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "event_gsvaccinatePfSI",
            value = MASHmacro:::event_gsvaccinatePfSI,
            overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "gsvaccinatePfSI",
            value = MASHmacro:::gsvaccinatePfSI,
            overwrite = overwrite
  )

  # waning protection
  MASHmacro::Human$set(which = "public",name = "add2Q_gswanePfSI",
            value = MASHmacro:::add2Q_gswanePfSI,
            overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "event_gswanePfSI",
            value = MASHmacro:::event_gswanePfSI,
            overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "gswanePfSI",
            value = MASHmacro:::gswanePfSI,
            overwrite = overwrite
  )

  ###################################################################
  # PfSI Diagnostics
  ###################################################################

  MASHmacro::Human$set(which = "public",name = "rdtTest_PfSI",
            value = MASHmacro:::rdtTest_PfSI,
            overwrite = overwrite
  )

  MASHmacro::Human$set(which = "public",name = "lmTest_PfSI",
            value = MASHmacro:::lmTest_PfSI,
            overwrite = overwrite
  )

}
