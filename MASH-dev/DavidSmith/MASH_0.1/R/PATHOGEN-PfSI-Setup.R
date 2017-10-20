#################################################################
#
#   MASH
#   R6-ified
#   PfSI Setup
#   David Smith, Hector Sanchez, Sean Wu
#   May 21, 2017
#
#################################################################


#' Initialize PfSI MODULE
#'
#' Generate a list of parameters PfSI_PAR in \code{\link{Human}} and public methods in \code{\link{Human}} for PfSI infection model; also defines public methods
#' in \code{\link{MicroMosquitoFemale}} for PfSI infection model.
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
#' PfSI.Setup()
#' @export
PfSI.Setup <- function(

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

  message(paste0("initializing PfSI PATHOGEN module"))

  ###################################################################
  # PfSI MICRO Mosquito methods
  ###################################################################

  ###################################################################
  # Add PfSI Pathogen Object to 'MicroMosquitoFemale' & 'MicroMosquitoPopFemale' Class
  ###################################################################

  MicroMosquitoFemale$set(which = "public",name = "probing",
            value = probing_PfSI,
            overwrite = overwrite
  )

  MicroMosquitoFemale$set(which = "public",name = "feeding",
            value = feeding_PfSI,
            overwrite = overwrite
  )

  MicroMosquitoFemale$set(which = "public",name = "init_Pathogens",
            value = init_Pathogens_MicroMosquitoFemale_PfSI,
            overwrite = overwrite
  )

  MicroMosquitoPopFemale$set(which = "public",name = "init_Pathogens",
            value = init_Pathogens_MicroMosquitoPopFemale_PfSI,
            overwrite = overwrite
  )

  ###################################################################
  # Add PfSI Parameters to 'HumanPop' Class
  ###################################################################

  # PfSI_PAR: list of PfSI parameters added to private field of 'HumanPop' class
  HumanPop$set(which = "private",name = "PfSI_PAR",
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
  Human$set(which = "public",name = "get_PfSI_PAR",
            value = Human_get_PfSI_PAR,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "get_PfSI_PAR",
            value = HumanPop_get_PfSI_PAR,
            overwrite = overwrite
  )

  # set PfSI_PAR for a HumanPop; this is useful for simulating multiple populations with different parameter values
  HumanPop$set(which = "public",name = "set_PfSI_PAR",
            value = HumanPop_set_PfSI_PAR,
            overwrite = overwrite
  )

  ###################################################################
  # Add PfSI Utilities to 'HumanPop' Class
  ###################################################################

  # PfID counter
  HumanPop$set(which = "private",name = "PfID",
            value = 0L,
            overwrite = overwrite
  )

  # whenever a new liver-stage infection manifests in a human we increment the PfID counter and return the new PfID
  HumanPop$set(which = "public",name = "increment_PfID",
            value = PfSI_increment_PfID,
            overwrite = overwrite
  )

  # initialize PfSI infections with patch parasite prevalence PfPR
  HumanPop$set(which = "public",name = "init_MICRO_PfSI",
            value = init_MICRO_PfSI,
            overwrite = overwrite
  )

  # initialize PfSI infections with patch parasite prevalence PfPR
  HumanPop$set(which = "public",name = "init_MACRO_PfSI",
            value = init_MACRO_PfSI,
            overwrite = overwrite
  )

  # initialize PfSI infections for MACRO
  MacroTile$set(which = "public",name = "init_PfSI",
            value = init_MacroTile_PfSI,
            overwrite = overwrite
  )

  # history getter
  HumanPop$set(which = "public",name = "get_PfSI_history",
            value = HumanPop_get_PfSI_history,
            overwrite = overwrite
  )


  ###################################################################
  # Add PfSI Pathogen Object to 'Human' & 'HumanPop' Class
  ###################################################################

  Human$set(which = "public",name = "set_humanPfSI",
            value = Human_set_humanPfSI,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "set_humanPfSI",
            value = HumanPop_set_humanPfSI,
            overwrite = overwrite
  )

  ###################################################################
  # PfSI: Mosquito to Human infectious bite
  # Add methods to 'Human' Class
  ###################################################################

  # probeHost_PfSI:
  # arguments are tBite (time of bite)
  # mosquitoPfSI; the mosquitoPfSI R6 object passed from the biting mosquito
  Human$set(which = "public",name = "probeHost_PfSI",
            value = probeHost_PfSI,
            overwrite = overwrite
  )

  # infectiousBite_PfSI
  Human$set(which = "public",name = "infectiousBite_PfSI",
            value = infectiousBite_PfSI,
            overwrite = overwrite
  )

  ###################################################################
  # Add PfSI Timing Functions to 'Human' Class
  ###################################################################

  # Duration of Infection
  # (How many days does the infection last?)
  Human$set(which = "public",name = "ttClearPf",
            value = PfSI_ttClearPf,
            overwrite = overwrite
  )

  # Latency:
  # (How many days after the infectious
  #  bite does the infection start?)
  Human$set(which = "public",name = "ttInfectionPf",
            value = PfSI_ttInfectionPf,
            overwrite = overwrite
  )

  # Timing of Fever Incident
  # (relative to start of the infection)
  Human$set(which = "public",name = "ttFeverPf",
            value = PfSI_ttFeverPf,
            overwrite = overwrite
  )

  # Timing of Treatment
  # (relative to start of the fever)
  Human$set(which = "public",name = "ttTreatPf",
            value = PfSI_ttTreatPf,
            overwrite = overwrite
  )

  # Prophylaxis, time to susceptibility
  Human$set(which = "public",name = "ttSusceptiblePf",
            value = PfSI_ttSusceptiblePf,
            overwrite = overwrite
  )

  # Duration of protection by PE Vaccination
  Human$set(which = "public",name = "ttPEWanePf",
            value = PfSI_ttPEWanePf,
            overwrite = overwrite
  )

  # Duration of protection Blocked by GS Vaccination
  Human$set(which = "public",name = "ttGSWanePf",
            value = PfSI_ttGSWanePf,
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
  Human$set(which = "public",name = "add2Q_infectHumanPfSI",
            value = add2Q_infectHumanPfSI,
            overwrite = overwrite
  )

  # event_infectHumanPfSI: begin a PfSI infection
  Human$set(which = "public",name = "event_infectHumanPfSI",
            value = event_infectHumanPfSI,
            overwrite = overwrite
  )

  # infectHumanPfSI
  Human$set(which = "public",name = "infectHumanPfSI",
            value = infectHumanPfSI,
            overwrite = overwrite
  )

  ###################################################################
  # End an PfSI Infection
  ###################################################################

  # add2Q_endPfSI
  Human$set(which = "public",name = "add2Q_endPfSI",
            value = add2Q_endPfSI,
            overwrite = overwrite
  )

  # event_endPfSI: end a PfSI infection
  Human$set(which = "public",name = "event_endPfSI",
            value = event_endPfSI,
            overwrite = overwrite
  )

  # endPfSI
  Human$set(which = "public",name = "endPfSI",
            value = endPfSI,
            overwrite = overwrite
  )

  ###################################################################
  # Fever
  ###################################################################

  Human$set(which = "public",name = "add2Q_feverPfSI",
            value = add2Q_feverPfSI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_feverPfSI",
            value = event_feverPfSI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "feverPfSI",
            value = feverPfSI,
            overwrite = overwrite
  )

  ###################################################################
  # Treatment
  ###################################################################

  Human$set(which = "public",name = "add2Q_treatPfSI",
            value = add2Q_treatPfSI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_treatPfSI",
            value = event_treatPfSI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "treatPfSI",
            value = treatPfSI,
            overwrite = overwrite
  )

  ###################################################################
  # End of Chemoprophylaxis
  ###################################################################

  Human$set(which = "public",name = "add2Q_endprophylaxisPfSI",
            value = add2Q_endprophylaxisPfSI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_endprophylaxisPfSI",
            value = event_endprophylaxisPfSI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "endprophylaxisPfSI",
            value = endprophylaxisPfSI,
            overwrite = overwrite
  )

  ###################################################################
  # HUMAN PE vaccination functions
  ###################################################################

  # vaccination
  Human$set(which = "public",name = "add2Q_pevaccinatePfSI",
            value = add2Q_pevaccinatePfSI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_pevaccinatePfSI",
            value = event_pevaccinatePfSI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "pevaccinatePfSI",
            value = pevaccinatePfSI,
            overwrite = overwrite
  )

  # waning protection
  Human$set(which = "public",name = "add2Q_pewanePfSI",
            value = add2Q_pewanePfSI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_pewanePfSI",
            value = event_pewanePfSI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "pewanePfSI",
            value = pewanePfSI,
            overwrite = overwrite
  )

  ###################################################################
  # HUMAN GS vaccination functions
  ###################################################################

  # vaccination
  Human$set(which = "public",name = "add2Q_gsvaccinatePfSI",
            value = add2Q_gsvaccinatePfSI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_gsvaccinatePfSI",
            value = event_gsvaccinatePfSI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "gsvaccinatePfSI",
            value = gsvaccinatePfSI,
            overwrite = overwrite
  )

  # waning protection
  Human$set(which = "public",name = "add2Q_gswanePfSI",
            value = add2Q_gswanePfSI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_gswanePfSI",
            value = event_gswanePfSI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "gswanePfSI",
            value = gswanePfSI,
            overwrite = overwrite
  )

  ###################################################################
  # PfSI Diagnostics
  ###################################################################

  Human$set(which = "public",name = "rdtTest_PfSI",
            value = rdtTest_PfSI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "lmTest_PfSI",
            value = lmTest_PfSI,
            overwrite = overwrite
  )

}
