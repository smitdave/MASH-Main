#################################################################
#
#   MASH
#   R6-ified
#   PfMOI Setup
#   David Smith, Hector Sanchez, Sean Wu
#   June 9, 2017
#
#################################################################

#' Initialize PfMOI MODULE
#'
#' Generate a list of parameters PfMOI_PAR in \code{\link{Human}} and public methods in \code{\link{Human}} for PfSI infection model; also defines public methods
#' in \code{\link{MicroMosquitoFemale}} for PfMOI infection model.
#'
#' @param overwrite overwrite existing methods and fields?
#' @param interaction independent clearance or pathogen interaction model for calculating infection clearance waiting times (if \code{FALSE} use \code{\link{PfMOI_ttClearPf_Independent}}, if \code{TRUE} use \code{\link{PfMOI_ttClearPf_Interaction}})
#' @param InteractionPf if using pathogen interaction model of clearance this parameter control strength of interaction
#'  * 0: no interaction (reduces to independent clearance)
#'  * <0: facilitation against host immune system (slower clearance rate than independent queueing)
#'  * 0<: competition for host resources (faster clearance rate than independent queueing)
#'  *  See \url{https://doi.org/10.1186/1475-2875-8-87} for details of queueing model with pathogen interaction.
#' @param MosyMaxI maximum number of clonal variants passed in single mosquito to human transmission event (set to \code{-1L} for unlimited)
#' @param HumanMaxI maximum number of clonal variants passed in single human to mosquito transmission event (set to \code{-1L} for unlimited)
#' @param Pf_c infected human to mosquito transmission efficiency
#' @param Pf_b infected mosquito to human transmission efficiency
#' @param DurationPf duration of infection (assuming clonal variants are cleared independently)
#' @param LatentPf latency (How many days after the infectious bite does the infection start?)
#' @param FeverPf probability of fever
#' @param mnFeverPf mean of timing of fever incident relative to start of PfMOI infection (log-normally distributed, mean on natural scale)
#' @param vrFeverPf standard deviation of timing of fever incident relative to start of PfMOI infection (log-normally distributed, standard deviation on natural scale)
#' @param TreatPf probability of treatment after fever incident
#' @param mnTreatPf average waiting time from fever to treatment (exponentially distributed)
#' @param mnChemoprophylaxisPf constant period of proteection from chemoprophylaxis
#' @param PEProtectPf proportion protected by PE vaccination (probability vaccination successful)
#' @param peBlockPf proportion of infections blocked by PE vaccination
#' @param mnPEPf mean duration of protection from PE vaccination
#' @param vrPEPf standard deviation of protection from PE vaccination
#' @param GSProtectPf proportion protected by GS vaccination (probability vaccination successful)
#' @param gsBlockPf proportion of infections blocked by GS vaccination
#' @param mnGSPf mean duration of protection from GS vaccination
#' @param vrGSPf standard deviation of protection from GS vaccination
#' @param rdtSensPf RDT sensitivity
#' @param rdtSpecPf RDT specificity
#' @param lmSensPf Light Microscopy sensitivity
#' @param lmSpecPf Light Microscopy specificity
#' @return Defines a field (list) PfSI_PAR in \code{\link{HumanPop}} and public methods in \code{\link{Human}}
#' @md
#' @export
PfMOI.Setup <- function(

  overwrite = TRUE,
  interaction = FALSE,
  InteractionPf = 0,

  ########################################
  #  Transmission & Infection
  ########################################
  MosyMaxI =  1L,
  HumanMaxI = 1L,
  Pf_c = 0.15,
  Pf_b = 0.55,

  ########################################
  #  Timing
  ########################################

  DurationPf = 200, # Duration of infection
  LatentPf = 10, # Latency

  # Fever
  FeverPf = 0.3,
  mnFeverPf = 10,
  vrFeverPf = .1,

  # Treatment
  TreatPf = 0.5,
  mnTreatPf = 3,

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

  #  Diagnostic Parameters
  rdtSensPf = .9,
  rdtSpecPf = .1,
  lmSensPf = 0.9,
  lmSpecPf = 0.1


){

  message(paste0("initializing PfMOI PATHOGEN module"))

  ###################################################################
  # PfMOI MICRO Mosquito methods
  ###################################################################

  ###################################################################
  # Add PfMOI Pathogen Object to 'MicroMosquitoFemale' & 'MicroMosquitoPopFemale' Class
  ###################################################################

  MicroMosquitoFemale$set(which = "public",name = "probing",
            value = probing_PfMOI,
            overwrite = overwrite
  )

  MicroMosquitoFemale$set(which = "public",name = "feeding",
            value = feeding_PfMOI,
            overwrite = overwrite
  )

  MicroMosquitoFemale$set(which = "public",name = "init_Pathogens",
            value = init_Pathogens_MicroMosquitoFemale_PfMOI,
            overwrite = overwrite
  )

  MicroMosquitoPopFemale$set(which = "public",name = "init_Pathogens",
            value = init_Pathogens_MicroMosquitoPopFemale_PfMOI,
            overwrite = overwrite
  )

  ###################################################################
  # Add PfMOI Parameters to 'HumanPop' Class
  ###################################################################

  # PfMOI_PAR: list of PfMOI parameters added to private field of 'HumanPop' class
  HumanPop$set(which = "private",name = "PfMOI_PAR",
            value = list(
              InteractionPf = InteractionPf,
              MosyMaxI =  MosyMaxI,
              HumanMaxI =  HumanMaxI,
              Pf_c = Pf_c,
              Pf_b = Pf_b,
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

  # Human getter for PfMOI_PAR: ix should be a character eg 'Pf_b'
  Human$set(which = "public",name = "get_PfMOI_PAR",
            value = Human_get_PfMOI_PAR,
            overwrite = overwrite
  )

  # HumanPop getter for PfMOI_PAR
  HumanPop$set(which = "public",name = "get_PfMOI_PAR",
            value = HumanPop_get_PfMOI_PAR,
            overwrite = overwrite
  )

  # HumanPop setter for PfMOI_PAR
  HumanPop$set(which = "public",name = "set_PfMOI_PAR",
            value = HumanPop_set_PfMOI_PAR,
            overwrite = overwrite
  )


  #################################################################
  # PfMOI Utilties for 'HumanPop' and 'Human'
  #################################################################

  # PfID counter
  HumanPop$set(which = "private",name = "PfID",
            value = 0L,
            overwrite = overwrite
  )

  # increment_PfID when new liver-stage infection begins
  HumanPop$set(which = "public",name = "increment_PfID",
            value = PfMOI_increment_PfID,
            overwrite = overwrite
  )

  # init_MICRO_PfMOI: init infections for SimBitePfMOI or MICRO
  HumanPop$set(which = "public",name = "init_MICRO_PfMOI",
            value = init_MICRO_PfMOI,
            overwrite = overwrite
  )

  # initialize PfMOI infections with patch parasite mean MOI PfMOI
  HumanPop$set(which = "public",name = "init_MACRO_PfMOI",
            value = init_MACRO_PfMOI,
            overwrite = overwrite
  )

  # initialize PfMOI infections for MACRO
  MacroTile$set(which = "public",name = "init_PfMOI",
            value = init_MacroTile_PfMOI,
            overwrite = overwrite
  )

  # get PfMOI history
  HumanPop$set(which = "public",name = "get_PfMOI_history",
            value = HumanPop_get_PfMOI_history,
            overwrite = overwrite
  )

  ###################################################################
  # Add PfMOI Pathogen Object to 'Human' & 'HumanPop' Class
  ###################################################################

  Human$set(which = "public",name = "set_humanPfMOI",
            value = Human_set_humanPfMOI,
            overwrite = overwrite
  )

  HumanPop$set(which = "public",name = "set_humanPfMOI",
            value = HumanPop_set_humanPfMOI,
            overwrite = overwrite
  )


  #################################################################
  # PfMOI Event Timing
  #################################################################

  if(interaction){
    Human$set(which = "public",name = "ttClearPf",
              value = PfMOI_ttClearPf_Interaction,
              overwrite = overwrite
    )
  } else {
    Human$set(which = "public",name = "ttClearPf",
              value = PfMOI_ttClearPf_Independent,
              overwrite = overwrite
    )
  }

  Human$set(which = "public",name = "ttInfectionPf",
            value = PfMOI_ttInfectionPf,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "ttFeverPf",
            value = PfMOI_ttFeverPf,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "ttTreatPf",
            value = PfMOI_ttTreatPf,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "ttSusceptiblePf",
            value = PfMOI_ttSusceptiblePf,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "ttPEWanePf",
            value = PfMOI_ttPEWanePf,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "ttGSWanePf",
            value = PfMOI_ttGSWanePf,
            overwrite = overwrite
  )


  #################################################################
  # PfMOI Diagnostics
  #################################################################

  Human$set(which = "public",name = "rdtTest_PfMOI",
            value = rdtTest_PfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "lmTest_PfMOI",
            value = lmTest_PfMOI,
            overwrite = overwrite
  )


  ###################################################################
  # PfMOI: Mosquito to Human infectious bite
  # Add methods to 'Human' Class
  ###################################################################

  # Human$set(which = "public",name = "probeHost_PfMOI",
  #           value = probeHost_PfMOI,
  #           overwrite = overwrite
  # )

  Human$set(which = "public",name = "infectiousBite_PfMOI",
            value = infectiousBite_PfMOI,
            overwrite = overwrite
  )


  ###################################################################
  # PfMOI: Human to Mosquito infectious bite
  # Add methods to 'MicroMosquitoFemale' Classe
  ###################################################################

  #  add when MICRO mosquitoes exist

  ###################################################################
  # PFMOI Events
  ###################################################################

  # Start a PfMOI Infection
  Human$set(which = "public",name = "add2Q_infectHumanPfMOI",
            value = add2Q_infectHumanPfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_infectHumanPfMOI",
            value = event_infectHumanPfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "infectHumanPfMOI",
            value = infectHumanPfMOI,
            overwrite = overwrite
  )

  # End a PfMOI Infection
  Human$set(which = "public",name = "add2Q_endPfMOI",
            value = add2Q_endPfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_endPfMOI",
            value = event_endPfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "endPfMOI",
            value = endPfMOI,
            overwrite = overwrite
  )

  # PfMOI Fever
  Human$set(which = "public",name = "add2Q_feverPfMOI",
            value = add2Q_feverPfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_feverPfMOI",
            value = event_feverPfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "feverPfMOI",
            value = feverPfMOI,
            overwrite = overwrite
  )

  # PfMOI Treatment
  Human$set(which = "public",name = "add2Q_treatPfMOI",
            value = add2Q_treatPfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_treatPfMOI",
            value = event_treatPfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "treatPfMOI",
            value = treatPfMOI,
            overwrite = overwrite
  )

  # PfMOI End of Chemoprophylaxis
  Human$set(which = "public",name = "add2Q_endprophylaxisPfMOI",
            value = add2Q_endprophylaxisPfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_endprophylaxisPfMOI",
            value = event_endprophylaxisPfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "endprophylaxisPfMOI",
            value = endprophylaxisPfMOI,
            overwrite = overwrite
  )

  # PfMOI PE vaccination
  Human$set(which = "public",name = "add2Q_pevaccinatePfMOI",
            value = add2Q_pevaccinatePfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_pevaccinatePfMOI",
            value = event_pevaccinatePfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "pevaccinatePfMOI",
            value = pevaccinatePfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "add2Q_pewanePfMOI",
            value = add2Q_pewanePfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_pewanePfMOI",
            value = event_pewanePfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "pewanePfMOI",
            value = pewanePfMOI,
            overwrite = overwrite
  )

  # PfMOI GS vaccination
  Human$set(which = "public",name = "add2Q_gsvaccinatePfMOI",
            value = add2Q_gsvaccinatePfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_gsvaccinatePfMOI",
            value = event_gsvaccinatePfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "gsvaccinatePfMOI",
            value = gsvaccinatePfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "add2Q_gswanePfMOI",
            value = add2Q_gswanePfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "event_gswanePfMOI",
            value = event_gswanePfMOI,
            overwrite = overwrite
  )

  Human$set(which = "public",name = "gswanePfMOI",
            value = gswanePfMOI,
            overwrite = overwrite
  )



}
