#################################################################
#
#   MASH
#   R6-ified
#   PfMOI Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   June 9, 2017
#
#################################################################

#' Initialize PfMOI Module Parameters
#'
#' This is mostly used to modify PfMOI parameters for different human populations after creation by using \code{set_PfMOI_PAR} method of \code{\link{HumanPop}}
#'
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
#' @return return a list
#' @examples
#' PfMOI.Parameters()
#' @md
#' @export
PfMOI.Parameters <- function(

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

    list(
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
      )

}
