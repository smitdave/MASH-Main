#################################################################
#
#   MASH
#   R6-ified
#   PfSI Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   May 21, 2017
#
#################################################################

#' Initialize PfSI Module Parameters
#'
#' This is mostly used to modify PfSI parameters for different human populations after creation by using \code{set_PfSI_PAR} method of HumanPop
#'
#' @param Pf_c 0.15; infected human to mosquito transmission efficiency
#' @param Pf_b 0.55; infected mosquito to human transmission efficiency
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
#' @return return a list
#' @examples
#' PfSI.Parameters()
#' @export
PfSI.Parameters <- function(

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

  list(
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
  )

}
