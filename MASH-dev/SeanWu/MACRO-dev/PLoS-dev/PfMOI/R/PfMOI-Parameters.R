################################################################################
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   PfMOI parameters module (parameters for inst/include/Parameters.hpp object)
#
#   Sean Wu (slwu89@berkeley.edu)
#   August 2019
#
################################################################################


#' PfMOI Parameters
#'
#' Parameters for the 'PfMOI' model of human infection dynamics
#' (see inst/include/Parameters.hpp for where parameters are stored)
#'
#' @param Pf_c human to mosquito transmission efficiency
#' @param Pf_b mosquito to human transmission efficiency
#' @param sigmaPf strength of interaction between broods (0 < sigmaPf < 1: facilitation; slower clearance than indepdence, sigmaPf > 1: competition; faster clearance than indepdence)
#' @param DurationPf duration of infection
#' @param LatentPf duration of latent stage
#' @param FeverPf probability of fever upon infection
#' @param mnFeverPf mean of time to fever (Gaussian)
#' @param vrFeverPf sd of time to fever (Gaussian)
#' @param TreatPf probability of seeking treatment after fever
#' @param mnTreatPf time to treatment
#' @param mnChemoprophylaxisPf duration of chemoprophylactic protection
#' @param PEProtectPf vaccination efficacy (probability of effect)
#' @param peBlockPf vaccine effectiveness (reduction in probability of transmission 'b' given challenge)
#' @param mnPEPf mean duration of protection from PE vaccination (Gaussian)
#' @param vrPEPf standard deviation of protection from PE vaccination (Gaussian)
#' @param GSProtectPf vaccination efficacy (probability of effect)
#' @param gsBlockPf vaccine effectiveness (reduction in probability of transmission 'c' given challenge)
#' @param mnGSPf mean duration of protection from GS vaccination (Gaussian)
#' @param vrGSPf standard deviation of protection from GS vaccination (Gaussian)
#'
#' @export
pfmoi_parameters <- function(
    Pf_c   = 0.15,
    Pf_b   = 0.55,
    sigmaPf = 1,
    DurationPf = 200,
    LatentPf = 10,
    FeverPf = 0.3,
    mnFeverPf = 10,
    vrFeverPf = .1,
    TreatPf = .5,
    mnTreatPf = 1,
    mnChemoprophylaxisPf = 32,
    PEProtectPf = .99,
    peBlockPf = 1,
    mnPEPf = 270,
    vrPEPf = 50,
    GSProtectPf = 1,
    gsBlockPf = .9,
    mnGSPf = 180,
    vrGSPf = 20
  ){
  list(
    Pf_c = Pf_c,
    Pf_b = Pf_b,
    sigmaPf = sigmaPf,
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
    vrGSPf = vrGSPf
  )
}
