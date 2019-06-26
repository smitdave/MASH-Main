################################################################################
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   PfSI parameters module (parameters for inst/include/Parameters.hpp object)
#
#   Sean Wu
#   December 2018
#
################################################################################


#' PfSI Parameters
#'
#' Parameters for the 'PfSI' model of human infection dynamics
#' (see inst/include/Parameters.hpp for where parameters are stored)
#'
#' @param Pf_c human to mosquito transmission efficiency
#' @param Pf_b mosquito to human transmission efficiency
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
pfsi_parameters <- function(
    Pf_c   = 0.15,
    Pf_b   = 0.55,
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

    # args <- as.list(match.call.defaults())
    # args <- args[-1]
    # out <- vector("list",length(args))
    # for(i in 1:length(args)){
    #   out[[i]] <- pars_obj(val = args[[i]],name = names(args[i]), fixtype = 1L)
    # }

    out <- list()
    out$Pf_c   = pars_obj(val = Pf_c,name = "Pf_c", fixtype = 1L)
    out$Pf_b   = pars_obj(val = Pf_b,name = "Pf_b", fixtype = 1L)
    out$DurationPf = pars_obj(val = DurationPf,name = "DurationPf", fixtype = 1L)
    out$LatentPf = pars_obj(val = LatentPf,name = "LatentPf", fixtype = 1L)
    out$FeverPf = pars_obj(val = FeverPf,name = "FeverPf", fixtype = 1L)
    out$mnFeverPf = pars_obj(val = mnFeverPf,name = "mnFeverPf", fixtype = 1L)
    out$vrFeverPf = pars_obj(val = vrFeverPf,name = "vrFeverPf", fixtype = 1L)
    out$TreatPf = pars_obj(val = TreatPf,name = "TreatPf", fixtype = 1L)
    out$mnTreatPf = pars_obj(val = mnTreatPf,name = "mnTreatPf", fixtype = 1L)
    out$mnChemoprophylaxisPf = pars_obj(val = mnChemoprophylaxisPf,name = "mnChemoprophylaxisPf", fixtype = 1L)
    out$PEProtectPf = pars_obj(val = PEProtectPf,name = "PEProtectPf", fixtype = 1L)
    out$peBlockPf = pars_obj(val = peBlockPf,name = "peBlockPf", fixtype = 1L)
    out$mnPEPf = pars_obj(val = mnPEPf,name = "mnPEPf", fixtype = 1L)
    out$vrPEPf = pars_obj(val = vrPEPf,name = "vrPEPf", fixtype = 1L)
    out$GSProtectPf = pars_obj(val = GSProtectPf,name = "GSProtectPf", fixtype = 1L)
    out$gsBlockPf = pars_obj(val = gsBlockPf,name = "gsBlockPf", fixtype = 1L)
    out$mnGSPf = pars_obj(val = mnGSPf,name = "mnGSPf", fixtype = 1L)
    out$vrGSPf = pars_obj(val = vrGSPf,name = "vrGSPf", fixtype = 1L)

    return(out)
}
