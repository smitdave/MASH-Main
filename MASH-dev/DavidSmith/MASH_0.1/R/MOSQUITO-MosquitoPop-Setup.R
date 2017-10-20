#################################################################
#
#   MASH
#   MICRO Mosquito Populations
#   Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   July 19, 2017
#
#################################################################

#' Setup MICRO Mosquito Population & M-BITES Module for a \code{\link{MicroTile}}
#'
#' Generate a list of parameters to initialize \code{\link{MicroMosquitoPopFemale}} and (optionally) \code{\link{MicroMosquitoPopMale}} for initialization of
#' mosquito populations in a microsimulation tile \code{\link{MicroTile}}. For MICRO simulations, this function automatically interfaces with the necessary M-BITES setup functions.
#'
#' @section MBITES-Cohort:
#'  For Cohort simulations the details of the population can be set to arbitrary values as they will be overwritten by the following functions.
#'  For more details on using MBITES-Cohort to fit EL4P, see \code{\link{set_FemalePop_MicroTile}}.
#'  * \code{\link{mbitesBRO_cohort_simCohort}}
#'  * others.
#'
#'
#' @param cohort run the chosen M-BITES module in Cohort mode
#' @param module which M-BITES module to use (must be a character in "BRO","BROM","BROS","BROMS","FULL")
#' @param aquaModule which Aquatic Ecology module to use (must be a character in "emerge", "EL4P")
#' @param N_female number of female mosquitoes
#' @param N_male number of male mosquitoes
#' @param time initial time to start populations (typically 0)
#' @param ix_female vector of starting location indices for females (indices of \code{\link{AquaticSite}} they emerge from)
#' @param ix_male vector of starting location indices for males (indices of \code{\link{AquaticSite}} they emerge from)
#' @param genotype_female vector of genotpes for females
#' @param genotype_male vector of genotpes for males
#' @param batchSize passed to \code{\link{MBITES.Generic.Setup}}
#' @param eggMatT passed to \code{\link{MBITES.Generic.Setup}}
#' @param ... additional named parameters to be passed to the specific M-BITES parameters function
#'  * MBITES-BRO: pass to \code{\link{MBITES.BRO.Parameters}}
#' @return return a list
#' @md
#' @export
MicroMosquitoPop.Setup <- function(
    cohort = FALSE,
    module,
    aquaModule,
    N_female,
    N_male = NULL,
    time = 0,
    ix_female,
    ix_male = NULL,
    genotype_female,
    genotype_male = NULL,
    batchSize = "bms",
    eggMatT = "off",
    ...
  ){

    if(cohort & aquaModule=="EL4P"){stop("cannot run MBITES-Cohort with EL4P Aquatic Ecology")}

    MosquitoPop_PAR = list()

    MosquitoPop_PAR$module = module
    MosquitoPop_PAR$N_female = N_female
    MosquitoPop_PAR$N_male = N_male
    MosquitoPop_PAR$time = time
    MosquitoPop_PAR$ix_female = ix_female
    MosquitoPop_PAR$ix_male = ix_male
    MosquitoPop_PAR$genotype_female = genotype_female
    MosquitoPop_PAR$genotype_male = genotype_male

    # make MBITES parameters
    MosquitoPop_PAR$MBITES_PAR = switch(module,
        BRO = {MBITES.BRO.Parameters(...)},
        BROS = {print("havent written")},
        BROM = {print("havent written")},
        BROMS = {print("havent written")},
        FULL = {print("havent written")},
        {stop("unrecognized M-BITES module")}
      )

    # set up M-BITES for mosquito pop classes

    # MBITES-Cohort
    if(cohort){
      switch(module,
          BRO = {
            # setup generic M-BITES methods
            MBITES.Generic.Setup(overwrite = TRUE, batchSize = batchSize, eggMatT = eggMatT)
            # setup M-BITES BRO methods
            MBITES.BRO.Cohort.Setup(overwrite = TRUE)
            # set parameters
            MicroMosquitoPopFemale$set(which = "private",name = "MBITES_PAR",
                                    value = MosquitoPop_PAR$MBITES_PAR,
                                    overwrite = TRUE
            )
          },
          BROS = {print("havent written")},
          BROM = {print("havent written")},
          BROMS = {print("havent written")},
          FULL = {print("havent written")},
          {stop("unrecognized M-BITES Cohort module")}
        )
    # MBITES
    } else {
      switch(module,
          BRO = {
            # setup generic M-BITES methods
            MBITES.Generic.Setup(overwrite = TRUE, batchSize = batchSize, eggMatT = eggMatT)
            # setup M-BITES BRO methods
            MBITES.BRO.Setup(overwrite = TRUE, aquaModule = aquaModule)
            # set parameters
            MicroMosquitoPopFemale$set(which = "private",name = "MBITES_PAR",
                                    value = MosquitoPop_PAR$MBITES_PAR,
                                    overwrite = TRUE
            )
          },
          BROS = {print("havent written")},
          BROM = {print("havent written")},
          BROMS = {print("havent written")},
          FULL = {print("havent written")},
          {stop("unrecognized M-BITES module")}
        )
    }

    return(MosquitoPop_PAR)
}
