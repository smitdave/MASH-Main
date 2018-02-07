#################################################################
#
#   MASH: Testing routines for MACRO
#   qsub - cluster usage test
#   Call using rdock Test-MACRO-Emerge-qsub.R
#   Outputs a pfsiHist data object and saves it as .rds
#
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   Sean Wu
#   May 25, 2017
#   Daniel Citron
#   November 28, 2017
#
#################################################################

#################################################################
#     _____   __ _____ _____
#    |  __ \ / _/ ____|_   _|
#    | |__) | || (___   | |
#    |  ___/|  _\___ \  | |
#    | |    | | ____) |_| |_
#    |_|    |_||_____/|_____|
#
#################################################################

rm(list=ls())
gc()
library(MASH, lib.loc = "/ihme/malaria_modeling/dtcitron/Rlibs/")

# This part is what takes in the seed as an argument when called from the command line
seed <- as.integer(commandArgs()[6])
if (is.na(seed)) {seed <- 42}
print(seed)
set.seed(seed)


# initialize classes for MACRO
MACRO.Humans.Setup(pathogenModule = "PfSI")

# setup class methods for PfSI and SimBite modules
# MACRO relies on SimBitePfSI module for adding bites to humans
PfSI.Setup(TreatPf = 0.0)
SimBitePfSI.Setup()

# MACRO Patch initialization
MACRO.Patch.Emerge.Setup() # 'Emerge' model

# patch parameters
nPatch = 10
tileParameters = MACRO.Tile.Parameters(N = nPatch,aquaModule = "emerge",aquaPars = list(N=nPatch,lambda=rep(50,nPatch)),pathogenModule = "PfSI")
tileParameters$MacroMosquitoPop_PAR$M_density = rep(200,nPatch)
tile = MacroTile$new(MacroTile_PAR = tileParameters)

tile$get_HumanPop()$queueVaccination_SimBitePfSI(tVaccine = (365*1),tTreat = (365*1)+1,fracPop = 0.75)

PfPR = c(rep(0,nPatch/2),rep(0.75,nPatch/2))
# PfPR = rep(0,nPatch)
tile$init_PfSI(PfPR = PfPR)

tMax = (365)
tile$simMacro(tMax)

pfsiHist = tile$get_HumanPop()$get_PfSI_history()
outfilename = paste0("pfsiHist_", as.character(seed),".rds")

pwd <- getwd()
print(paste0("Saving to present working directory ", pwd, "/Test-MACRO-Emerge-outputs/", outfilename))
saveRDS(pfsiHist, sprintf(paste0(pwd, "/Test-MACRO-Emerge-outputs/", outfilename)))