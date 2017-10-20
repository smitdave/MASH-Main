#################################################################
#
#   MASH: Testing routines for MACRO
#
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   Sean Wu
#   May 25, 2017
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
library(MASH)

set.seed(42)

# initialize classes for MACRO
MACRO.Humans.Setup(pathogenModule = "PfSI")

# setup class methods for PfSI and SimBite modules
# MACRO relies on SimBitePfSI module for adding bites to humans
PfSI.Setup()
SimBitePfSI.Setup()

# MACRO Patch initialization
MACRO.Patch.Emerge.Setup() # 'Emerge' model

# patch parameters
nPatch = 10
tileParameters = MACRO.Tile.Parameters(N = nPatch,aquaModule = "emerge",aquaPars = list(N=nPatch,lambda=rep(50,nPatch)),pathogenModule = "PfSI")
tileParameters$MacroMosquitoPop_PAR$M_density = rep(200,nPatch)
tile = MacroTile$new(MacroTile_PAR = tileParameters)

PfPR = c(rep(0,nPatch/2),rep(0.75,nPatch/2))
# PfPR = rep(0,nPatch)
tile$init_PfSI(PfPR = PfPR)

tile$simMacro(500)

pfsiHist = tile$get_HumanPop()$get_PfSI_history()
plot_PfSI(pfsiHist)

travelHist = tile$get_HumanPop()$get_travelHistory()
tile$get_HumanPop()$json_travelHistory(con = file(description = "/Users/slwu89/Desktop/OUTPUT/humanTravel.json",open = "wt"))


#################################################################
#
#     _____   __ __  __  ____ _____
#    |  __ \ / _|  \/  |/ __ \_   _|
#    | |__) | |_| \  / | |  | || |
#    |  ___/|  _| |\/| | |  | || |
#    | |    | | | |  | | |__| || |_
#    |_|    |_| |_|  |_|\____/_____|
#
#################################################################

rm(list=ls())
gc()
library(MASH)

set.seed(42)

# initialize classes for MACRO
MACRO.Humans.Setup(pathogenModule = "PfMOI")

PfMOI.Setup(interaction = TRUE,InteractionPf = 0.15)
SimBitePfMOI.Setup()

# MACRO Patch initialization
MACRO.Patch.Emerge.Setup() # 'Emerge' model

# patch parameters
nPatch = 10
tileParameters = MACRO.Tile.Parameters(N = nPatch,aquaModule = "emerge",aquaPars = list(N=nPatch,lambda=rep(50,nPatch)),pathogenModule = "PfMOI")
tileParameters$MacroMosquitoPop_PAR$M_density = rep(200,nPatch)
tile = MacroTile$new(MacroTile_PAR = tileParameters)

PfMOI = c(rep(0,nPatch/2),rep(1,nPatch/2))
# debug(tile$init_PfMOI)
tile$init_PfMOI(PfMOI = PfMOI)

tile$simMacro(500)

pfmoiHist = tile$get_HumanPop()$get_PfMOI_history()
# plot_PfSI(pfsiHist)

travelHist = tile$get_HumanPop()$get_travelHistory()
tile$get_HumanPop()$json_travelHistory(con = file(description = "/Users/slwu89/Desktop/OUTPUT/humanTravel.json",open = "wt"))
