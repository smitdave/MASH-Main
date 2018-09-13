###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Simulations of Peri-domestic breeding resource-scapes
#     Script for running a single MBITES simulation
#     MBITES Team
#     May 2018
#
###############################################################################

rm(list=ls());gc()

###############################################################################
# Catch variables from the qsubber call
###############################################################################

seed <- as.integer(commandArgs()[6])
if (!is.na(seed)) {
  print(seed) } else {
    print("Not an integer")
    seed <- 0 # set default seed
  }
set.seed(seed)

###############################################################################
# load landscape sets
###############################################################################

dir_dev <- "/ihme/malaria_modeling/dtcitron/MASH-Main/MASH-dev/"

landscapes <- readRDS(file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticLandscapes.rds"))
humans <- readRDS(file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticHumans.rds"))
mosquitoes <- readRDS(file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticMosquitoes.rds"))

peridomestic_data <- vector(mode="list",length=length(landscapes))
directory <- "/ihme/malaria_modeling/dtcitron/MBITES_Experiment_Simulations/peridom_qsub/"

for(i in 1:length(peridomestic_data)){
  peridomestic_data[[i]]$id <- i
  peridomestic_data[[i]]$directory <- directory
  peridomestic_data[[i]]$landscape <- landscapes[[i]]$sites
  peridomestic_data[[i]]$humans <- as.data.frame(humans[[i]])
  peridomestic_data[[i]]$mosquitoes <- as.data.frame(mosquitoes[[i]])
}


###############################################################################
# load library
###############################################################################
.libPaths( c( .libPaths(), "/ihme/malaria_modeling/dtcitron/Rlibs") )
library(MBITES, lib.loc = "/ihme/malaria_modeling/dtcitron/Rlibs")
MBITES_Setup_MBDETES()



###############################################################################
# start simulation
###############################################################################
# pull out the data we need:
x=peridomestic_data[[seed]]

PATHOGEN_Setup(pathogen_model = "null")

# we want detailed output of blood hosts from the mosquito
trackBloodHost()
trackOviposition()

# set parameters
MBITES:::Parameters$set_parameters(Bs_surv = 0.95,Os_surv = 0.95,B_surv = 0.99,O_surv = 0.99,
                                   Bs_succeed = 0.99,Os_succeed = 0.99,B_succeed = 0.95,O_succeed = 0.99,
                                   S_u = 0,disperse = 0.2)

# initialize a tile
Tile_Initialize(x$landscape)
Human_NULL_Initialize(x$humans)
MBITES_Initialize(x$mosquitoes)

# run simulation
set_output(directory = x$directory,runID = x$id)

simulation(tMax = 5*365,pretty = TRUE)
hardreset()