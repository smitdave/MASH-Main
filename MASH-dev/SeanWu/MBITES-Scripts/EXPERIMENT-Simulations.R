###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Simulations of Peri-domestic breeding resource-scapes
#     MBITES Team
#     May 2018
#
###############################################################################

rm(list=ls());gc()
library(truncdist) # for zero-inflation of distributions (we dont do movement from i->i)
library(viridis)


###############################################################################
# load landscape sets
###############################################################################

dir_dev <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/"

landscapes <- readRDS(file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticLandscapes.rds"))
# xy_sites <- readRDS(file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticRaw.rds"))
humans <- readRDS(file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticHumans.rds"))
mosquitoes <- readRDS(file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticMosquitoes.rds"))
