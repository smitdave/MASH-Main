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


###############################################################################
# load landscape sets
###############################################################################

dir_dev <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/"

landscapes <- readRDS(file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticLandscapes.rds"))
# xy_sites <- readRDS(file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticRaw.rds"))
humans <- readRDS(file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticHumans.rds"))
mosquitoes <- readRDS(file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticMosquitoes.rds"))


# ###############################################################################
# # Run MBITES
# ###############################################################################
# 
# library(MBITES)
# 
# # initialize methods
# # deterministic wait times
# # MBITES_Setup_Timing(timing_model = 1,
# #                     wait_b = 1,wait_o = 1,wait_m = 1,wait_s = 1,
# #                     wait_bs = 1,wait_os = 1,wait_ms = 1,wait_ss = 1,
# #                     ppr_model = 1,wait_ppr = 0.5/24)
# 
# MBITES_Setup_Timing(timing_model = 2,
#                     rate_b = 1/3,tmin_b = 0,
#                     rate_bs = 1/3,tmin_bs = 0,
#                     rate_o = 1/3,tmin_o = 0,
#                     rate_os = 1/3,tmin_os = 0,
#                     ppr_model = 1,wait_ppr = 0.5/24
# )
# 
# MBITES_Setup_BloodMeal(overfeeding = FALSE)
# 
# MBITES_Setup_Oogenesis(oogenesis_model = 1,eggMaturationTime = FALSE,eggsize_model = 2,refeeding = 3)
# 
# MBITES_Setup_Energetics(sugar = FALSE)
# 
# MBITES_Setup_Oviposition(aqua_model = 1)
# 
# MBITES_Setup_Survival(tattering = FALSE,senescence = FALSE)
# 
# PATHOGEN_Setup(pathogen_model = "null")
# 
# # we want detailed output of blood hosts from the mosquito
# # trackBloodHost()
# 
# # set parameters
# MBITES:::Parameters$set_parameters(disperse = 0.01,Bs_surv = 0.98,Os_surv = 0.98,B_surv = 0.98,O_surv = 0.98,
#                                    Bs_succeed = 0.99,Os_succeed = 0.99,B_succeed = 0.99,O_succeed = 0.99)
# 
# # initialize a tile
# Tile_Initialize(landscape)
# 
# Human_NULL_Initialize(humans)
# 
# MBITES_Initialize(mosquitos)
# 
# # run simulation
# set_output(directory = "/Users/slwu89/Desktop/mbites/",runID = 1)
# simulation(tMax = 365,pretty = TRUE)
# 
# reset(directory = "/Users/slwu89/Desktop/mbites/",runID = 2)
# Human_NULL_Initialize(humans)
# MBITES_Initialize(mosquitos)
# simulation(tMax = 365,pretty = TRUE)

# something like this
# library(doParallel)
# registerDoParallel(cores = 8)
# result <- foreach( A =rep(c(1,2,3),2), B = rep(c(10, 20), each=3), 
#                    .combine='cbind') %dopar% { A*B }
