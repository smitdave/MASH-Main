###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Trivial Landscape
#     MBITES Team
#     June 2018
#
###############################################################################

rm(list = ls())
library(MBITES)


###############################################################################
# Make landscape initialization object
###############################################################################

# landscapes
landscape <- vector(mode = "list",length = 1)

# site characteristics
i = 1L
landscape[[i]]$id = 1
landscape[[i]]$xy = c(1,1)
landscape[[i]]$type = 1L
landscape[[i]]$tileID = 1L
landscape[[i]]$move = 1
landscape[[i]]$move_id = as.integer(1)
landscape[[i]]$haz = 0.001
# null resources
landscape[[i]]$feed[[1]] = list(w=1,enterP=1)
landscape[[i]]$aqua[[1]] = list(w=1,lambda=1)


###############################################################################
# Make human initialization object
###############################################################################

nHumans = 1

humans = data.frame(
  tileID = rep(1,nHumans),
  siteID = 1,
  feedingID = rep(1,nHumans),
  w = rep(1,nHumans)
)


###############################################################################
# Make mosquito initialization object
###############################################################################

nMosquitos = 50

mosquitos = data.frame(
  tileID = rep(1,nMosquitos),
  siteID =rep(1,nMosquitos),
  female = rep(T,nMosquitos)
)


###############################################################################
# Run MBITES
###############################################################################

library(MBITES)

# initialize methods
MBITES_Setup_Timing(timing_model = 2,
                    rate_b = 1/3,tmin_b = 0,
                    rate_bs = 1/3,tmin_bs = 0,
                    rate_o = 1/3,tmin_o = 0,
                    rate_os = 1/3,tmin_os = 0,
                    ppr_model = 1,wait_ppr = 0.5/24
)

MBITES_Setup_BloodMeal(overfeeding = FALSE)

MBITES_Setup_Oogenesis(oogenesis_model = 1,eggMaturationTime = FALSE,eggsize_model = 2,refeeding = 3)

MBITES_Setup_Energetics(sugar = FALSE)

MBITES_Setup_Oviposition(aqua_model = 1)

MBITES_Setup_Survival(tattering = FALSE,senescence = FALSE)

PATHOGEN_Setup(pathogen_model = "null")

# we want detailed output of blood hosts from the mosquito
trackBloodHost()

# set parameters
MBITES:::Parameters$set_parameters(disperse = 0.01,Bs_surv = 0.98,Os_surv = 0.98,B_surv = 0.98,O_surv = 0.98,
                                   Bs_succeed = 0.99,Os_succeed = 0.99,B_succeed = 0.99,O_succeed = 0.99)

# initialize a tile
Tile_Initialize(landscape)

Human_NULL_Initialize(humans)

transitions <- MBDETES_Approx(1L)
