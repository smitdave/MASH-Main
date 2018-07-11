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
landscape <- vector(mode = "list",length = 3)

# site characteristics
for(i in 1:3){
  landscape[[i]]$id = 1
  landscape[[i]]$xy = c(1,1)
  landscape[[i]]$type = 1L
  landscape[[i]]$tileID = 1L
  landscape[[i]]$move = 1
  landscape[[i]]$move_id = (1:3)[-i]
  landscape[[i]]$haz = 0.001
}

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

directory <- "/Users/slwu89/Desktop/mbites/trivial/"

# initialize methods
MBITES_Setup_Timing(timing_model = 2,
                    rate_b = 1/2/24,tmin_b = 0,
                    rate_bs = 3/24,tmin_bs = 0,
                    rate_o = 1/2/24,tmin_o = 0,
                    rate_os = 1/24,tmin_os = 0,
                    ppr_model = 2,rate_ppr = 18/24,tmin_ppr = 0
)

MBITES_Setup_BloodMeal(overfeeding = FALSE)

MBITES_Setup_Oogenesis(oogenesis_model = 1,eggMaturationTime = FALSE,eggsize_model = 2,refeeding = 3)

MBITES_Setup_Energetics(sugar = FALSE)

MBITES_Setup_Oviposition(aqua_model = 1)

MBITES_Setup_Survival(tattering = FALSE,senescence = FALSE)

PATHOGEN_Setup(pathogen_model = "null")

# we want detailed output of blood hosts from the mosquito
trackBloodHost()
trackOviposition()

# set parameters
Bs_success = 0.84/0.94
B_success = 0.75/0.95
O_success = 0.75/0.95
Os_success = 0.84/0.94
MBITES:::Parameters$set_parameters(disperse = 0.05,Bs_surv = 0.94,Os_surv = 0.95,B_surv = 0.98,O_surv = 0.98,
                                   Bs_succeed = Bs_success,Os_succeed = Os_success,B_succeed = B_success,O_succeed = O_success)

# initialize a tile
Tile_Initialize(landscape)

Human_NULL_Initialize(humans)

transitions <- MBDETES_Approx(1L)

MBITES_Initialize(mosquitos)

# run simulation
set_output(directory = directory,runID = 1)

simulation(tMax = 365*2,pretty = TRUE)
hardreset()


###############################################################################
# Parse output
###############################################################################

library(jsonlite)
library(ggplot2)

# where the files can be found
output_dir <- "/Users/slwu89/Desktop/mbites/trivial/run1"

mosquitos <- fromJSON(paste0(output_dir,"/mosquito_F_1.json"), flatten = TRUE)
mosquitos <- mosquitos[-which(sapply(mosquitos$id,is.null)),]
humans <- fromJSON(paste0(output_dir,"/human_1.json"), flatten = TRUE)
humans <- humans[-which(sapply(humans$id,is.null)),]

lf <- Bionomics_lifespan(mosquitos)
mean(lf$lifespan)
sd(lf$lifespan)

#Mosquito lifespan chart
ggplot() + geom_histogram(data = lf, aes(lifespan), fill = "steelblue", bins = 20) +
  ggtitle("Mosquito Lifespans") + xlab("Days") + ylab("Frequency") + theme_bw()

