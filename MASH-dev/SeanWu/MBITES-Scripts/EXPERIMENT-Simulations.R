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
humans <- readRDS(file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticHumans.rds"))
mosquitoes <- readRDS(file = paste0(dir_dev,"DavidSmith/MBITES-Demo/periDomesticMosquitoes.rds"))

peridomestic_data <- vector(mode="list",length=length(landscapes))
directory <- "/Users/slwu89/Desktop/mbites/peridom/"

for(i in 1:length(peridomestic_data)){
  peridomestic_data[[i]]$id <- i
  peridomestic_data[[i]]$directory <- directory
  peridomestic_data[[i]]$landscape <- landscapes[[i]]$sites
  peridomestic_data[[i]]$humans <- as.data.frame(humans[[i]])
  peridomestic_data[[i]]$mosquitoes <- as.data.frame(mosquitoes[[i]])
}


###############################################################################
# Run MBITES
###############################################################################

library(parallel)
cl <- parallel::makePSOCKcluster(names = 4)

# initialize MBITES parameters on cores
parallel::clusterEvalQ(cl = cl,expr = {
  library(MBITES)

  # initialize methods
  MBITES_Setup_MBDETES()

  PATHOGEN_Setup(pathogen_model = "null")

  # we want detailed output of blood hosts from the mosquito
  trackBloodHost()
  trackOviposition()

  # set parameters
  MBITES:::Parameters$set_parameters(Bs_surv = 0.95,Os_surv = 0.95,B_surv = 0.99,O_surv = 0.99,
                                     Bs_succeed = 0.99,Os_succeed = 0.99,B_succeed = 0.95,O_succeed = 0.99,
                                     S_u = 0,disperse = 0.2)
})

# set RNG streams
parallel::clusterSetRNGStream(cl = cl,iseed = 123)

# for running locally with only 4 cores
idx <- list((1:4),(5:8),(9:12),(13:16),(17:20),(21:24),(25:26))
idx <- list((1:8),(9:17),(18:25),26) # 8 cores

for(ix in idx){
  # run simulation
  parallel::clusterMap(cl = cl,fun = function(x){

    # initialize a tile
    Tile_Initialize(x$landscape)
    Human_NULL_Initialize(x$humans)
    MBITES_Initialize(x$mosquitoes)

    # run simulation
    set_output(directory = x$directory,runID = x$id)

    simulation(tMax = 365*100,pretty = TRUE)
    hardreset()

  },x=peridomestic_data[ix],RECYCLE = FALSE,SIMPLIFY = FALSE,USE.NAMES = FALSE,.scheduling = "dynamic")
}

parallel::stopCluster(cl)
rm(cl);gc()
