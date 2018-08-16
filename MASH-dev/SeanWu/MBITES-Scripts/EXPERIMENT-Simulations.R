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


###############################################################################
# Run MBITES
###############################################################################

library(MBITES)

# the thing to toss to Tile_Initialize(landscape) for landscape i
# landscapes[[i]]$sites

# the thing to toss to Human_NULL_Initialize for landscape i
# as.data.frame(humans[[i]])

# the thing to toss to MBITES_Initialize(mosquitos) for landscape i
# as.data.frame(mosquitoes[[i]])

library(parallel)
cl <- parallel::makePSOCKcluster(names = 4)

parallel::clusterSetRNGStream(cl = cl,iseed = 123)

vec <- vector("list",4)
vec[[1]]$n = 5
vec[[2]]$n = 6
vec[[3]]$n = 7
vec[[4]]$n = 10

out <- parallel::clusterMap(cl = cl,fun = function(x){
  return(rpois(n = x$n,lambda = 10))
},x=vec,RECYCLE = FALSE,SIMPLIFY = FALSE,USE.NAMES = FALSE,.scheduling = "dynamic")

parallel::stopCluster(cl)
rm(cl)
