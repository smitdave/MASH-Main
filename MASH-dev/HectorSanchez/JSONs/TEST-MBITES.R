###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MICRO
#   MBITES: Testing
#   MASH Team
#   January 2018
#
###############################################################################

###############################################################################
# M-BITES: Complex Female + MBITES-Male Run with Emerge
###############################################################################

rm(list=ls());gc()
library(MASHmicro)
# set.seed(42L)

# DEBUG.MASHMICRO()
# MASHcpp::DEBUG.MASHCPP()
# MASHmacro::DEBUG.MASHMACRO()

# make a tile
DIR = "/Users/sanchez.hmsc/Desktop/MBITES/"

# setup
Humans.MICRO.Setup()
PfSI.MICRO.Setup()
AQUA.Emerge.Setup()

# MBITES setup
MBITES.Setup(aquaModule = "emerge",timing = "exponential")
MBITES.Male.Setup(timing = "exponential")

# SEARCH setup
MBITES.Search.Setup(module = "kernel")

# landscape parameters
nAqua = 20
nPeriDom = 2
nFeed = 15
nSugar = 12
nMate = 10
emerge_par = list(N = nAqua,lambda = 25, lambdaWeight = NULL, offset = NULL)
landscape_par = Landscape.Parameters(nFeed = nFeed,nAqua = nAqua,nMate = nMate,nSugar = nSugar,pointGen = "lattice",module = "emerge",modulePars = emerge_par)

# set up the peri-domestic bit
periDomestic = rep(FALSE,nFeed)
lambda = replicate(n = nFeed,expr = NULL,simplify = FALSE)
module = rep(NA,nFeed)

periDomestic[1:nPeriDom] = TRUE
lambda[1:nPeriDom] = simpleLambda_Emerge(N = nPeriDom,lambda = 5)
module[1:nPeriDom] = "emerge"

landscape_par$FeedingSite_PAR$periDomestic = periDomestic
landscape_par$FeedingSite_PAR$lambda = lambda
landscape_par$FeedingSite_PAR$module = module

# human parameters
patch_humans = rep(1,nFeed)
n_humans = sum(patch_humans)
patch_id = rep(x = 1:nFeed,patch_humans)
home_id = rep(x = 1:nFeed,patch_humans)
human_ages = unlist(lapply(X = patch_humans,FUN = MASHmacro:::siteAges_HumanPop))
human_bWeight = MASHmacro:::bitingWeight_HumanPop(human_ages)
human_par = lapply(X = 1:n_humans,function(i){
  list(
    houseID = home_id[i],
    patchID = patch_id[i],
    homeHouseID = home_id[i],
    homePatchID = patch_id[i],
    age = human_ages[i],
    bWeight = human_bWeight[i]

  )
})
write(toJSON(human_par),paste0(DIR,"human_par.json"))
human_par=fromJSON(file=paste0(DIR,"human_par.json"))

# M-BITES parameters
nMosy = 50
mbites_par_female = MBITES.Complex.Parameters(PfEIP = 1 )
mbites_par_male = MBITES.Male.Parameters(maleHistory = TRUE)
mosquito_par = list(
  N_female = nMosy,
  N_male = nMosy,
  ix_female = rep(1,nMosy),
  ix_male = rep(1,nMosy),
  genotype_female = rep(1,nMosy),
  genotype_male = rep(1,nMosy),
  MBITES_PAR_FEMALE = mbites_par_female,
  MBITES_PAR_MALE = mbites_par_male
)
# Export to json ---------------------------------------------------------------
# jsonOut=prettify(toJSON(human_par))
# write(jsonOut,paste0(DIR,"human_par.json"))
# human_par2=read_json(paste0(DIR,"human_par.json"),simplifyVector=TRUE)
# jsonOut=prettify(toJSON(mbites_par_female))
# write(jsonOut,paste0(DIR,"mbites_par_female.json"))
# mbites_par_female2=read_json(paste0(DIR,"mbites_par_female.json"),simplifyVector=TRUE)
# jsonOut=toJSON(mbites_par_male)
# write(jsonOut,paste0(DIR,"mbites_par_male.json"))
# mbites_par_male2=read_json(paste0(DIR,"mbites_par_male.json"),simplifyVector=FALSE)
# ------------------------------------------------------------------------------


MicroTile = Tile$new(Landscape_PAR = landscape_par,HumanPop_PAR = human_par,MosquitoPop_PAR = mosquito_par,directory = DIR)
MicroTile$get_HumanPop()$init_ActivitySpace()

# PfPR
pfpr = rep(0.5,nFeed)

MicroTile$simMICRO_oneRun(tMax = 50,PfPAR = pfpr,verbose = TRUE,trackPop = TRUE)

MicroTile$resetMicro(MosquitoPar = mosquito_par,HumanPar = human_par,EL4P = FALSE,mating = TRUE)
MicroTile$get_HumanPop()$init_ActivitySpace()
MicroTile$simMICRO_oneRun(tMax = 50,PfPAR = pfpr,verbose = TRUE,trackPop = TRUE)

detach("package:MASHmicro", unload=TRUE)
