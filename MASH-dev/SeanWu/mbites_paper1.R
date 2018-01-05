###############################################################################
#           __  ___      ____  _____________________
#         /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \ 
#     / /  / /_____/ /_/ // /  / / / /___ ___/ / 
#   /_/  /_/     /_____/___/ /_/ /_____//____/  
#
#   M-BITES Manuscript File 1
#   Simulations on pointsets
#   MASH Team
#   January 2018
#
###############################################################################


###############################################################################
# load pointsets
###############################################################################

rm(list=ls());gc()
set.seed(42L)

files_dir = "/Users/slwu89/Desktop/git/MASH-Main-slwu89/MASH-dev/DavidSmith/MBITES-Demo/"
files_inDir = system(command = paste0("ls ",files_dir),intern = TRUE)
f_files = files_inDir[grep(pattern = "peridom.f",x = files_inDir)]
l_files = files_inDir[grep(pattern = "peridom.l",x = files_inDir)]

xy_lscape = vector(mode = "list",length = length(f_files))
N = length(xy_lscape)

for(i in 1:N){
  xy_lscape[[i]]$f = read.table(file = paste0(files_dir,f_files[i]),header = TRUE,sep = ",",stringsAsFactors = FALSE)
  xy_lscape[[i]]$l = read.table(file = paste0(files_dir,l_files[i]),header = TRUE,sep = ",",stringsAsFactors = FALSE)
}
xy_lscape = xy_lscape[1:2]

###############################################################################
# run simulations (100 repetitions on each pointset for averaging)
###############################################################################

library(foreach)
library(doParallel)

doParallel::registerDoParallel(cl = 2)

# output directory
master_dir = "/Users/slwu89/Desktop/MBITES/"
dir.create(master_dir)

# giant foreach loop
out = foreach(it = iter(xy_lscape), i = icount(), .export = c("master_dir"), .inorder = FALSE, .packages = c("MASHcpp","MASHmacro","MASHmicro"), .verbose = F) %dopar% {
  
  # simulation parameters
  nMosy = 10
  tMax = 20
  jmax = 5
  site_lambda = 5
  
  # make a tile
  DIR = paste0(master_dir,"lscape",i,"/")
  
  # setup
  Humans.MICRO.Setup()
  PfSI.MICRO.Setup()
  AQUA.Emerge.Setup()
  
  # MBITES setup
  MBITES.Setup(SUGAR = FALSE, MATE = FALSE, aquaModule = "emerge",timing = "exponential")
  
  # SEARCH setup
  MBITES.Search.Setup(module = "kernel")
  
  # landscape parameters
  periDom_index = unname(apply(X = it$f[,1:2] == it$l[,1:2],MARGIN = 1,FUN = all))
  
  nFeed = nrow(it$f)
  nAqua = sum(!periDom_index)
  nPeriDom = sum(periDom_index)
  nSugar = 0
  nMate = 0
  
  emerge_par = list(N = nAqua,lambda = site_lambda, lambdaWeight = NULL, offset = NULL)
  landscape_par = Landscape.Parameters(nFeed = nFeed,nAqua = nAqua,nMate = nMate,nSugar = nSugar,pointGen = "lattice",module = "emerge",modulePars = emerge_par)
  landscape_par$AquaticSite_PAR$lambda = replicate(n = nAqua,expr = {rep(site_lambda,365)},simplify = FALSE)
  
  # set up the peri-domestic bit
  periDomestic = rep(FALSE,nFeed)
  lambda = replicate(n = nFeed,expr = NULL,simplify = FALSE)
  module = rep(NA,nFeed)
  
  if(nPeriDom>0){
    periDomestic[1:nPeriDom] = TRUE
    lambda[1:nPeriDom] = replicate(n = nPeriDom,expr = {rep(site_lambda,365)},simplify = FALSE)
    module[1:nPeriDom] = "emerge"
  }
  
  landscape_par$FeedingSite_PAR$periDomestic = periDomestic
  landscape_par$FeedingSite_PAR$lambda = lambda
  landscape_par$FeedingSite_PAR$module = module
  
  # human parameters
  patch_humans = rep(1,nFeed)
  n_humans = sum(patch_humans)
  patch_id = rep(x = 1:nFeed,patch_humans)
  home_id = rep(x = 1:nFeed,patch_humans)
  human_ages = unlist(lapply(X = patch_humans,FUN = MASHmacro:::siteAges_HumanPop))
  human_bWeight = rep(1,n_humans)
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
  
  # M-BITES parameters
  mbites_par_female = MBITES.Complex.Parameters(PfEIP = 10,SUGAR = FALSE,MATE = FALSE)
  mosquito_par = list(
    N_female = nMosy,
    ix_female = rep(x = 1:nAqua,length.out=nMosy),
    genotype_female = rep(1,nMosy),
    MBITES_PAR_FEMALE = mbites_par_female
  )
  
  MicroTile = Tile$new(Landscape_PAR = landscape_par,HumanPop_PAR = human_par,MosquitoPop_PAR = mosquito_par,directory = DIR)
  MicroTile$get_HumanPop()$init_ActivitySpace()
  
  # PfPR
  pfpr = rep(0,nFeed)
  
  for(j in 1:jmax){
    
    MicroTile$simMICRO_oneRun(tMax = tMax,PfPAR = pfpr,verbose = FALSE,trackPop = TRUE, runID = j)
    
    MicroTile$resetMicro(MosquitoPar = mosquito_par,HumanPar = human_par,EL4P = FALSE,mating = FALSE)
    MicroTile$get_HumanPop()$init_ActivitySpace()
  }
  
  return(c(i,j))
}

doParallel::stopImplicitCluster()