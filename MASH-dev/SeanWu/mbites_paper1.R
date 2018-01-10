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
out = foreach(it = iter(xy_lscape), i = icount(), .export = c("master_dir"), .inorder = FALSE, .packages = c("MASHcpp","MASHmacro","MASHmicro"), .verbose = TRUE, .errorhandling = "pass") %dopar% {
  
  # simulation parameters
  nMosy = 250
  tMax = 365
  jmax = 1
  site_lambda = 2
  
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
  if(all(periDom_index)){
    periDom_index[length(periDom_index)] = FALSE
  }
  
  nFeed = nrow(it$f)
  nAqua = sum(!periDom_index)
  nPeriDom = sum(periDom_index)
  nSugar = 0
  nMate = 0
  
  emerge_par = list(N = nAqua,lambda = site_lambda, lambdaWeight = NULL, offset = NULL)
  landscape_par = Landscape.Parameters(nFeed = nFeed,nAqua = nAqua,nMate = nMate,nSugar = nSugar,pointGen = "lattice",module = "emerge",modulePars = emerge_par)
  landscape_par$AquaticSite_PAR$lambda = replicate(n = nAqua,expr = {rep(site_lambda,365)},simplify = FALSE)
  
  landscape_par$FeedingSite_PAR$siteXY$x = it$f$x
  landscape_par$FeedingSite_PAR$siteXY$y = it$f$y
  landscape_par$AquaticSite_PAR$siteXY$x = it$l$x[!periDom_index]
  landscape_par$AquaticSite_PAR$siteXY$y = it$l$y[!periDom_index]
  
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


###############################################################################
# Graphics
###############################################################################

source("/Users/slwu89/Desktop/git/MASH-Main-slwu89/MASH-dev/SeanWu/PLOTS_Bionomics.R")
library(stringr)
library(chorddiag)
library(jsonlite)
library(ggplot2)
library(gridExtra)
landscapes = system(command = paste0("ls ",master_dir),intern = TRUE)
landscapes = landscapes[order(as.numeric(str_match(string = landscapes,pattern = "[0-9]+$")))]

for(l in landscapes){
  
  # files from each simulation
  files_l = system(command = paste0("ls ",master_dir,l,"/MOSQUITO/"),intern = TRUE)
  hist_l = files_l[grep(pattern = "History",x = files_l)] # individual json histories
  pop_l = files_l[grep(pattern = "Pop",x = files_l)] # population csv
  
  # output
  mHist = fromJSON(txt = paste0(master_dir,l,"/MOSQUITO/",hist_l),flatten = FALSE,simplifyVector=FALSE)
  mPop = read.table(file = paste0(master_dir,l,"/MOSQUITO/",pop_l),header = TRUE,sep = ",")
  
  nullIx = which(vapply(X = mHist,FUN = function(x){x$ID[[1]]},FUN.VALUE = character(1)) == "NULL")
  mHist = mHist[-nullIx]
  
  # histograms
  # lifespans = histogramPlotLyGenericBionomics(data = bionomics_lifespan(mHist),title = "Mosquito Lifespans",color = rgb(0,.5,.5,.5))
  # BMinterval = histogramPlotLyGenericBionomics(data = bionomics_BMinterval(mHist),title = "Bloodmeal Interval",color = rgb(0,.5,0,.5))
  # HumanBMinterval = histogramPlotLyGenericBionomics(data = bionomics_HumanBMinterval(mHist),title = "Human Bloodmeal Interval",color = rgb(1,.5,0,.5))
  # HumanBM = histogramPlotLyGenericBionomics(data = bionomics_HumanBM(mHist),title = "Human Bloodmeals",color = rgb(1,0,0,.5))
  
  axisSize = 12
  titleSize = 14.5
  
  lifespans = bionomics_lifespan(mHist)
  lifespans_plot = ggplot(data = data.frame(lifespan=lifespans)) +
    geom_histogram(aes(lifespan),fill=rgb(0,.5,.5,.5)) +
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          axis.title=element_text(size=axisSize),
          plot.title = element_text(size=titleSize)) +
    guides(fill = FALSE) + 
    labs(x="Days",y="Frequency",title="Mosquito Lifespans")
  
  BMintervals = bionomics_BMinterval(mHist)
  BMintervals_plot = ggplot(data = data.frame(BMinterval=BMintervals)) +
    geom_histogram(aes(BMinterval), fill = rgb(0,.5,0,.5)) +
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          axis.title=element_text(size=axisSize),
          plot.title = element_text(size=titleSize)) +
    guides(fill = FALSE) + 
    labs(x="Days",y="Frequency",title="Bloodmeal Interval")
  
  # HumanBMintervals = bionomics_HumanBMinterval(mHist)
  # HumanBMintervals_plot = ggplot(data = data.frame(HumanBMinterval=HumanBMintervals)) +
  #   geom_histogram(aes(HumanBMinterval), fill = rgb(1,.5,0,.5)) +
  #   theme_bw() + 
  #   theme(panel.grid.minor = element_blank()) +
  #   guides(fill = FALSE) + 
  #   labs(x="Days",y="Frequency",title="Human Bloodmeal Interval")
  vectorialCapacity = bionomics_vc(mHist,eip = 8)
  vectorialCapacity_plot = ggplot(data = data.frame(vectorialCapacity=vectorialCapacity)) +
    geom_histogram(aes(vectorialCapacity), fill = rgb(0,.5,0,.5),stat = "count") +
    scale_x_continuous(breaks=0:(max(vectorialCapacity)+2)) + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          axis.title=element_text(size=axisSize),
          plot.title = element_text(size=titleSize)) +
    guides(fill = FALSE) + 
    labs(x="Vectorial Capacity",y="Frequency",title="Individual Vectorial Capacity")
  
  HumanBMs = bionomics_HumanBM(mHist)
  HumanBMs_plot = ggplot(data = data.frame(HumanBM=HumanBMs)) +
    geom_histogram(aes(HumanBM), fill = rgb(1,0,0,0.5),stat = "count") +
    scale_x_continuous(breaks=0:(max(HumanBMs)+2)) + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          axis.title=element_text(size=axisSize),
          plot.title = element_text(size=titleSize)) +
    guides(fill = FALSE) + 
    labs(x="Count",y="Frequency",title="Human Bloodmeals")
  
  grid.arrange(BMintervals_plot,HumanBMs_plot,lifespans_plot,vectorialCapacity_plot,nrow=2)
  
  
  
}


