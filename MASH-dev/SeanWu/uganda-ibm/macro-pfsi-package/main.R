# sample of MACRO (metapopulation) model with PfSI infection module
# include human movement
# include simple mosquito model (difference equations) with diffusion between patches

# source("/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/SeanWu/uganda-ibm/macro-pfsi-dev/main.R")

# clear the R workspace
rm(list=ls());gc()

# here package to get paths right between different systems
library(here)

# # need these 3 packages of the Rcpp ecosystem
# # Rcpp: the main R to C++ interface package
# # RcppArmadillo: the mosquito model relies on matrix algebra, this lets us use those headers
# # RcppProgress: header for an interruptable progress bar
# library(Rcpp)
# library(RcppArmadillo)
# library(RcppProgress)

library(macro.pfsi.rm)

# need the Matrix library for sparse matrices
library(Matrix)

# a path to put output output files
# it makes a sub-directory if it doesnt exist, and if it does exist, it clears it out
path <- here::here("output/")
if(!dir.exists(path)){
  dir.create(path)
} else {
  files <- list.files(path)
  if(length(files) > 0){
    for(f in files){
      file.remove(paste0(path,f))
    }
  }
}

# vector of parameters
pfsi_pars <- pfsi_parameters()

# set up patches (n is how many patches we have)
n <- 5
# movement matrix for humans is uniform over possible destinations.
move <- matrix(data = 1/(n-1),nrow = n,ncol = n)
diag(move) <- 0
patch_pars <- patches_parameters(move = move,bWeightZoo = rep(0,n),bWeightZootox = rep(0,n),reservoir = rep(F,n),res_EIR = rep(0,n))

# mosquitos
psi <- Matrix::sparseMatrix(i = {},j = {},x = 0.0,dims = c(5,5))
diag(psi) <- rep(1,n)
mosy_pars <- mosquito_rm_conpars(N = n,lambda = matrix(50,nrow = 365,ncol = n),
                                 psi = psi,EIP = rep(11,365),M = rep(450,n),Y = rep(0,n),Z = rep(0,n))

# humans
patch_sizes <- rep(1e2,n) # number of people in each patch
pfpr <- rep(0.5,n) # malaria prevalence in each patch
nh <- sum(patch_sizes) # total num of humans

# sample the infection status of the population stratified by patch and return as a character vector
init_state <- unlist(mapply(FUN = function(n,pr){
  sample(x = c("I","S"),size = n,replace = T,prob = c(pr,1-pr))
},n=patch_sizes,pr=pfpr,SIMPLIFY = F))

# where the people go (0-indexed for c++)
patch_id <- rep(0:(n-1),times=patch_sizes)

# uniform biting weights (it's renormalized inside the simulation)
bweights <- rep(1,nh)

human_pars <- vector("list",nh)
for(i in 1:nh){
  human_pars[[i]] <- human_pfsi_conpars(id = i-1,home_patch_id = patch_id[i],
                                        trip_duration = rep(3,n),trip_frequency = 1/20,bweight = bweights[i],
                                        age = 20,state = init_state[i])
}
check_human_pfsi_conpars(human_pars)

# vaccinations (can uncomment the code below to vaccinate 25% of the population at day 500)
# vaxx_pars <- list()
vaxx_id <- sample(x = 0:(nh-1),size = nh*0.25,replace = F)
vaxx_pars <- lapply(X = vaxx_id,FUN = function(id){
  vaccination_pfsi_conpars(id = id,t = 5e2,treat = T,type = "PE")
})


################################################################################
# single run
################################################################################

log_pars <- list()
h_inf <- paste0(path,"pfsi.csv")
log_pars[[1]] <- list(outfile = h_inf,key = "pfsi",
                      header = paste0(c("time","patch",unlist(lapply(c("S","I","P"),function(x){paste0(x,c("_visitor","_resident_home","_resident_away"))})),"incidence_resident","incidence_traveller"),collapse = ","))
mosy <- paste0(path,"mosy.csv")
log_pars[[2]] <- list(outfile = mosy,key = "mosquito",
                      header = paste0(c("time","state",paste0("patch",1:n)),collapse = ","))

run_macro(tmax = 1e3,
          human_pars = human_pars,
          mosquito_pars = mosy_pars,
          patch_pars = patch_pars,
          model_pars = pfsi_pars,
          log_streams = log_pars,
          vaxx_events = vaxx_pars,
          verbose = T)

library(tidyverse)
pfsi <- readr::read_csv(here::here("output/pfsi.csv"))

pfsi_pr <- pfsi %>% 
  select(-ends_with("away")) %>%
  select(-starts_with("incidence")) %>%
  gather(key, value, -time,-patch)

ggplot(pfsi_pr) +
  geom_line(aes(x=time,y=value,color=key)) +
  facet_wrap(. ~ patch) +
  theme_bw()

pfsi_inc <- pfsi %>%
  gather(key,value,incidence_resident,incidence_traveller) %>%
  select(one_of(c("time","patch","key","value")))

ggplot(pfsi_inc) +
  geom_line(aes(x=time,y=value,color=key)) +
  facet_wrap(. ~ patch) +
  theme_bw()


################################################################################
# run ensemble
################################################################################

# run ensemble
nrun <- 1e2
tsteps <- 1e3
pb <- txtProgressBar(min = 1,max = nrun)
for(i in 1:nrun){
  
  log_pars <- list()
  h_inf <- paste0(path,"pfsi_",i,".csv")
  log_pars[[1]] <- list(outfile = h_inf,key = "pfsi",
                        header = paste0(c("time","patch",unlist(lapply(c("S","I","P"),function(x){paste0(x,c("_visitor","_resident_home","_resident_away"))})),"incidence_resident","incidence_traveller"),collapse = ","))
  mosy <- paste0(path,"mosy_",i,".csv")
  log_pars[[2]] <- list(outfile = mosy,key = "mosquito",
                        header = paste0(c("time","state",paste0("patch",1:n)),collapse = ","))
  
  run_macro(tmax = tsteps,
            human_pars = human_pars,
            mosquito_pars = mosy_pars,
            patch_pars = patch_pars,
            model_pars = pfsi_pars,
            log_streams = log_pars,
            vaxx_events = vaxx_pars,
            verbose = FALSE)
  setTxtProgressBar(pb,i)
}

pfsi_ensemble <-
  list.files(path = here::here("output/"),pattern = "pfsi_[[:digit:]]+.csv") %>%
  map_df(~read_csv(paste0(here::here("output/",.))),.id = "run") 

pfsi_ensemble_pr <- pfsi_ensemble %>%
  select(-ends_with("away")) %>%
  select(-starts_with("incidence")) %>%
  gather(key, value, -time,-patch,-run)

ggplot(pfsi_ensemble_pr,aes(x=time,y=value,color=key,fill=key)) +
  stat_summary(fun.data = median_hilow,fun.args = list(conf.int = 0.95),geom = "ribbon",alpha=0.4,color=NA) +
  stat_summary(geom="line", fun.y="mean") +
  facet_wrap(. ~ patch) +
  guides(color = FALSE) +
  theme_bw()
