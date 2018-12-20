################################################################################
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   Testing PfSI in MACRO package
#
#   Sean Wu
#   December 2018
#
################################################################################

rm(list=ls())
library(MACRO)

a=as.numeric(Sys.time())
abs(100000000*abs(log(a)))
seed <- floor(a)

# seed <- 95234L

# test a very simple 2-patch system
# unlike a normal stochastic matrix, p_{ii} = 0, even though sum_{j} p_{ij} = 1 (this is conditional on going *somewhere*)
n <- 2
move <- matrix(c(0,1,1,0),byrow = T,n,n)

patch_pars <- patches_parameters(move = move,bWeightZoo = rep(0,n),bWeightZootox = rep(0,n),reservoir = rep(F,n),res_EIR = rep(0,n))
pfsi_pars <- pfsi_parameters()
mosy_pars <- mosquito_rm_conpars(N = n,lambda = matrix(1,nrow = 365,ncol = n),
                                 psi = diag(n),rep(11,365),M = rep(500,n),Y = rep(10,n),Z = rep(2,n))
vaxx_pars <- list()

path <- "/Users/slwu89/Desktop/macro/"
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

log_pars <- list()
log_pars[[1]] <- list(outfile = "/Users/slwu89/Desktop/macro/h_move.csv",key = "human_move", 
                      header = paste0(c("humanID","time","event","location"),collapse = ","))
log_pars[[2]] <- list(outfile = "/Users/slwu89/Desktop/macro/h_inf.csv",key = "human_inf",
                      header = paste0(c("humanID","time","event"),collapse = ","))
log_pars[[3]] <- list(outfile = "/Users/slwu89/Desktop/macro/mosy.csv",key = "mosquito",
                      header = paste0(c("time","state",paste0("patch",1:n)),collapse = ","))

nh <- 4L
pfpr <- c(1,0.1)
inf_bool <- c(sample(x = c(T,F),size = nh/2,replace = T,prob = c(pfpr[1],1-pfpr[1])),
              sample(x = c(T,F),size = nh/2,replace = T,prob = c(pfpr[2],1-pfpr[2])))
patch_id <- c(rep(0,nh/2),rep(1,nh/2))
bweights <- rgamma(n = nh,shape = 16,rate = 16)
human_pars <- vector("list",nh)
for(i in 1:nh){
  human_pars[[i]] <- human_pfsi_conpars(id = i-1,home_patch_id = patch_id[i],
                                        trip_duration = 2,trip_frequency = 1/5,bweight = bweights[i],
                                        age = 20,inf = inf_bool[i],chx = F)
}
check_human_pfsi_conpars(human_pars)

# source("/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/SeanWu/MACRO-dev/MACRO-PfSI-test.R")

run_macro(seed = seed,
          tmax = 20L,
          human_pars = human_pars,
          mosquito_pars = mosy_pars,
          patch_pars = patch_pars,
          model_pars = pfsi_pars,
          log_streams = log_pars,
          vaxx_events = vaxx_pars)
