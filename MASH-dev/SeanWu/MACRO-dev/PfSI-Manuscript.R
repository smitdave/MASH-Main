################################################################################
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   Simulation results for manuscript
#
#   Sean Wu
#   February 2019
#
################################################################################

################################################################################
# ensemble run with pfpr 0.15
################################################################################

rm(list=ls());gc()
library(MACRO)
library(tidyr)

# output files
path <- "/Users/slwu89/Desktop/macro/pfpr15/"
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

# pfsi parameters
pfsi_pars <- pfsi_parameters(DurationPf = 200,LatentPf = 0,FeverPf = 0,TreatPf = 0)

# patchs
n <- 1
move <- diag(n)
patch_pars <- patches_parameters(move = move,bWeightZoo = rep(0,n),bWeightZootox = rep(0,n),reservoir = rep(F,n),res_EIR = rep(0,n))

# mosquitos
mosy_pars <- mosquito_rm_conpars(N = n,lambda = matrix(73.1869,nrow = 365,ncol = n),
                                 psi = diag(n),EIP = rep(11,365),M = rep(450,n),Y = rep(0,n),Z = rep(0,n))

# humans
nh <- rep(2000,n)
pfpr <- rep(0.15,n)

init_state <- c(rep("I",times = nh*pfpr),rep("S",nh*(1-pfpr)))
patch_id <- rep(0,nh)
bweights <- rep(1,nh)

human_pars <- vector("list",nh)
for(i in 1:nh){
  human_pars[[i]] <- human_pfsi_conpars(id = i-1,home_patch_id = patch_id[i],
                                        trip_duration = 1,trip_frequency = 1/1000000,bweight = bweights[i],
                                        age = 20,state = init_state[i])
}
check_human_pfsi_conpars(human_pars)

vaxx_pars <- list()

# run ensemble
nrun <- 1e2
tsteps <- 5e3
pb <- txtProgressBar(min = 1,max = nrun)
for(i in 1:nrun){
  
  seed <- as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31)
  
  log_pars <- list()
  h_move <- paste0(path,"h_move_",i,".csv")
  log_pars[[1]] <- list(outfile = h_move,key = "human_move",
                        header = paste0(c("humanID","time","event","location"),collapse = ","))
  h_inf <- paste0(path,"h_inf_",i,".csv")
  log_pars[[2]] <- list(outfile = h_inf,key = "human_inf",
                        header = paste0(c("humanID","time","state0","state1","location"),collapse = ","))
  mosy <- paste0(path,"mosy_",i,".csv")
  log_pars[[3]] <- list(outfile = mosy,key = "mosquito",
                        header = paste0(c("time","state",paste0("patch",1:n)),collapse = ","))
  run_macro(seed = seed,
            tmax = tsteps,
            human_pars = human_pars,
            mosquito_pars = mosy_pars,
            patch_pars = patch_pars,
            model_pars = pfsi_pars,
            log_streams = log_pars,
            vaxx_events = vaxx_pars,
            verbose = FALSE)
  setTxtProgressBar(pb,i) 
}

# process human output
dx <- 1

# iterate over runs
h_inf_files <- list.files(path = path, pattern= "h_inf*")
mosy_files <- list.files(path = path, pattern= "mosy*")
pb <- txtProgressBar(min = 1,max = length(h_inf_files))
for(i in 1:length(h_inf_files)){
  # read in and process human file
  file <- h_inf_files[i]
  h_inf_csv <- read.csv(file = paste0(path,file),stringsAsFactors = FALSE)
  h_inf_out <- pfsi_human_output(h_inf = h_inf_csv,tmax = tsteps,dx = dx,pb = FALSE)
  
  # read in and process mosquito files
  file <- mosy_files[i]
  mosy_csv <- read.csv(file = paste0(path,file),stringsAsFactors = FALSE)
  mosy_out <- spread(data = mosy_csv,key = state,value = patch1)
  
  # merge & write output
  out_merge <- merge(x = mosy_out,y = h_inf_out,by = "time",all.x = FALSE)[,c("time","M","Y","Z","I")]
  write.table(x = out_merge,file = paste0(path,"output",i,".csv"),dec = ".",sep = ",",col.names = TRUE,row.names = FALSE)
  setTxtProgressBar(pb = pb,value = i)
}


################################################################################
# ensemble run with pfpr 0.17
################################################################################

rm(list=ls());gc()
library(MACRO)
library(tidyr)

# output files
path <- "/Users/slwu89/Desktop/macro/pfpr17/"
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

# pfsi parameters
pfsi_pars <- pfsi_parameters(DurationPf = 200,LatentPf = 0,FeverPf = 0,TreatPf = 0)

# patchs
n <- 1
move <- diag(n)
patch_pars <- patches_parameters(move = move,bWeightZoo = rep(0,n),bWeightZootox = rep(0,n),reservoir = rep(F,n),res_EIR = rep(0,n))

# mosquitos
mosy_pars <- mosquito_rm_conpars(N = n,lambda = matrix(73.1869,nrow = 365,ncol = n),
                                 psi = diag(n),EIP = rep(11,365),M = rep(450,n),Y = rep(0,n),Z = rep(0,n))

# humans
nh <- rep(2000,n)
pfpr <- rep(0.17,n)

init_state <- c(rep("I",times = nh*pfpr),rep("S",nh*(1-pfpr)))
patch_id <- rep(0,nh)
bweights <- rep(1,nh)

human_pars <- vector("list",nh)
for(i in 1:nh){
  human_pars[[i]] <- human_pfsi_conpars(id = i-1,home_patch_id = patch_id[i],
                                        trip_duration = 1,trip_frequency = 1/1000000,bweight = bweights[i],
                                        age = 20,state = init_state[i])
}
check_human_pfsi_conpars(human_pars)

vaxx_pars <- list()

# run ensemble
nrun <- 1e2
tsteps <- 5e3
pb <- txtProgressBar(min = 1,max = nrun)
for(i in 1:nrun){
  
  seed <- as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31)
  
  log_pars <- list()
  h_move <- paste0(path,"h_move_",i,".csv")
  log_pars[[1]] <- list(outfile = h_move,key = "human_move",
                        header = paste0(c("humanID","time","event","location"),collapse = ","))
  h_inf <- paste0(path,"h_inf_",i,".csv")
  log_pars[[2]] <- list(outfile = h_inf,key = "human_inf",
                        header = paste0(c("humanID","time","state0","state1","location"),collapse = ","))
  mosy <- paste0(path,"mosy_",i,".csv")
  log_pars[[3]] <- list(outfile = mosy,key = "mosquito",
                        header = paste0(c("time","state",paste0("patch",1:n)),collapse = ","))
  run_macro(seed = seed,
            tmax = tsteps,
            human_pars = human_pars,
            mosquito_pars = mosy_pars,
            patch_pars = patch_pars,
            model_pars = pfsi_pars,
            log_streams = log_pars,
            vaxx_events = vaxx_pars,
            verbose = FALSE)
  setTxtProgressBar(pb,i) 
}

# process human output
dx <- 1

# iterate over runs
h_inf_files <- list.files(path = path, pattern= "h_inf*")
mosy_files <- list.files(path = path, pattern= "mosy*")
pb <- txtProgressBar(min = 1,max = length(h_inf_files))
for(i in 1:length(h_inf_files)){
  # read in and process human file
  file <- h_inf_files[i]
  h_inf_csv <- read.csv(file = paste0(path,file),stringsAsFactors = FALSE)
  h_inf_out <- pfsi_human_output(h_inf = h_inf_csv,tmax = tsteps,dx = dx,pb = FALSE)
  
  # read in and process mosquito files
  file <- mosy_files[i]
  mosy_csv <- read.csv(file = paste0(path,file),stringsAsFactors = FALSE)
  mosy_out <- spread(data = mosy_csv,key = state,value = patch1)
  
  # merge & write output
  out_merge <- merge(x = mosy_out,y = h_inf_out,by = "time",all.x = FALSE)[,c("time","M","Y","Z","I")]
  write.table(x = out_merge,file = paste0(path,"output",i,".csv"),dec = ".",sep = ",",col.names = TRUE,row.names = FALSE)
  setTxtProgressBar(pb = pb,value = i)
}