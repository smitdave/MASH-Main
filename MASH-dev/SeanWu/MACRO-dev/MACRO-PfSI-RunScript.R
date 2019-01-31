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


################################################################################
# run a single simulation
################################################################################

rm(list=ls());gc()
library(MACRO)

# output files
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


# pfsi parameters
pfsi_pars <- pfsi_parameters()
# pfsi_pars <- pfsi_parameters(DurationPf = 200,LatentPf = 0,FeverPf = 0,TreatPf = 0)

# patchs
n <- 1
move <- diag(n)
patch_pars <- patches_parameters(move = move,bWeightZoo = rep(0,n),bWeightZootox = rep(0,n),reservoir = rep(F,n),res_EIR = rep(0,n))

# mosquitos
mosy_pars <- mosquito_rm_conpars(N = n,lambda = matrix(50,nrow = 365,ncol = n),
                                 psi = diag(n),EIP = rep(11,365),M = rep(450,n),Y = rep(0,n),Z = rep(0,n))

# humans
nh <- rep(500,n)
pfpr <- rep(0.5,n)

init_state <- c(sample(x = c("I","S"),size = nh,replace = T,prob = c(pfpr[1],1-pfpr[1])))
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

# run single trajectory
seed <- as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31)

log_pars <- list()
h_move <- paste0(path,"h_move.csv")
log_pars[[1]] <- list(outfile = h_move,key = "human_move",
                      header = paste0(c("humanID","time","event","location"),collapse = ","))
h_inf <- paste0(path,"h_inf.csv")
log_pars[[2]] <- list(outfile = h_inf,key = "human_inf",
                      header = paste0(c("humanID","time","state0","state1","location"),collapse = ","))
mosy <- paste0(path,"mosy.csv")
log_pars[[3]] <- list(outfile = mosy,key = "mosquito",
                      header = paste0(c("time","state",paste0("patch",1:n)),collapse = ","))

run_macro(seed = seed,
          tmax = 1e4,
          human_pars = human_pars,
          mosquito_pars = mosy_pars,
          patch_pars = patch_pars,
          model_pars = pfsi_pars,
          log_streams = log_pars,
          vaxx_events = vaxx_pars,
          verbose = TRUE)

# process output
h_inf_csv <- read.csv(file = log_pars[[2]]$outfile,stringsAsFactors = FALSE)
h_inf_out <- pfsi_human_output(h_inf = h_inf_csv,dx = 10,tmax = 1e4,pb = T)


################################################################################
# run an ensemble
################################################################################

rm(list=ls())
library(MACRO)

# output files
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

# pfsi parameters
pfsi_pars <- pfsi_parameters()

# patchs
n <- 1
move <- diag(n)
patch_pars <- patches_parameters(move = move,bWeightZoo = rep(0,n),bWeightZootox = rep(0,n),reservoir = rep(F,n),res_EIR = rep(0,n))

# mosquitos
mosy_pars <- mosquito_rm_conpars(N = n,lambda = matrix(50,nrow = 365,ncol = n),
                                 psi = diag(n),EIP = rep(11,365),M = rep(450,n),Y = rep(0,n),Z = rep(0,n))

# humans
nh <- rep(500,n)
pfpr <- rep(0.5,n)

init_state <- c(sample(x = c("I","S"),size = nh,replace = T,prob = c(pfpr[1],1-pfpr[1])))
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
tsteps <- 2e3
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

# read in data
dx <- 5
statenames <- c("S","I","P","F","PEvaxx","GSvaxx","PEwane","GSwane")
nstate <- length(statenames) # for PfSI
tbins <- c(0,seq(from=1,to=tsteps+1,by=dx))
nbin <- length(tbins)

h_inf_out <- array(data = 0,dim = c(nbin,nstate,nrun),
                   dimnames = list(tbins,statenames,1:nrun))

# iterate over runs
h_inf_files <- list.files(path = path, pattern= "h_inf*")
pb <- txtProgressBar(min = 1,max = length(h_inf_files))
for(i in 1:length(h_inf_files)){
  file <- h_inf_files[i]
  h_inf_csv <- read.csv(file = paste0(path,file),stringsAsFactors = FALSE)
  h_inf_out[,,as.character(i)] <- pfsi_human_output(h_inf = h_inf_csv,tmax = tsteps,dx = dx,pb = FALSE)
  setTxtProgressBar(pb = pb,value = i)
}

h_inf_mean <- apply(X = h_inf_out,MARGIN = c(1,2),FUN = mean)
h_inf_sd <- apply(X = h_inf_out,MARGIN = c(1,2),FUN = sd)

cols <- c("S"="steelblue","I"="firebrick3","P"="darkorchid")
alpha <- 0.5

ymax <- ceiling(max(h_inf_mean+h_inf_sd))+10

plot(x = tbins,y = h_inf_mean[,"S"],lwd = 2,col = cols["S"],main = "PfSI Ensemble",type = "l",
     ylim = c(0,ymax),xlab = "Time (days)",ylab = "Count")
polygon(x = c(tbins,rev(tbins)),
        y = c(h_inf_mean[,"S"] - h_inf_sd[,"S"],
              rev(h_inf_mean[,"S"] + h_inf_sd[,"S"])),
        col = adjustcolor(cols["S"],alpha.f = alpha),border = NA)

lines(x = tbins,y = h_inf_mean[,"I"],lwd = 2,lty = 1,col = cols["I"])
polygon(x = c(tbins,rev(tbins)),
        y = c(h_inf_mean[,"I"] - h_inf_sd[,"I"],
              rev(h_inf_mean[,"I"] + h_inf_sd[,"I"])),
        col = adjustcolor(cols["I"],alpha.f = alpha),border = NA)

lines(x = tbins,y = h_inf_mean[,"P"],lwd = 2,lty = 1,col = cols["P"])
polygon(x = c(tbins,rev(tbins)),
        y = c(h_inf_mean[,"P"] - h_inf_sd[,"P"],
              rev(h_inf_mean[,"P"] + h_inf_sd[,"P"])),
        col = adjustcolor(cols["P"],alpha.f = alpha),border = NA)