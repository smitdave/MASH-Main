# sample of MACRO (metapopulation) model with PfSI infection module
# include human movement
# include simple mosquito model (difference equations) with diffusion between patches


# clear the R workspace
rm(list=ls());gc()

# here package to get paths right between different systems
library(here)

# need these 3 packages of the Rcpp ecosystem
# Rcpp: the main R to C++ interface package
# RcppArmadillo: the mosquito model relies on matrix algebra, this lets us use those headers
# RcppProgress: header for an interruptable progress bar
library(Rcpp)
library(RcppArmadillo)
library(RcppProgress)

# a path to put output output files
# it makes a sub-directory if it doesnt exist, and if it does exist, it clears it out
root <- "/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/SeanWu/MACRO-dev/macro-pfsi"
path <- paste0(root,"/output/")
# path <- here::here("output/")
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

# source the helper R code (mostly just sets up the R lists that get fed to the C++ simulation)
# source(here::here("R/Human-PfSI.R"))
# source(here::here("R/MACRO-Utility.R"))
# source(here::here("R/Mosquito-RM.R"))
# source(here::here("R/Patch.R"))
# source(here::here("R/PfSI-Parameters.R"))
# source(here::here("R/PfSI-Utility.R"))

source(paste0(root,"/R/Human-PfSI.R"))
source(paste0(root,"/R/MACRO-Utility.R"))
source(paste0(root,"/R/Mosquito-RM.R"))
source(paste0(root,"/R/Patch.R"))
source(paste0(root,"/R/PfSI-Parameters.R"))
source(paste0(root,"/R/PfSI-Utility.R"))

# vector of parameters
pfsi_pars <- pfsi_parameters()

# set up patches (n is how many patches we have)
n <- 5
# movement matrix for humans is uniform over possible destinations.
move <- matrix(data = 1/(n-1),nrow = n,ncol = n)
diag(move) <- 0
patch_pars <- patches_parameters(move = move,bWeightZoo = rep(0,n),bWeightZootox = rep(0,n),reservoir = rep(F,n),res_EIR = rep(0,n))

# mosquitos
mosy_pars <- mosquito_rm_conpars(N = n,lambda = matrix(50,nrow = 365,ncol = n),
                                 psi = diag(n),EIP = rep(11,365),M = rep(450,n),Y = rep(0,n),Z = rep(0,n))

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
                                        trip_duration = rpois(n = n,lambda = 10),trip_frequency = 1/20,bweight = bweights[i],
                                        age = 20,state = init_state[i])
}
check_human_pfsi_conpars(human_pars)

# vaccinations (can uncomment the code below to vaccinate 25% of the population at day 500)
# vaxx_pars <- list()
vaxx_id <- sample(x = 0:(nh-1),size = nh*0.25,replace = F)
vaxx_pars <- lapply(X = vaxx_id,FUN = function(id){
  vaccination_pfsi_conpars(id = id,t = 5e2,treat = T,type = "PE")
})

# compile the simulation
# sourceCpp(here::here("src/main.cpp"))
sourceCpp(paste0(root,"/src/main.cpp"))

# run ensemble
nrun <- 1e2
tsteps <- 1e3
# pb <- txtProgressBar(min = 1,max = nrun)
# for(i in 1:nrun){
  i=1
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
            verbose = TRUE)
 # setTxtProgressBar(pb,i)
# }

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
  out_i <- pfsi_human_output(h_inf = h_inf_csv,tmax = tsteps,dx = dx,pb = FALSE)
  h_inf_out[,,as.character(i)] <- out_i[,-1]
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
