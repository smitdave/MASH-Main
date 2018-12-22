################################################################################
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   Test C++ vs. R implementation
#
#   Sean Wu
#   December 2018
#
################################################################################


################################################################################
# R code validation
# from the file:
# /Users/slwu89/Desktop/git/MASH-Main/MASH-dev/DanielCitron/MACRO-model-alignment/MACRO-PfSI-alignment_run_2_ensemble.R
################################################################################

rm(list=ls());gc()
library(MASHmacro) # R package

# Define the directory for the output:
directory <- "/Users/slwu89/Desktop/macro_validation_R/"
# if(!dir.exists(directory)){
#   dir.create(directory)
# } else {
#   files <- list.files(directory)
#   if(length(files) > 0){
#     for(f in files){
#       file.remove(paste0(directory,f))
#     }
#   }
# }

# Set the random seed
set.seed(0)

# PFSI MODULE PARAMETERS
PfSI.Setup(DurationPf = 200, LatentPf = 0, FeverPf = 0, TreatPf = 0)
SimBitePfSI.Setup()
MACRO.Human.Setup(pathogen = "PfSI",tripFrequency = 1/1000000, tripDuration = 1)

# PATCH PARAMETERS
# Number of Patches
n = 1

# aquatic ecology parameters
aquaPar = AquaPop_Emerge.Parameters(nPatch = n,lambda = rep(50,n),seasonality = FALSE)

# Human movement matrix (in general, for n>1, make movement follow AR(1) covariance structure, then subtract out the diagonal and renormalize)
moveMat = diag(1, n)
patchPar = lapply(X = 1:n,FUN = function(i){
  list(
    bWeightZoo = 0,
    bWeightZootox = 0,
    travelWeight = moveMat[i,],
    reservoir = FALSE,
    resEIR = NULL
  )
})

# MOSQUITO PARAMTERS
mosquitoPar = list(model="RM", M=rep(450),EIP = rep(11,365),
                   Y = 0, Z = 0,
                   p=0.9, f=0.3, Q=0.9, v=20, psi = diag(n))


# HUMAN PARAMETERS
# Number of humans in each patch
patch_humans = rep(500, n)
# Total number of humans
n_humans = sum(patch_humans)
# Assign patch and home IDs to humans
patch_id = rep(x = 1:n,patch_humans)
home_id = rep(x = 1:n,patch_humans)
human_ages = unlist(lapply(X = patch_humans,FUN = siteAges_HumanPop))
# Set biting weights
human_bWeight = rep(1, n_humans)

# Generate lists of parameters
humanPar = lapply(X = 1:n_humans,function(i){
  list(
    houseID = home_id[i],
    patchID = patch_id[i],
    homeHouseID = home_id[i],
    homePatchID = patch_id[i],
    age = human_ages[i],
    bWeight = human_bWeight[i],
    tripDuration = c(1),# this is if we use TaR matrix 1
    tripFrequency = 1/1000000
    
  )
})

# PfPR - defines initial number infected
pfpr = rep(0.9,n)

# GENERATE TILE
tile = MacroTile$new(nPatch = n,
                     AquaPar = aquaPar,
                     PatchPar = patchPar,
                     MosquitoPar = mosquitoPar,
                     HumanPar = humanPar,
                     directory = directory)

# RUN AN ENSEMBLE OF SIMULATIONS
nrun = 100
tsteps = 2000
for (i in 1:nrun){
  tile$simMacro(tMax = tsteps, PfPAR = pfpr)
  tile$resetMacro(PatchPar = patchPar,MosquitoPar = mosquitoPar,HumanPar = humanPar)
}

# CALCULATE STATISTICS OF THE ENSEMBLE
M.data = matrix(0, nrow = nrun, ncol = tsteps)
Y.data = matrix(0, nrow = nrun, ncol = tsteps)
Z.data = matrix(0, nrow = nrun, ncol = tsteps)
files = list.files(path = directory, pattern= "Mosquito_Run*")
for (i in 1:nrun){
  file = files[i]
  moshist<- read.csv(paste0(directory, "/", file))
  M <- moshist$patch1[moshist$state == "M"]
  Y <- moshist$patch1[moshist$state == "Y"]
  Z <- moshist$patch1[moshist$state == "Z"]
  # Save mos data
  M.data[i,] <- M[-1]
  Y.data[i,] <- Y[-1]
  Z.data[i,] <- Z[-1]
}
M.mean <- sapply(X = 1:tsteps, FUN = function(X){return(mean(M.data[,X]))})
Y.mean <- sapply(X = 1:tsteps, FUN = function(X){return(mean(Y.data[,X]))})
Z.mean <- sapply(X = 1:tsteps, FUN = function(X){return(mean(Z.data[,X]))})

M.std <- sapply(X = 1:tsteps, FUN = function(X){return(sd(M.data[,X]))})
Y.std <- sapply(X = 1:tsteps, FUN = function(X){return(sd(Y.data[,X]))})
Z.std <- sapply(X = 1:tsteps, FUN = function(X){return(sd(Z.data[,X]))})

I.data = matrix(0, nrow = nrun, ncol = tsteps+1)
files = list.files(path = directory, pattern= "HumanPathogen_Run*")
for (j in 1:nrun) {
  p <- read.csv(paste0(directory, "/", files[j]))
  e <- table(data.frame(time = ceiling(p$time), event = p$event))
  h <- matrix(0, nrow = tsteps+1, ncol = 2)
  for (i in 1:nrow(e)){
    tIndex <- as.numeric(rownames(e))[i] + 1
    h[tIndex,1] <- e[i,1] # infecteds
    h[tIndex,2] <- e[i,2] # susceptibles
  }
  I.data[j,] = append(cumsum(h[1,1]), cumsum(h[1,1]) + cumsum(h[-1,1]) - cumsum(h[-1,2]))
}
I.data.mean <- sapply(X = 1:tsteps, FUN = function(X){return(mean(I.data[,X]))})
I.data.std <- sapply(X = 1:tsteps, FUN = function(X){return(sd(I.data[,X]))})

# write output
out <- data.frame(M = M.mean, Y = Y.mean, Z = Z.mean, I = I.data.mean,
                  Ms = M.std, Ys = Y.std, Zs = Z.std, Is = I.data.std)
write.table(out, file = paste0(directory,"/ensemble_average_data.csv"), sep=",",row.names = FALSE)


################################################################################
# C++ code validation
################################################################################

rm(list=ls());gc()
library(MACRO) # C++ package

# Define the directory for the output:
directory <- "/Users/slwu89/Desktop/macro_validation_CPP/"
if(!dir.exists(directory)){
  dir.create(directory)
} else {
  files <- list.files(directory)
  if(length(files) > 0){
    for(f in files){
      file.remove(paste0(directory,f))
    }
  }
}

seed <- 0L

# pfsi parameters
pfsi_pars <- pfsi_parameters(DurationPf = 200,LatentPf = 0,FeverPf = 0,TreatPf = 0)

# patchs
n <- 1
move <- diag(n)
patch_pars <- patches_parameters(move = move,bWeightZoo = rep(0,n),bWeightZootox = rep(0,n),reservoir = rep(F,n),res_EIR = rep(0,n))

# mosquitos
mosy_pars <- mosquito_rm_conpars(N = n,lambda = matrix(50,nrow = 365,ncol = n),
                                 psi = diag(n),EIP = rep(11,365),M = rep(450,n),Y = rep(0,n),Z = rep(0,n))

# humans
nh <- rep(500,n)
pfpr <- rep(0.9,n)

inf_bool <- c(sample(x = c(T,F),size = nh,replace = T,prob = c(pfpr[1],1-pfpr[1])))
patch_id <- rep(0,nh)
bweights <- rep(1,nh)

human_pars <- vector("list",nh)
for(i in 1:nh){
  human_pars[[i]] <- human_pfsi_conpars(id = i-1,home_patch_id = patch_id[i],
                                        trip_duration = 1,trip_frequency = 1/1000000,bweight = bweights[i],
                                        age = 20,inf = inf_bool[i],chx = F)
}
check_human_pfsi_conpars(human_pars)

vaxx_pars <- list()

# run ensemble
nrun <- 1000
tsteps <- 2000
system.time(for (i in 1:nrun){
  
  seed <- as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31)
  
  log_pars <- list()
  h_move <- paste0(directory,"h_move_",i,".csv")
  log_pars[[1]] <- list(outfile = h_move,key = "human_move",
                        header = paste0(c("humanID","time","event","location"),collapse = ","))
  h_inf <- paste0(directory,"h_inf_",i,".csv")
  log_pars[[2]] <- list(outfile = h_inf,key = "human_inf",
                        header = paste0(c("humanID","time","event","location"),collapse = ","))
  mosy <- paste0(directory,"mosy_",i,".csv")
  log_pars[[3]] <- list(outfile = mosy,key = "mosquito",
                        header = paste0(c("time","state",paste0("patch",1:n)),collapse = ","))
  run_macro(seed = seed,
            tmax = tsteps,
            human_pars = human_pars,
            mosquito_pars = mosy_pars,
            patch_pars = patch_pars,
            model_pars = pfsi_pars,
            log_streams = log_pars,
            vaxx_events = vaxx_pars)
  
})

# CALCULATE STATISTICS OF THE ENSEMBLE
M.data = matrix(0, nrow = nrun, ncol = tsteps)
Y.data = matrix(0, nrow = nrun, ncol = tsteps)
Z.data = matrix(0, nrow = nrun, ncol = tsteps)
files = list.files(path = directory, pattern= "mosy_*")
for (i in 1:nrun){
  file = files[i]
  moshist<- read.csv(paste0(directory, "/", file))
  M <- moshist$patch1[moshist$state == "M"]
  Y <- moshist$patch1[moshist$state == "Y"]
  Z <- moshist$patch1[moshist$state == "Z"]
  # Save mos data
  M.data[i,] <- M[-1]
  Y.data[i,] <- Y[-1]
  Z.data[i,] <- Z[-1]
}
M.mean <- sapply(X = 1:tsteps, FUN = function(X){return(mean(M.data[,X]))})
Y.mean <- sapply(X = 1:tsteps, FUN = function(X){return(mean(Y.data[,X]))})
Z.mean <- sapply(X = 1:tsteps, FUN = function(X){return(mean(Z.data[,X]))})

M.std <- sapply(X = 1:tsteps, FUN = function(X){return(sd(M.data[,X]))})
Y.std <- sapply(X = 1:tsteps, FUN = function(X){return(sd(Y.data[,X]))})
Z.std <- sapply(X = 1:tsteps, FUN = function(X){return(sd(Z.data[,X]))})

I.data = matrix(0, nrow = nrun, ncol = tsteps+1)
files = list.files(path = directory, pattern= "h_inf*")
for (j in 1:nrun) {
  p <- read.csv(paste0(directory, "/", files[j]))
  e <- table(data.frame(time = ceiling(p$time), event = p$event))
  h <- matrix(0, nrow = tsteps+1, ncol = 2)
  for (i in 1:nrow(e)){
    tIndex <- as.numeric(rownames(e))[i] + 1
    h[tIndex,1] <- e[i,1] # infecteds
    h[tIndex,2] <- e[i,2] # susceptibles
  }
  I.data[j,] = append(cumsum(h[1,1]), cumsum(h[1,1]) + cumsum(h[-1,1]) - cumsum(h[-1,2]))
}
I.data.mean <- sapply(X = 1:tsteps, FUN = function(X){return(mean(I.data[,X]))})
I.data.std <- sapply(X = 1:tsteps, FUN = function(X){return(sd(I.data[,X]))})

# write output
out <- data.frame(M = M.mean, Y = Y.mean, Z = Z.mean, I = I.data.mean,
                  Ms = M.std, Ys = Y.std, Zs = Z.std, Is = I.data.std)
write.table(out, file = paste0(directory,"/ensemble_average_data.csv"), sep=",",row.names = FALSE)


################################################################################
# compare output
################################################################################

library(data.table)
rm(list=ls());gc()
dir_R <- "/Users/slwu89/Desktop/macro_validation_R/"
dir_CPP <- "/Users/slwu89/Desktop/macro_validation_CPP/"

out_R <- fread(file = paste0(dir_R,"ensemble_average_data.csv"))
out_CPP <- fread(file = paste0(dir_CPP,"ensemble_average_data.csv"))

# par(mfrow=c(2,2))
# 
# # C++ mosquitos
# plot(x = 1:nrow(out_CPP),y = out_CPP[,"M"][[1]],type = "l",col = "red",lwd = 2,main= "mosquitos C++",ylim = c(0,(max(out_CPP[,"M"][[1]]))+10))
# lines(x = 1:nrow(out_CPP),y = out_CPP[,"Y"][[1]],col = "blue",lwd = 2)
# lines(x = 1:nrow(out_CPP),y = out_CPP[,"Z"][[1]],col = "purple",lwd = 2)
# 
# # C++ humans
# plot(x = 1:nrow(out_CPP),y = out_CPP[,"I"][[1]],lwd = 2,col = "red",main = "humans C++",type="l",
#      ylim = c(floor(min(out_CPP[,"I"][[1]] - out_CPP[,"Is"][[1]])),ceiling(max(out_CPP[,"I"][[1]] + out_CPP[,"Is"][[1]]))))
# polygon(x = c(1:nrow(out_CPP), rev(1:nrow(out_CPP))),
#         y = c(out_CPP[,"I"][[1]] - out_CPP[,"Is"][[1]], 
#               rev(out_CPP[,"I"][[1]] + out_CPP[,"Is"][[1]])),
#         col =  adjustcolor("red", alpha.f = 0.5), border = NA)
# 
# # R mosquitos
# plot(x = 1:nrow(out_R),y = out_R[,"M"][[1]],type = "l",col = "red",lwd = 2,main= "mosquitos R",ylim = c(0,(max(out_R[,"M"][[1]]))+10))
# lines(x = 1:nrow(out_R),y = out_R[,"Y"][[1]],col = "blue",lwd = 2)
# lines(x = 1:nrow(out_R),y = out_R[,"Z"][[1]],col = "purple",lwd = 2)
# 
# # R humans
# plot(x = 1:nrow(out_R),y = out_R[,"I"][[1]],lwd = 2,col = "red",main = "humans R",type="l",
#      ylim = c(floor(min(out_R[,"I"][[1]] - out_R[,"Is"][[1]])),ceiling(max(out_R[,"I"][[1]] + out_R[,"Is"][[1]]))),
#      xlab = "Time",ylab = "Count")
# polygon(x = c(1:nrow(out_R), rev(1:nrow(out_R))),
#         y = c(out_R[,"I"][[1]] - out_R[,"Is"][[1]], 
#               rev(out_R[,"I"][[1]] + out_R[,"Is"][[1]])),
#         col =  adjustcolor("red", alpha.f = 0.5), border = NA)
# 
# par(mfrow=c(1,1))

par(mfrow=c(1,2))

# compare humans
plot(x = 1:nrow(out_CPP),y = out_CPP[,"I"][[1]],lwd = 2,col = "red",main = "humans C++: red, R: blue",type="l",
     ylim = c(floor(min(out_CPP[,"I"][[1]] - out_CPP[,"Is"][[1]])),ceiling(max(out_CPP[,"I"][[1]] + out_CPP[,"Is"][[1]]))),
     xlab = "Time",ylab = "Count")
lines(x = 1:nrow(out_R),y = out_R[,"I"][[1]],lwd = 2,col = "blue")
polygon(x = c(1:nrow(out_CPP), rev(1:nrow(out_CPP))),
        y = c(out_CPP[,"I"][[1]] - out_CPP[,"Is"][[1]], 
              rev(out_CPP[,"I"][[1]] + out_CPP[,"Is"][[1]])),
        col =  adjustcolor("red", alpha.f = 0.5), border = NA)
polygon(x = c(1:nrow(out_R), rev(1:nrow(out_R))),
        y = c(out_R[,"I"][[1]] - out_R[,"Is"][[1]], 
              rev(out_R[,"I"][[1]] + out_R[,"Is"][[1]])),
        col =  adjustcolor("blue", alpha.f = 0.5), border = NA)

# compare mosquitos
plot(x = 1:nrow(out_CPP),y = out_CPP[,"M"][[1]],type = "l",col = "red",lwd = 2,lty = 1,main= "mosquitos C++:red, R:blue",
     ylim = c(0,(max(out_CPP[,"M"][[1]]))+10),
     xlab = "Time",ylab = "Count")
polygon(x = c(1:nrow(out_CPP), rev(1:nrow(out_CPP))),
        y = c(out_CPP[,"M"][[1]] - out_CPP[,"Ms"][[1]], 
              rev(out_CPP[,"M"][[1]] + out_CPP[,"Ms"][[1]])),
        col =  adjustcolor("red", alpha.f = 0.25), border = NA)

lines(x = 1:nrow(out_CPP),y = out_CPP[,"Y"][[1]],col = "red",lwd = 2,lty = 2)
polygon(x = c(1:nrow(out_CPP), rev(1:nrow(out_CPP))),
        y = c(out_CPP[,"Y"][[1]] - out_CPP[,"Ys"][[1]], 
              rev(out_CPP[,"Y"][[1]] + out_CPP[,"Ys"][[1]])),
        col =  adjustcolor("red", alpha.f = 0.25), border = NA)

lines(x = 1:nrow(out_CPP),y = out_CPP[,"Z"][[1]],col = "red",lwd = 2,lty = 3)
polygon(x = c(1:nrow(out_CPP), rev(1:nrow(out_CPP))),
        y = c(out_CPP[,"Z"][[1]] - out_CPP[,"Zs"][[1]], 
              rev(out_CPP[,"Z"][[1]] + out_CPP[,"Zs"][[1]])),
        col =  adjustcolor("red", alpha.f = 0.25), border = NA)

lines(x = 1:nrow(out_R),y = out_R[,"M"][[1]],col = "blue",lwd = 2,lty = 1)
polygon(x = c(1:nrow(out_R), rev(1:nrow(out_R))),
        y = c(out_R[,"M"][[1]] - out_R[,"Ms"][[1]], 
              rev(out_R[,"M"][[1]] + out_R[,"Ms"][[1]])),
        col =  adjustcolor("blue", alpha.f = 0.25), border = NA)

lines(x = 1:nrow(out_R),y = out_R[,"Y"][[1]],col = "blue",lwd = 2,lty = 2)
polygon(x = c(1:nrow(out_R), rev(1:nrow(out_R))),
        y = c(out_R[,"Y"][[1]] - out_R[,"Ys"][[1]], 
              rev(out_R[,"Y"][[1]] + out_R[,"Ys"][[1]])),
        col =  adjustcolor("blue", alpha.f = 0.25), border = NA)

lines(x = 1:nrow(out_R),y = out_R[,"Z"][[1]],col = "blue",lwd = 2,lty = 3)
polygon(x = c(1:nrow(out_R), rev(1:nrow(out_R))),
        y = c(out_R[,"Z"][[1]] - out_R[,"Zs"][[1]], 
              rev(out_R[,"Z"][[1]] + out_R[,"Zs"][[1]])),
        col =  adjustcolor("blue", alpha.f = 0.25), border = NA)

par(mfrow=c(1,1))