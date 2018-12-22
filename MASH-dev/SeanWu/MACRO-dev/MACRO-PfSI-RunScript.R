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
# initial tests
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
nrun <- 100
tsteps <- 2000
pb <- txtProgressBar(min = 1,max = nrun)
for(i in 1:nrun){
  
  seed <- as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31)
  
  log_pars <- list()
  h_move <- paste0(path,"h_move_",i,".csv")
  log_pars[[1]] <- list(outfile = h_move,key = "human_move",
                        header = paste0(c("humanID","time","event","location"),collapse = ","))
  h_inf <- paste0(path,"h_inf_",i,".csv")
  log_pars[[2]] <- list(outfile = h_inf,key = "human_inf",
                        header = paste0(c("humanID","time","event","location"),collapse = ","))
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

# CALCULATE STATISTICS OF THE ENSEMBLE
M.data = matrix(0, nrow = nrun, ncol = tsteps)
Y.data = matrix(0, nrow = nrun, ncol = tsteps)
Z.data = matrix(0, nrow = nrun, ncol = tsteps)
files = list.files(path = path, pattern= "mosy_*")
for (i in 1:nrun){
  file = files[i]
  moshist<- read.csv(paste0(path, "/", file))
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
files = list.files(path = path, pattern= "h_inf*")
for (j in 1:nrun) {
  p <- read.csv(paste0(path, "/", files[j]),stringsAsFactors = F)
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


par(mfrow=c(1,2))

h_col <- "firebrick3"
m_col <- "steelblue"

# compare humans
plot(x = 1:nrow(out),y = out[,"I"],lwd = 2,col = h_col,main = "Human infection prevalence",type="l",
     ylim = c(floor(min(out[,"I"] - out[,"Is"])),ceiling(max(out[,"I"] + out[,"Is"]))),
     xlab = "Time",ylab = "Count")
polygon(x = c(1:nrow(out), rev(1:nrow(out))),
        y = c(out[,"I"] - out[,"Is"], 
              rev(out[,"I"] + out[,"Is"])),
        col =  adjustcolor(h_col, alpha.f = 0.5), border = NA)


# compare mosquitos
plot(x = 1:nrow(out),y = out[,"M"],type = "l",col = m_col,lwd = 2,lty = 1,main= "Mosquito State Variables",
     ylim = c(0,(max(out[,"M"]))+10),
     xlab = "Time",ylab = "Count")
polygon(x = c(1:nrow(out), rev(1:nrow(out))),
        y = c(out[,"M"] - out[,"Ms"], 
              rev(out[,"M"] + out[,"Ms"])),
        col =  adjustcolor(m_col, alpha.f = 0.25), border = NA)

lines(x = 1:nrow(out),y = out[,"Y"],col = m_col,lwd = 2,lty = 2)
polygon(x = c(1:nrow(out), rev(1:nrow(out))),
        y = c(out[,"Y"] - out[,"Ys"], 
              rev(out[,"Y"] + out[,"Ys"])),
        col =  adjustcolor(m_col, alpha.f = 0.25), border = NA)

lines(x = 1:nrow(out),y = out[,"Z"],col = m_col,lwd = 2,lty = 3)
polygon(x = c(1:nrow(out), rev(1:nrow(out))),
        y = c(out[,"Z"] - out[,"Zs"], 
              rev(out[,"Z"] + out[,"Zs"])),
        col =  adjustcolor(m_col, alpha.f = 0.25), border = NA)

par(mfrow=c(1,1))


################################################################################
# sweeping lambda
################################################################################

rm(list=ls());gc()
library(MACRO)

lambda_sweep <- c(2, 5, 10, 20, 100)

# lambda sweep
for(lambda in lambda_sweep){
  
  # Define the directory for the output:
  directory <- paste0("/Users/slwu89/Desktop/macro_sweep_lambda_",lambda,"/")
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
  mosy_pars <- mosquito_rm_conpars(N = n,lambda = matrix(lambda,nrow = 365,ncol = n),
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
  for(i in 1:nrun){
    
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
    
  }
  
}


################################################################################
# process output
################################################################################

pb <- txtProgressBar(min = 1,max = length(lambda_sweep))
for(i in 1:length(lambda_sweep)){
  
  lambda <- lambda_sweep[i]
  directory <- paste0("/Users/slwu89/Desktop/macro_sweep_lambda_",lambda,"/")
  
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
  

  setTxtProgressBar(pb,i)
}