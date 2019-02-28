################################################################################
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
# 
#   Sean Wu & Daniel Citron
#   Replication of results from Uganda PRISM Study
#   February 2019
# 
################################################################################


################################################################################
#   existing data and target
################################################################################

rm(list=ls());gc()

library(here)
library(MACRO)

# read in data
tororo <- read.table(here("data/TOROROnew.txt"), header=T)
jinja <- read.table (here("data/JINJAnew.txt"), header=T)
kanungu <- read.table(here("data/KANUNGUnew.txt"), header=T)
load(here("data/data_plot_dave.Rdata"))

# plot
getEff = function(loc){with(data.plot,{
  ix = which(site==loc)
  eir = 10^log10_eir[ix]
  foi = yearly_rate[ix]
  mean(eir/foi)
})} 

with(data.plot,{
  eir = 10^log10_eir
  aeff = eir/yearly_rate
  plot(eir, aeff, type = "n", xlab = "Annual EIR", ylab =
         "Inefficiency (aEIR : aFOI)", main = "d)", xaxt = "n", yaxt = "n", log = "xy")
  axis(1, 10^c(0, 1, 2, 3), c(1,10,100, 1000)) 
  axis(2, c(1/2, 2, 10, 50), c("1:2","2:1","10:1","50:1")) 
  
  # plotting parameters
  ccx =0.9
  tpc = 16
  kpc = 4
  jpc = 18
  cx = .8
  pc = 20
  
  text(10,70,"Exposure vs. Infection", cex = 1.2)  
  
  ix = which(site=="Kanungu")
  points(eir[ix], aeff[ix], col = "darkblue", pch=kpc)
  
  ix = which(site=="Tororo")
  points(eir[ix], aeff[ix], col = "darkred", pch=tpc)
  
  ix = which(site=="Jinja")
  points(eir[ix], aeff[ix], col = "darkgreen", pch=jpc)
  
  llm=lm(log(aeff)~log(eir))
  xx = exp(seq(0, 7, length.out=20))
  a = exp(coef(llm)[1]) 
  b = coef(llm)[2]
  lines(xx, a*xx^b)
  print(c(a,b))
})

# gamma_t <- MASS::fitdistr(x = tororo$w,densfun = "gamma")
lnorm_t <- MASS::fitdistr(x = tororo$w,densfun = "log-normal")
# gamma_k <- MASS::fitdistr(x = kanungu$w,densfun = "gamma")
lnorm_k <- MASS::fitdistr(x = kanungu$w,densfun = "log-normal")
# gamma_j <- MASS::fitdistr(x = jinja$w,densfun = "gamma")
lnorm_j <- MASS::fitdistr(x = jinja$w,densfun = "log-normal")


################################################################################
#   MACRO simulations: Kanungu
################################################################################

# EIRs for each house (consider each house a node)
EIR_k <- (10^data.plot[data.plot$site=="Kanungu","log10_eir"])/365

# output files
path <- path_k <- "/Users/slwu89/Desktop/macro-kanungu/"
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

pfsi_pars <- pfsi_parameters(DurationPf = 200,LatentPf = 0,FeverPf = 0,TreatPf = 0)

# patches
n <- length(EIR_k)
move <- diag(n)
patch_pars <- patches_parameters(move = move,bWeightZoo = rep(0,n),bWeightZootox = rep(0,n),reservoir = rep(T,n),res_EIR = EIR_k)

# mosquitos
mosy_pars <- mosquito_rm_conpars(N = n,lambda = matrix(0,nrow = 365,ncol = n),
                                 psi = diag(n),EIP = rep(11,365),M = rep(0,n),Y = rep(0,n),Z = rep(0,n))

# humans
nh_house <- nh_house_k <- 1e3
nh <- nh_house*n

patch_id <- rep(1:n,each=nh_house)-1
# bweights <- rep(1,nh)
# bweights <- rgamma(n = nh,shape = gamma_k$estimate[["shape"]],rate = gamma_k$estimate[["rate"]])
bweights <- rlnorm(n = nh,meanlog = lnorm_k$estimate[["meanlog"]],sdlog = lnorm_k$estimate[["sdlog"]])

human_pars <- vector("list",nh)
for(i in 1:nh){
  human_pars[[i]] <- human_pfsi_conpars(id = i-1,home_patch_id = patch_id[i],
                                        trip_duration = 1,trip_frequency = 1/1000000,bweight = bweights[i],
                                        age = 20,state = "S")
}
check_human_pfsi_conpars(human_pars)

vaxx_pars <- list()

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
          tmax = 365,
          human_pars = human_pars,
          mosquito_pars = mosy_pars,
          patch_pars = patch_pars,
          model_pars = pfsi_pars,
          log_streams = log_pars,
          vaxx_events = vaxx_pars,
          verbose = TRUE)


################################################################################
#   MACRO simulations: Tororo
################################################################################

EIR_t <- (10^data.plot[data.plot$site=="Tororo","log10_eir"])/365

# output files
path <- path_t <- "/Users/slwu89/Desktop/macro-tororo/"
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

pfsi_pars <- pfsi_parameters(DurationPf = 200,LatentPf = 0,FeverPf = 0,TreatPf = 0)

# patches
n <- length(EIR_t)
move <- diag(n)
patch_pars <- patches_parameters(move = move,bWeightZoo = rep(0,n),bWeightZootox = rep(0,n),reservoir = rep(T,n),res_EIR = EIR_t)

# mosquitos
mosy_pars <- mosquito_rm_conpars(N = n,lambda = matrix(0,nrow = 365,ncol = n),
                                 psi = diag(n),EIP = rep(11,365),M = rep(0,n),Y = rep(0,n),Z = rep(0,n))

# humans
nh_house <- nh_house_t <- 1e3
nh <- nh_house*n

patch_id <- rep(1:n,each=nh_house)-1
# bweights <- rep(1,nh)
# bweights <- rgamma(n = nh,shape = gamma_t$estimate[["shape"]],rate = gamma_t$estimate[["rate"]])
bweights <- rlnorm(n = nh,meanlog = lnorm_t$estimate[["meanlog"]],sdlog = lnorm_t$estimate[["sdlog"]])

human_pars <- vector("list",nh)
for(i in 1:nh){
  human_pars[[i]] <- human_pfsi_conpars(id = i-1,home_patch_id = patch_id[i],
                                        trip_duration = 1,trip_frequency = 1/1000000,bweight = bweights[i],
                                        age = 20,state = "S")
}
check_human_pfsi_conpars(human_pars)

vaxx_pars <- list()

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
          tmax = 365,
          human_pars = human_pars,
          mosquito_pars = mosy_pars,
          patch_pars = patch_pars,
          model_pars = pfsi_pars,
          log_streams = log_pars,
          vaxx_events = vaxx_pars,
          verbose = TRUE)


################################################################################
#   MACRO simulations: Jinja
################################################################################

EIR_j <- (10^data.plot[data.plot$site=="Jinja","log10_eir"])/365

# output files
path <- path_j <- "/Users/slwu89/Desktop/macro-jinja/"
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

pfsi_pars <- pfsi_parameters(DurationPf = 200,LatentPf = 0,FeverPf = 0,TreatPf = 0)

# patches
n <- length(EIR_j)
move <- diag(n)
patch_pars <- patches_parameters(move = move,bWeightZoo = rep(0,n),bWeightZootox = rep(0,n),reservoir = rep(T,n),res_EIR = EIR_j)

# mosquitos
mosy_pars <- mosquito_rm_conpars(N = n,lambda = matrix(0,nrow = 365,ncol = n),
                                 psi = diag(n),EIP = rep(11,365),M = rep(0,n),Y = rep(0,n),Z = rep(0,n))

# humans
nh_house <- nh_house_j <- 1e3
nh <- nh_house*n

patch_id <- rep(1:n,each=nh_house)-1
# bweights <- rep(1,nh)
# bweights <- rgamma(n = nh,shape = gamma_j$estimate[["shape"]],rate = gamma_j$estimate[["rate"]])
bweights <- rlnorm(n = nh,meanlog = lnorm_j$estimate[["meanlog"]],sdlog = lnorm_j$estimate[["sdlog"]])

human_pars <- vector("list",nh)
for(i in 1:nh){
  human_pars[[i]] <- human_pfsi_conpars(id = i-1,home_patch_id = patch_id[i],
                                        trip_duration = 1,trip_frequency = 1/1000000,bweight = bweights[i],
                                        age = 20,state = "S")
}
check_human_pfsi_conpars(human_pars)

vaxx_pars <- list()

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
          tmax = 365,
          human_pars = human_pars,
          mosquito_pars = mosy_pars,
          patch_pars = patch_pars,
          model_pars = pfsi_pars,
          log_streams = log_pars,
          vaxx_events = vaxx_pars,
          verbose = TRUE)


################################################################################
#   read in simulated data
################################################################################

# process output into 14-day FOI (attack rates)
out_k <- read.csv(file = paste0(path_k,"h_inf.csv"),stringsAsFactors = FALSE)
out_t <- read.csv(file = paste0(path_t,"h_inf.csv"),stringsAsFactors = FALSE)
out_j <- read.csv(file = paste0(path_j,"h_inf.csv"),stringsAsFactors = FALSE)

ar_k <- sapply(X = unique(out_k$location),FUN = function(id){
  sum(out_k$state0=="S" & out_k$state1=="I" & out_k$location==id)
})
ar_k_14 <- ar_k/nh_house_k/14

ar_t <- sapply(X = unique(out_t$location),FUN = function(id){
  sum(out_t$state0=="S" & out_t$state1=="I" & out_t$location==id)
})
ar_t_14 <- ar_t/nh_house_t/14

ar_j <- sapply(X = unique(out_j$location),FUN = function(id){
  sum(out_j$state0=="S" & out_j$state1=="I" & out_j$location==id)
})
ar_j_14 <- ar_j/nh_house_j/14

tpc = 16
kpc = 4
jpc = 18
site_cols <- c("darkblue","darkred","darkgreen")
adj_cols <- adjustcolor(site_cols,alpha.f = 0.65)

# 14-day attack rates
plot(log(c(EIR_k,EIR_t,EIR_j)),c(ar_k_14,ar_t_14,ar_j_14), type = "n", 
     xlab = "Log(EIR)", ylab = "14-day Attack Rates",main = "Exposure vs. Infection")
legend(x = "topleft",fill = site_cols,legend = c("Kunungu","Tororo","Jinja"))

points(log(EIR_k), ar_k_14, col = adj_cols[1], pch=kpc)
points(log(EIR_t), ar_t_14, col = adj_cols[2], pch=tpc)
points(log(EIR_j), ar_j_14, col = adj_cols[3], pch=jpc)

# inefficiency
year_rate_k <- sapply(X = unique(out_k$location),FUN = function(id){
  sum(out_k$state0=="S" & out_k$state1=="I" & out_k$location==id)
})
aeff_k <- (EIR_k*365)/year_rate_k

year_rate_t <- sapply(X = unique(out_t$location),FUN = function(id){
  sum(out_t$state0=="S" & out_t$state1=="I" & out_t$location==id)
})
aeff_t <- (EIR_t*365)/year_rate_t

year_rate_j <- sapply(X = unique(out_j$location),FUN = function(id){
  sum(out_j$state0=="S" & out_j$state1=="I" & out_j$location==id)
})
aeff_j <- (EIR_j*365)/year_rate_j

plot(c(EIR_k*365,(EIR_t*365)[-which.max(aeff_t)],EIR_j*365),c(aeff_k,aeff_t[-which.max(aeff_t)],aeff_j), type = "n", 
     xlab = "aEIR", ylab = "Inefficiency (aEIR : aFOI)",main = "Exposure vs. Infection", log = "xy")
legend(x = "topleft",fill = site_cols,legend = c("Kunungu","Tororo","Jinja"))

points(EIR_k*365, aeff_k, col = adj_cols[1], pch=kpc)
points((EIR_t*365)[-which.max(aeff_t)], aeff_t[-which.max(aeff_t)], col = adj_cols[2], pch=tpc)
points(EIR_j*365, aeff_j, col = adj_cols[3], pch=jpc)