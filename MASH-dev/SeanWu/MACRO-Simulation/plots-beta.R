rm(list=ls());gc()

library(here)
library(Rcpp)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(scales)
library(matrixStats)

source(here::here("data/sampledata.R"))
b <- 0.55
pfpr_means <- c(Tororo=0.0001001316,Kanungu=0.0001014503,Jinja=0.0001090688)


# run PfSI
N <- 1e4
Rcpp::sourceCpp(here::here("tiny-pfsi.cpp"))

simdat_t <- make_tororo(N = N,which = 1)
simout_t_pfsi <- tiny_pfsi(tmax = 1260,nh = N,init = sample(x = c("S","I"),size = N,replace = TRUE,prob = c(1-pfpr_means[["Tororo"]],pfpr_means[["Tororo"]])),
                      EIR_size = simdat_t$size,EIR_prob = simdat_t$prob,pb = TRUE)

simdat_k <- make_kanungu(N = N,which = 1)
simout_k_pfsi <- tiny_pfsi(tmax = 1260,nh = N,init = sample(x = c("S","I"),size = N,replace = TRUE,prob = c(1-pfpr_means[["Kanungu"]],pfpr_means[["Kanungu"]])),
                      EIR_size = simdat_k$size,EIR_prob = simdat_k$prob,pb = TRUE)

simdat_j <- make_jinja(N = N,which = 1)
simout_j_pfsi <- tiny_pfsi(tmax = 1260,nh = N,init = sample(x = c("S","I"),size = N,replace = TRUE,prob = c(1-pfpr_means[["Jinja"]],pfpr_means[["Jinja"]])),
                      EIR_size = simdat_j$size,EIR_prob = simdat_j$prob,pb = TRUE)

# # # run PfMOI
# # pfmoi <- rep(0L,N)
# # Rcpp::sourceCpp(here::here("tiny-pfmoi.cpp"))
# # 
# # simout_t_pfmoi <- tiny_pfmoi(tmax = 1260,nh = N,init = pfmoi,
# #                        EIR_size = simdat_t$size,EIR_prob = simdat_t$prob,pb = TRUE)
# # 
# # simout_k_pfmoi <- tiny_pfmoi(tmax = 1260,nh = N,init = pfmoi,
# #                        EIR_size = simdat_k$size,EIR_prob = simdat_k$prob,pb = TRUE)
# # 
# # simout_j_pfmoi <- tiny_pfmoi(tmax = 1260,nh = N,init = pfmoi,
# #                        EIR_size = simdat_j$size,EIR_prob = simdat_j$prob,pb = TRUE)
# 
# # aggregate output
# block_dt <- 28
# blocks <- seq(from=0,to=1260,by=block_dt)
# nblock <- length(blocks)-1
# 
# # calculate means-variance by houses
# EIR_mean_pfsi_t <- EIR_mean_pfsi_k <- EIR_mean_pfsi_j <- matrix(data = NA,nrow = nblock,ncol = N)
# EIR_var_pfsi_t <- EIR_var_pfsi_k <- EIR_var_pfsi_j <- matrix(data = NA,nrow = nblock,ncol = N)
# 
# # loop over blocks
# pb <- txtProgressBar(min = 1,max = nblock)
# for(t in 1:nblock){
#   
#   # aggregate over this
#   block_t <- (blocks[t]+1):blocks[t+1]
#   
#   # EIR
#   EIR_t_pfsi[t] <- mean(simout_t_pfsi$bites[block_t,])
#   EIR_k_pfsi[t] <- mean(simout_k_pfsi$bites[block_t,])
#   EIR_j_pfsi[t] <- mean(simout_j_pfsi$bites[block_t,])
#   # EIR_t_pfmoi[t] <- mean(simout_t_pfmoi$bites[block_t,])
#   # EIR_k_pfmoi[t] <- mean(simout_k_pfmoi$bites[block_t,])
#   # EIR_j_pfmoi[t] <- mean(simout_j_pfmoi$bites[block_t,])
#   
#   
#   setTxtProgressBar(pb,t)
# }
# close(pb)


eir_means_t <- colMeans(simout_t_pfsi$bites)
eir_vars_t <- colVars(simout_t_pfsi$bites)
eir_means_k <- colMeans(simout_k_pfsi$bites)
eir_vars_k <- colVars(simout_k_pfsi$bites)
eir_means_j <- colMeans(simout_j_pfsi$bites)
eir_vars_j <- colVars(simout_j_pfsi$bites)

eir_m_v <- data.frame(
  mean = c(eir_means_t,eir_means_k,eir_means_j),
  var = c(eir_vars_t,eir_vars_k,eir_vars_j),
  site = rep(c("Tororo","Kanungu","Jinja"),each=N)
)

eir_m_v <- eir_m_v[eir_m_v$mean > 0 & eir_m_v$var > 0,]

eir_model <- ggplot(data=eir_m_v) +
  geom_point(aes(x=mean,y=var,color=site),alpha=0.025) +
  geom_smooth(aes(x=mean,y=var,color=site),method = "lm",alpha=0.9,se = F) +
  geom_abline(intercept = 0,slope = 1,color = "black",alpha=0.5,linetype=2) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  scale_x_continuous(trans = "log10",name = "Means",breaks = 10^c(-3:0),labels = c(0.001,0.01,0.1,1)) +
  scale_y_continuous(trans = "log10",name = "Variances",breaks = c(1e-2,1,1e2),labels = c(0.01,1,100)) +
  guides(color = FALSE) +
  ggtitle("Simulated EIR") +
  theme_bw() +
  theme(plot.title = element_text(size = 16),axis.title = element_text(size = 12))

ggsave(filename = here::here("figures/eir_meanvar.pdf"),plot = eir_model,device = "pdf",width = 10,height = 8)

# want the data there too

# read in data
tororo <- read.table(here("data/TOROROnew.txt"), header=T)
jinja <- read.table (here("data/JINJAnew.txt"), header=T)
kanungu <- read.table(here("data/KANUNGUnew.txt"), header=T)

# mean/var for household
getMVid = function(id, hhid, obs){
  ix = which(hhid == id)
  c(mean(obs[ix]), var(obs[ix]))
}

powerFit = function(mn,vr){
  ix = which(mn>0)
  fitlineT = lm(log(vr[ix])~log(mn[ix]))
  a = coef(fitlineT)[1]
  b = coef(fitlineT)[2]
  c(exp(a), b)
}

powerHouse = function(DT, counts, clr, mtl, ppch=19, ccx=.65, add=FALSE){
  id = unique(DT$hhid)
  
  out = sapply(id, getMVid, hhid=DT$hhid, obs=counts)
  zeros = which(out[1,] == 0)
  if(length(zeros)>0) out = out[,-zeros]
  nas = which(is.na(out[2,]))
  if(length(nas)>0) out = out[,-nas]
  
  # plotPower(out[1,], out[2,], clr, mtl, ppch, ccx, add)
  # plotPowerFit(out[1,], out[2,], clr, mtl, TRUE)
  par = powerFit(out[1,], out[2,])
  list(mn = out[1,], vr=out[2,], par=par)
}

# plotting parameters
ccx =0.9
tpc = 16
kpc = 4
jpc = 18

# TabE = powerMonth(tororo, tororo$EIR, "darkred", "Infectious Mosquito Counts by Month", tpc, ccx)
# KabE = powerMonth(kanungu, kanungu$EIR, "darkblue", NULL, kpc, ccx, 1)
# JabE = powerMonth(jinja, jinja$EIR, "darkgreen", NULL, jpc, ccx, 1)

Tab = powerHouse(tororo, tororo$EIR, "darkred", "Infectious Counts by Household", tpc, ccx)
Kab = powerHouse(kanungu, kanungu$EIR, "darkblue", NULL, kpc, ccx, 1)
Jab = powerHouse(jinja, jinja$EIR, "darkgreen", NULL, jpc, ccx, 1)
ALLmn = c(Tab$mn, Kab$mn, Jab$mn)
ALLvr = c(Tab$vr, Kab$vr, Jab$vr)

eir_m_v_both <- NULL
eir_m_v_both <- rbind(eir_m_v_both,data.frame(mean=Tab$mn,var=Tab$vr,site="Tororo",type="data"))
eir_m_v_both <- rbind(eir_m_v_both,data.frame(mean=Kab$mn,var=Kab$vr,site="Kanungu",type="data"))
eir_m_v_both <- rbind(eir_m_v_both,data.frame(mean=Jab$mn,var=Jab$vr,site="Jinja",type="data"))

# point <- scales::format_format(big.mark = " ", decimal.mark = ".", scientific = FALSE,nsmall = 0)

ranges <- ggplot_build(eir_model)$layout$panel_params[[1]][c("x.range","y.range")]

ic_data <- ggplot(data=eir_m_v_both) +
  geom_point(aes(x=mean,y=var,color=site),alpha=0.75) +
  geom_smooth(aes(x=mean,y=var,color=site),method = "lm",se = F) +
  geom_abline(intercept = 0,slope = 1,color = "black",alpha=0.5,linetype=2) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  scale_x_continuous(trans = "log10",name = "Means",breaks = 10^c(-3:0),labels = c(0.001,0.01,0.1,1)) +
  scale_y_continuous(trans = "log10",name = "Variances",breaks=c(0.01,1,100),labels = c(0.01,1,100)) +
  # scale_x_continuous(trans = "log10",name = "Means",breaks = 10^c(-3:0),labels = c(0.001,0.01,0.1,1),limits = 10^ranges$x.range) +
  # scale_y_continuous(trans = "log10",name = "Variances",breaks=c(0.01,1,100),labels = c(0.01,1,100),limits = 10^ranges$y.range) +
  guides(color = FALSE) +
  ggtitle("Infectious Mosqutio Counts") +
  theme_bw() +
  theme(plot.title = element_text(size = 16),axis.title = element_text(size = 12))


joint_plot <- grid.arrange(ic_data,eir_model,nrow=1)
ggsave(filename = here::here("figures/eir_meanvar.pdf"),plot = joint_plot,device = "pdf",width = 12,height = 6)






s# # discretise
# 
# 
# # EIR
# EIR_t_pfsi <- EIR_k_pfsi <- EIR_j_pfsi <- EIR_t_pfmoi <- EIR_k_pfmoi <- EIR_j_pfmoi <- rep(0,length(blocks)-1)
# # EIR_t_pfsi <- EIR_k_pfsi <- EIR_j_pfsi <- EIR_t_pfmoi <- EIR_k_pfmoi <- EIR_j_pfmoi <- matrix(0,nrow = length(blocks)-1,ncol = N)
# 
# # FOI exact
# FOI_exact_t_pfsi <- FOI_exact_k_pfsi <- FOI_exact_j_pfsi <- FOI_exact_t_pfmoi <- FOI_exact_k_pfmoi <- FOI_exact_j_pfmoi <- rep(0,length(blocks)-1)
# 
# # attack rate
# AR_t_pfsi <- AR_k_pfsi <- AR_j_pfsi <- AR_t_pfmoi <- AR_k_pfmoi <- AR_j_pfmoi <- rep(0,length(blocks)-1)
# 
# # FOI approximate
# FOI_approx_t_pfsi <- FOI_approx_k_pfsi <- FOI_approx_j_pfsi <- FOI_approx_t_pfmoi <- FOI_approx_k_pfmoi <- FOI_approx_j_pfmoi <- rep(0,length(blocks)-1)
# 
# # loop over blocks
# pb <- txtProgressBar(min = 1,max = length(blocks)-1)
# for(t in 1:(length(blocks)-1)){
#   
#   # aggregate over this
#   block_t <- (blocks[t]+1):blocks[t+1]
#   
#   # EIR
#   # EIR_t_pfsi <- sum(simout_t_pfsi$bites[block_t,])/block_dt/N
#   EIR_t_pfsi[t] <- mean(simout_t_pfsi$bites[block_t,])
#   EIR_k_pfsi[t] <- mean(simout_k_pfsi$bites[block_t,])
#   EIR_j_pfsi[t] <- mean(simout_j_pfsi$bites[block_t,])
#   EIR_t_pfmoi[t] <- mean(simout_t_pfmoi$bites[block_t,])
#   EIR_k_pfmoi[t] <- mean(simout_k_pfmoi$bites[block_t,])
#   EIR_j_pfmoi[t] <- mean(simout_j_pfmoi$bites[block_t,])
#   
#   # FOI exact
#   FOI_exact_t_pfsi[t] <- mean(simout_t_pfsi$foi[block_t,])
#   FOI_exact_k_pfsi[t] <- mean(simout_k_pfsi$foi[block_t,])
#   FOI_exact_j_pfsi[t] <- mean(simout_j_pfsi$foi[block_t,])
#   FOI_exact_t_pfmoi[t] <- mean(simout_t_pfmoi$foi[block_t,])
#   FOI_exact_k_pfmoi[t] <- mean(simout_k_pfmoi$foi[block_t,])
#   FOI_exact_j_pfmoi[t] <- mean(simout_j_pfmoi$foi[block_t,])
#   
#   # attack rate
#   # pfsi needs to only calc AR among susceptibles
#   S_ix_t <- which(simout_t_pfsi$states[block_t[1],] == 0)
#   S_ix_k <- which(simout_k_pfsi$states[block_t[1],] == 0)
#   S_ix_j <- which(simout_j_pfsi$states[block_t[1],] == 0)
#   AR_t_pfsi[t] <- sum(colSums(simout_t_pfsi$ar[block_t,S_ix_t]) > 0) / length(S_ix_t)
#   AR_k_pfsi[t] <- sum(colSums(simout_k_pfsi$ar[block_t,S_ix_k]) > 0) / length(S_ix_k)
#   AR_j_pfsi[t] <- sum(colSums(simout_j_pfsi$ar[block_t,S_ix_j]) > 0) / length(S_ix_j)
#   AR_t_pfmoi[t] <- sum(colSums(simout_t_pfmoi$ar[block_t,])  > 0) / N
#   AR_k_pfmoi[t] <- sum(colSums(simout_k_pfmoi$ar[block_t,])  > 0) / N
#   AR_j_pfmoi[t] <- sum(colSums(simout_j_pfmoi$ar[block_t,])  > 0) / N
#   
#   # FOI approx
#   FOI_approx_t_pfsi[t] <- -log(1 - AR_t_pfsi[t]) / block_dt
#   FOI_approx_k_pfsi[t] <- -log(1 - AR_k_pfsi[t]) / block_dt
#   FOI_approx_j_pfsi[t] <- -log(1 - AR_j_pfsi[t]) / block_dt
#   FOI_approx_t_pfmoi[t] <- -log(1 - AR_t_pfmoi[t]) / block_dt
#   FOI_approx_k_pfmoi[t] <- -log(1 - AR_k_pfmoi[t]) / block_dt
#   FOI_approx_j_pfmoi[t] <- -log(1 - AR_j_pfmoi[t]) / block_dt
#   
#   setTxtProgressBar(pb,t)
# }
# close(pb)
# 
# 
# par(mfrow=c(2,2));
# 
# plot(EIR_t_pfsi,FOI_exact_t_pfsi,pch=16,col=grey(0.5,0.15),main="exact FOI vs. EIR (PfSI)");
# 
# plot(EIR_t_pfsi,FOI_approx_t_pfsi,pch=16,col=grey(0.5,0.15),main="approximate FOI vs. EIR (PfSI)",ylim = c(0,10));
# 
# plot(EIR_t_pfsi,FOI_approx_t_pfsi/EIR_t_pfsi,pch=16,col=grey(0.5,0.15),main="exact inefficiency vs. EIR");
# 
# plot(EIR_t_pfsi,FOI_approx_t_pfsi/EIR_t_pfsi,pch=16,col=grey(0.5,0.15),main="approximate inefficiency vs. EIR",ylim = c(0,10));
# 
# par(mfrow=c(1,1))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # calc data
# aEIR_t <- colSums(simout_t$bites)/(1260/365)
# aFOI_t <- colSums(simout_t$ar)/(1260/365)
# # aFOI_exact_t <- colSums(simout_t$foi)/(1260/365)
# 
# aEIR_k <- colSums(simout_k$bites)/(1260/365)
# aFOI_k <- colSums(simout_k$ar)/(1260/365)
# # aFOI_k <- colSums(simout_k$foi)/(1260/365)
# 
# aEIR_j <- colSums(simout_j$bites)/(1260/365)
# aFOI_j <- colSums(simout_j$ar)/(1260/365)
# # aFOI_j <- colSums(simout_j$foi)/(1260/365)
# 
# ineff_df <- data.frame(
#   aEIR = c(aEIR_t,aEIR_k,aEIR_j),
#   aFOI = c(aFOI_t,aFOI_k,aFOI_j),
#   site = rep(c("Tororo","Kanungu","Jinja"),each=length(aEIR_t))
# )
# ineff_df$eff <- ineff_df$aEIR / ineff_df$aFOI
# 
# 
# 
# 
# 
# 
# 
# par(mfrow=c(2,2));
# 
# plot(aEIR_t,aFOI_exact_t,pch=16,col=grey(0.5,0.15),main="exact FOI vs. EIR (annual)");
# 
# plot(aEIR_t,aFOI_t,pch=16,col=grey(0.5,0.15),main="approximate FOI vs. EIR (annual)",ylim = c(0,10));
# 
# plot(aEIR_t,aEIR_t/aFOI_exact_t,pch=16,col=grey(0.5,0.15),main="exact inefficiency vs. EIR");
# 
# plot(aEIR_t,aEIR_t/aFOI_t,pch=16,col=grey(0.5,0.15),main="approximate inefficiency vs. EIR",ylim = c(0,10));
# 
# par(mfrow=c(1,1))