################################################################################
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   Sean Wu & Daniel Citron
#   Replication of results from Uganda PRISM Study
#   PfSI with homogeneous biting
#   February 2019
#
################################################################################

rm(list=ls());gc()

library(here)
library(Rcpp)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(scales)

source(here::here("data/sampledata.R"))
b <- 0.55


################################################################################
#   PfSI model needs to burn-in the number of S/I individuals
################################################################################

N <- 1e4
nrep <- 1e3

library(doSNOW)
library(foreach)
library(parallel)

# # set up cluster and source the file on each core
# cl <- makeSOCKcluster(4)
# registerDoSNOW(cl)
# clusterEvalQ(cl,{
#   Rcpp::sourceCpp(here::here("tiny-pfsi.cpp"))
# })
#
# # progress bar
# pb <- txtProgressBar(max=nrep, style=3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress=progress)
#
# # loop over repetitions
# burnin <- foreach(i = 1:nrep, .combine="rbind",.options.snow=opts) %dopar% {
#
#   iter <- setNames(object = rep(0,3),nm = c("Tororo","Kanungu","Jinja"))
#
#   dat <- make_tororo(N = N,which = 1)
#   out <- tiny_pfsi(tmax = 365*3,nh = N,init = rep("S",N),
#                    EIR_size = dat$size,EIR_prob = dat$prob,pb = FALSE)
#
#   iter[["Tororo"]] <- out$states[nrow(out$states),2] / sum(out$states[nrow(out$states),])
#
#   dat <- make_kanungu(N = N,which = 1)
#   out <- tiny_pfsi(tmax = 365*3,nh = N,init = rep("S",N),
#                    EIR_size = dat$size,EIR_prob = dat$prob,pb = FALSE)
#
#   iter[["Kanungu"]] <- out$states[nrow(out$states),2] / sum(out$states[nrow(out$states),])
#
#   dat <- make_jinja(N = N,which = 1)
#   out <- tiny_pfsi(tmax = 365*3,nh = N,init = rep("S",N),
#                    EIR_size = dat$size,EIR_prob = dat$prob,pb = FALSE)
#
#   iter[["Jinja"]] <- out$states[nrow(out$states),2] / sum(out$states[nrow(out$states),])
#
#   iter
# }
#
# close(pb)
# stopCluster(cl);rm(cl);gc()
#
# pfpr_means <- colMeans(burnin)
pfpr_means <- c(Tororo=0.0001001316,Kanungu=0.0001014503,Jinja=0.0001090688)


################################################################################
# ensemble simulation run for Tororo
################################################################################

nrep <- 1e2
rm(tiny_pfmoi);gc() # otherwise the cluster cores will pull null symbols

# set up cluster and source the file on each core
cl <- makeSOCKcluster(4)
registerDoSNOW(cl)
clusterEvalQ(cl,{
  Rcpp::sourceCpp(here::here("tiny-pfsi.cpp"))
})

# progress bar
pb <- txtProgressBar(max=nrep, style=3)
progress <- function(n){setTxtProgressBar(pb, n)}
opts <- list(progress=progress)

# loop over repetitions
simout_t <- foreach(i = 1:nrep, .combine="rbind",.options.snow=opts) %dopar% {

  N <- 1e4
  # simdat <- make_tororo(N = N,which = 1)

  simout <- tiny_pfsi(tmax = 1260,nh = N,init = sample(x = c("S","I"),size = N,replace = TRUE,prob = c(1-pfpr_means[["Tororo"]],pfpr_means[["Tororo"]])),
                       EIR_size = replicate(N,TTSd*TTz,simplify=FALSE),EIR_prob = replicate(N,rep(0,1260),simplify=FALSE),pb = FALSE)

  # aggregate data into 2-week blocks
  blocks <- seq(from=0,to=1260,by=14)

  # FOI
  h_hat <- h_hatS <-rep(0,length(blocks)-1)

  # attack rate
  AR <-rep(0,length(blocks)-1)

  # estimated FOI
  h_tilde <-rep(0,length(blocks)-1)

  # loop over blocks
  for(t in 1:(length(blocks)-1)){

    # aggregate over this
    block_t <- (blocks[t]+1):blocks[t+1]

    # standardize by the number of susceptibles at the start of this 2-wk block
    block_sus <- simout$states[block_t[1],] == 0
    N_S <- sum(block_sus)
    S_ix <- which(block_sus)

    # FOI (using both denominators)
    h_hat[t] <- sum(simout$foi[block_t,])/14/N
    h_hatS[t] <- sum(simout$foi[block_t,])/14/N_S

    # attack rate (only count amongst proportion of people who started this 2-wk block susceptible)
    AR[t] <- 1 - (sum(apply(simout$ar[block_t,S_ix],2,function(x){all(x==0)})) / N_S)

    # estimated FOI
    h_tilde[t] <- -log(1 - AR[t])/14

  }

  # transmission efficiency
  aeff <- h_hat/h_tilde

  data.frame(iter=rep(i,90),time=1:90,h_hat=h_hat,h_hatS=h_hatS,AR=AR,h_tilde=h_tilde,aeff=aeff)

  # # annual metrics
  # aEIR <- sum(simout$bites)/(1260/365)/N
  #
  # # annual FOI
  # aFOI <- sum(simout$foi)/(1260/365)/N
  #
  # data.frame(iter=rep(i,90),time=1:90,h_hat=h_hat,AR=AR,h_tilde=h_tilde,
  #            aeff=aeff,aEIR=aEIR,aFOI=aFOI)
}

close(pb)
stopCluster(cl);rm(cl);gc()


################################################################################
# ensemble simulation run for Kanungu
################################################################################

rm(tiny_pfmoi);gc() # otherwise the cluster cores will pull null symbols

# set up cluster and source the file on each core
cl <- makeSOCKcluster(4)
registerDoSNOW(cl)
clusterEvalQ(cl,{
  Rcpp::sourceCpp(here::here("tiny-pfsi.cpp"))
})

# progress bar
pb <- txtProgressBar(max=nrep, style=3)
progress <- function(n){setTxtProgressBar(pb, n)}
opts <- list(progress=progress)

# loop over repetitions
simout_k <- foreach(i = 1:nrep, .combine="rbind",.options.snow=opts) %dopar% {

  N <- 1e4
  # simdat <- make_kanungu(N = N,which = 1)

  simout <- tiny_pfsi(tmax = 1260,nh = N,init = sample(x = c("S","I"),size = N,replace = TRUE,prob = c(1-pfpr_means[["Kanungu"]],pfpr_means[["Kanungu"]])),
                       EIR_size = replicate(N,KKSd*KKz,simplify=FALSE),EIR_prob = replicate(N,rep(0,1260),simplify=FALSE),pb = FALSE)

  # aggregate data into 2-week blocks
  blocks <- seq(from=0,to=1260,by=14)

  # FOI
  h_hat <- h_hatS <-rep(0,length(blocks)-1)

  # attack rate
  AR <-rep(0,length(blocks)-1)

  # estimated FOI
  h_tilde <-rep(0,length(blocks)-1)

  # loop over blocks
  for(t in 1:(length(blocks)-1)){

    # aggregate over this
    block_t <- (blocks[t]+1):blocks[t+1]

    # standardize by the number of susceptibles at the start of this 2-wk block
    block_sus <- simout$states[block_t[1],] == 0
    N_S <- sum(block_sus)
    S_ix <- which(block_sus)

    # FOI (using both denominators)
    h_hat[t] <- sum(simout$foi[block_t,])/14/N
    h_hatS[t] <- sum(simout$foi[block_t,])/14/N_S

    # attack rate (only count amongst proportion of people who started this 2-wk block susceptible)
    AR[t] <- 1 - (sum(apply(simout$ar[block_t,S_ix],2,function(x){all(x==0)})) / N_S)

    # estimated FOI
    h_tilde[t] <- -log(1 - AR[t])/14

  }

  # transmission efficiency
  aeff <- h_hat/h_tilde

  data.frame(iter=rep(i,90),time=1:90,h_hat=h_hat,h_hatS=h_hatS,AR=AR,h_tilde=h_tilde,aeff=aeff)
}

close(pb)
stopCluster(cl);rm(cl);gc()


################################################################################
# ensemble simulation run for Jinja
################################################################################

rm(tiny_pfmoi);gc() # otherwise the cluster cores will pull null symbols

# set up cluster and source the file on each core
cl <- makeSOCKcluster(4)
registerDoSNOW(cl)
clusterEvalQ(cl,{
  Rcpp::sourceCpp(here::here("tiny-pfsi.cpp"))
})

# progress bar
pb <- txtProgressBar(max=nrep, style=3)
progress <- function(n){setTxtProgressBar(pb, n)}
opts <- list(progress=progress)

# loop over repetitions
simout_j <- foreach(i = 1:nrep, .combine="rbind",.options.snow=opts) %dopar% {

  N <- 1e4
  # simdat <- make_jinja(N = N,which = 1)

  simout <- tiny_pfsi(tmax = 1260,nh = N,init = sample(x = c("S","I"),size = N,replace = TRUE,prob = c(1-pfpr_means[["Jinja"]],pfpr_means[["Jinja"]])),
                       EIR_size = replicate(N,JJSd*JJz,simplify=FALSE),EIR_prob = replicate(N,rep(0,1260),simplify=FALSE),pb = FALSE)

  # aggregate data into 2-week blocks
  blocks <- seq(from=0,to=1260,by=14)

  # FOI
  h_hat <- h_hatS <-rep(0,length(blocks)-1)

  # attack rate
  AR <-rep(0,length(blocks)-1)

  # estimated FOI
  h_tilde <-rep(0,length(blocks)-1)

  # loop over blocks
  for(t in 1:(length(blocks)-1)){

    # aggregate over this
    block_t <- (blocks[t]+1):blocks[t+1]

    # standardize by the number of susceptibles at the start of this 2-wk block
    block_sus <- simout$states[block_t[1],] == 0
    N_S <- sum(block_sus)
    S_ix <- which(block_sus)

    # FOI (using both denominators)
    h_hat[t] <- sum(simout$foi[block_t,])/14/N
    h_hatS[t] <- sum(simout$foi[block_t,])/14/N_S

    # attack rate (only count amongst proportion of people who started this 2-wk block susceptible)
    AR[t] <- 1 - (sum(apply(simout$ar[block_t,S_ix],2,function(x){all(x==0)})) / N_S)

    # estimated FOI
    h_tilde[t] <- -log(1 - AR[t])/14

  }

  # transmission efficiency
  aeff <- h_hat/h_tilde

  data.frame(iter=rep(i,90),time=1:90,h_hat=h_hat,h_hatS=h_hatS,AR=AR,h_tilde=h_tilde,aeff=aeff)
}

close(pb)
stopCluster(cl);rm(cl);gc()


################################################################################
# transmission efficiency vs. time (Fig 4)
################################################################################

# transmission inefficiency over time
simout_t$site <- rep("Tororo",nrow(simout_t))
simout_k$site <- rep("Kanungu",nrow(simout_k))
simout_j$site <- rep("Jinja",nrow(simout_j))

simout <- rbind(simout_t,simout_k,simout_j)

fig4a <- ggplot(data = simout_t) +
  geom_line(aes(x=time,y=h_hat,group=iter),color="darkred",alpha=0.15) +
  geom_line(aes(x=time,y=h_tilde,group=iter),color="firebrick3",alpha=0.15) +
  scale_x_continuous(breaks = (0:3)*26,labels = 0:3,name = "Time (Years)") +
  ylab("daily FOI (simulated)") +
  ggtitle("b) PfSI: Tororo") +
  theme_bw()

fig4b <- ggplot(data = simout_k) +
  geom_line(aes(x=time,y=h_hat,group=iter),color="darkblue",alpha=0.15) +
  geom_line(aes(x=time,y=h_tilde,group=iter),color="steelblue",alpha=0.15) +
  scale_x_continuous(breaks = (0:3)*26,labels = 0:3,name = "Time (Years)") +
  ylab("daily FOI (simulated)") +
  ggtitle("b) PfSI: Kanungu") +
  theme_bw()

fig4c <- ggplot(data = simout_j) +
  geom_line(aes(x=time,y=h_hat,group=iter),color="darkgreen",alpha=0.15) +
  geom_line(aes(x=time,y=h_tilde,group=iter),color="forestgreen",alpha=0.15) +
  scale_x_continuous(breaks = (0:3)*26,labels = 0:3,name = "Time (Years)") +
  ylab("daily FOI (simulated)") +
  ggtitle("c) PfSI: Jinja") +
  theme_bw()

# transmission efficiency (4D: FOI normalized by N
fig4d <- ggplot(data = simout) +
  geom_line(aes(x=time,y=aeff,color=site,group=interaction(iter,site)),alpha=0.15) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  # scale_color_manual(values = c(Tororo="firebrick3",Kanungu="steelblue",Jinja="darkorchid3")) +
  # guides(colour = guide_legend(override.aes = list(alpha = 1,size = 2))) +
  guides(colour = FALSE) +
  scale_y_log10(breaks = c(2.2, 4.4, 9.5),labels = c("1.7:1","2.7:1","7.4:1")) +
  scale_x_continuous(breaks = (0:3)*26,labels = as.character(0:3)) +
  ylab("Transmission Efficiency") +
  xlab("Time (Years)") +
  ggtitle("d)") +
  theme_bw()

fig4 <- grid.arrange(fig4a,fig4b,fig4c,fig4d,nrow=2,ncol=2)

ggsave(filename = here::here("figures/pfsi_hom_4.pdf"),plot = fig4,device = "pdf",width = 12,height = 10)

# # FOI normalized by S
# simout$aeffS <- simout$h_hatS/simout$h_tilde
#
# ggplot(data = simout) +
#   geom_line(aes(x=time,y=aeffS,color=site,group=interaction(iter,site)),alpha=0.15) +
#   scale_color_manual(values = c(Tororo="darkred",Kanungu="darkgreen",Jinja="darkblue")) +
#   # scale_color_manual(values = c(Tororo="firebrick3",Kanungu="steelblue",Jinja="darkorchid3")) +
#   guides(colour = guide_legend(override.aes = list(alpha = 1,size = 2))) +
#   scale_y_log10(breaks = c(2.2, 4.4, 9.5),labels = c("1.7:1","2.7:1","7.4:1")) +
#   scale_x_continuous(breaks = (0:3)*26,labels = as.character(0:3)) +
#   ylab("Transmission Efficiency") +
#   xlab("Time (Years)") +
#   ggtitle("PfSI") +
#   theme_bw()


################################################################################
#   exposure vs. infection (Fig 2D)
################################################################################

# run sims
N <- 1e4
Rcpp::sourceCpp(here::here("tiny-pfsi.cpp"))

simdat_t <- make_tororo(N = N,which = 1)
simout_t <- tiny_pfsi(tmax = 1260,nh = N,init = sample(x = c("S","I"),size = N,replace = TRUE,prob = c(1-pfpr_means[["Tororo"]],pfpr_means[["Tororo"]])),
                    EIR_size = simdat_t$size,EIR_prob = simdat_t$prob,pb = TRUE)

simdat_k <- make_kanungu(N = N,which = 1)
simout_k <- tiny_pfsi(tmax = 1260,nh = N,init = sample(x = c("S","I"),size = N,replace = TRUE,prob = c(1-pfpr_means[["Kanungu"]],pfpr_means[["Kanungu"]])),
                      EIR_size = simdat_k$size,EIR_prob = simdat_k$prob,pb = TRUE)

simdat_j <- make_jinja(N = N,which = 1)
simout_j <- tiny_pfsi(tmax = 1260,nh = N,init = sample(x = c("S","I"),size = N,replace = TRUE,prob = c(1-pfpr_means[["Jinja"]],pfpr_means[["Jinja"]])),
                    EIR_size = simdat_j$size,EIR_prob = simdat_j$prob,pb = TRUE)

# calc data
aEIR_t <- colSums(simout_t$bites)/(1260/365)
aFOI_t <- colSums(simout_t$ar)/(1260/365)

aEIR_k <- colSums(simout_k$bites)/(1260/365)
aFOI_k <- colSums(simout_k$ar)/(1260/365)

aEIR_j <- colSums(simout_j$bites)/(1260/365)
aFOI_j <- colSums(simout_j$ar)/(1260/365)

ineff_df <- data.frame(
  aEIR = c(aEIR_t,aEIR_k,aEIR_j),
  aFOI = c(aFOI_t,aFOI_k,aFOI_j),
  site = rep(c("Tororo","Kanungu","Jinja"),each=length(aEIR_t))
)
ineff_df$eff <- ineff_df$aEIR / ineff_df$aFOI

fig2 <- ggplot(data = ineff_df[is.finite(ineff_df$eff) & is.finite(ineff_df$aEIR),]) +
  geom_jitter(aes(x=aEIR,y=eff,color=site),alpha=0.075,width = 0.15, height = 0.15) +
  scale_y_continuous(trans = scales::log1p_trans(),breaks = c(1/2, 2, 10, 50, 1e2, 1e3),labels = c("1:2","2:1","10:1","50:1","100:1","1000:1")) +
  scale_x_continuous(trans = scales::log1p_trans(),breaks = 10^(0:3)) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
  # scale_color_manual(values = c(Tororo="firebrick3",Kanungu="steelblue",Jinja="darkorchid3")) +
  # guides(colour = guide_legend(override.aes = list(alpha = 1,size = 2))) +
  guides(colour = FALSE) +
  ylab("Inefficiency (aEIR : aFOI)") +
  xlab("Annual EIR") +
  theme_bw()

ggsave(filename = here::here("figures/pfsi_hom_2.pdf"),plot = fig2,device = "pdf",width = 10,height = 8)

# llm=lm(log(ineff_df$eff[is.finite(ineff_df$eff)])~log(ineff_df$aEIR[is.finite(ineff_df$eff)]))
# a = exp(coef(llm)[1])
# b = coef(llm)[2]
# plot(x=ineff_df$aEIR,y=ineff_df$eff,col=as.factor(ineff_df$site),log="xy",pch=16)
