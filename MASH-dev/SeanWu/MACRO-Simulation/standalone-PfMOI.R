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

rm(list=ls());gc()

library(here)
library(Rcpp)
library(reshape2)
library(ggplot2)
library(scales)

source(here::here("data/sampledata.R"))
sourceCpp(here::here("tiny-pfmoi.cpp"))

N <- 1e4
simdat <- make_tororo(N = N,which = 1)

pfmoi <- rep(0L,N)

simout <- tiny_pfmoi(tmax = 1260,nh = N,init = pfmoi,
                     EIR_size = simdat$size,EIR_prob = simdat$prob,pb = TRUE)

df_bite <- data.frame(time=1:nrow(simout$bites),
                      mean=rowMeans(simout$bites),
                      median=apply(simout$bites,1,function(x){quantile(x,probs=c(0.5))}),
                      low=apply(simout$bites,1,function(x){quantile(x,probs = c(0.025))}),
                      high=apply(simout$bites,1,function(x){quantile(x,probs = c(0.975))}))

ggplot(data = df_bite) +
  geom_line(aes(x=time,y=mean),color="firebrick3") +
  geom_ribbon(aes(x=time,ymin=low,ymax=high),fill="firebrick3",alpha=0.25) +
  scale_y_continuous(trans = scales::log1p_trans()) +
  ylab("EIR") +
  theme_bw()

df_foi <- data.frame(time=seq_along(simout$foi[-1]),
                     foi_scale=simout$foi[-1]/N
                     )

ggplot(data = df_foi) +
  geom_line(aes(x=time,y=foi_scale)) +
  theme_bw()

# run some ensemble sims
library(doSNOW)
library(foreach)
library(parallel)

nrep <- 5e2
rm(tiny_pfmoi);gc() # otherwise the cluster cores will pull null symbols

# set up cluster and source the file on each core
cl <- makeSOCKcluster(4)
registerDoSNOW(cl)
clusterEvalQ(cl,{
  Rcpp::sourceCpp(here::here("tiny-pfmoi.cpp"))
})

# progress bar
pb <- txtProgressBar(max=nrep, style=3)
progress <- function(n){setTxtProgressBar(pb, n)}
opts <- list(progress=progress)

# loop over repetitions
simout_rep <- foreach(i = 1:nrep, .combine="rbind",.options.snow=opts) %dopar% {
  
  N <- 1e3
  simdat <- make_tororo(N = N,which = 1)
  
  pfmoi <- rep(0L,N)
  
  simout <- tiny_pfmoi(tmax = 1260,nh = N,init = pfmoi,
                       EIR_size = simdat$size,EIR_prob = simdat$prob,pb = TRUE)
  
  data.frame(time=seq_along(simout$foi[-1]),
             iter=rep(i,length(simout$foi[-1])),
             foi_scale=simout$foi[-1]/N)
}

close(pb)
stopCluster(cl);rm(cl);gc()

ggplot(data = simout_rep) +
  geom_line(aes(x=time,y=foi_scale,group=iter),alpha=0.05) +
  theme_bw()
