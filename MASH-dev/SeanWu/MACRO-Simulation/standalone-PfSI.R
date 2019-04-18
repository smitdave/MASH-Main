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
b <- 0.55


################################################################################
#   PfSI model needs to burn-in the number of S/I individuals
################################################################################

N <- 1e4
nrep <- 1e3

library(doSNOW)
library(foreach)
library(parallel)

# set up cluster and source the file on each core
cl <- makeSOCKcluster(4)
registerDoSNOW(cl)
clusterEvalQ(cl,{
  Rcpp::sourceCpp(here::here("tiny-pfsi.cpp"))
})

# progress bar
pb <- txtProgressBar(max=nrep, style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

# loop over repetitions
burnin <- foreach(i = 1:nrep, .combine="rbind",.options.snow=opts) %dopar% {

  iter <- setNames(object = rep(0,3),nm = c("Tororo","Kanungu","Jinja"))

  dat <- make_tororo(N = N,which = 1)
  out <- tiny_pfsi(tmax = 365*3,nh = N,init = rep("S",N),
                   EIR_size = dat$size,EIR_prob = dat$prob,pb = FALSE)

  iter[["Tororo"]] <- out$states[nrow(out$states),2] / sum(out$states[nrow(out$states),])

  dat <- make_kanungu(N = N,which = 1)
  out <- tiny_pfsi(tmax = 365*3,nh = N,init = rep("S",N),
                   EIR_size = dat$size,EIR_prob = dat$prob,pb = FALSE)

  iter[["Kanungu"]] <- out$states[nrow(out$states),2] / sum(out$states[nrow(out$states),])

  dat <- make_jinja(N = N,which = 1)
  out <- tiny_pfsi(tmax = 365*3,nh = N,init = rep("S",N),
                   EIR_size = dat$size,EIR_prob = dat$prob,pb = FALSE)

  iter[["Jinja"]] <- out$states[nrow(out$states),2] / sum(out$states[nrow(out$states),])

  iter
}

close(pb)
stopCluster(cl);rm(cl);gc()

pfpr_means <- colMeans(burnin)


################################################################################
# ensemble simulation run for Tororo
################################################################################

nrep <- 1e3
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
  simdat <- make_tororo(N = N,which = 1)

  pfmoi <- rep(0L,N)

  simout <- tiny_pfsi(tmax = 1260,nh = N,init = sample(x = c("S","I"),size = N,replace = TRUE,prob = c(1-pfpr_means[["Tororo"]],pfpr_means[["Tororo"]])),
                       EIR_size = simdat$size,EIR_prob = simdat$prob,pb = FALSE)

  # aggregate data into 2-week blocks
  blocks <- seq(from=0,to=1260,by=14)

  # FOI
  h_hat <-rep(0,length(blocks)-1)

  # attack rate
  AR <-rep(0,length(blocks)-1)

  # estimated FOI
  h_tilde <- h_theory <-rep(0,length(blocks)-1)

  # loop over blocks
  for(t in 1:(length(blocks)-1)){

    # aggregate over this
    block_t <- (blocks[t]+1):blocks[t+1]

    # standardize by the number of susceptibles at the start of this 2-wk block
    N_S <- sum(simout$states[block_t[1],] == 0)

    # FOI (standardize by the number of S individuals at the start of this 2-wk block)
    h_hat[t] <- sum(simout$foi[block_t])/14/N_S

    # FOI-theoretical
    h_theory[t] <- (sum(simout$bites[block_t,])*b)/14/N

    # attack rate (only count amongst proportion of people who started this 2-wk block susceptible)
    ar <- (table(apply(simout$ar[block_t,simout$states[block_t[1],]==0],2,max)) / N_S)
    if(length(ar)>1){
      AR[t] <- ar[["1"]]
    } else {
      AR[t] <- 0
    }

    # estimated FOI
    h_tilde[t] <- -log(1 - AR[t])/14

  }

  # transmission efficiency
  aeff <- h_hat/h_tilde

  # annual metrics
  aEIR <- sum(simout$bites)/(1260/365)/N

  # annual FOI
  aFOI <- sum(simout$foi)/(1260/365)/N

  data.frame(iter=rep(i,90),time=1:90,h_hat=h_hat,AR=AR,h_tilde=h_tilde,
             aeff=aeff,aEIR=aEIR,aFOI=aFOI)
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
  simdat <- make_kanungu(N = N,which = 1)

  pfmoi <- rep(0L,N)

  simout <- tiny_pfsi(tmax = 1260,nh = N,init = sample(x = c("S","I"),size = N,replace = TRUE,prob = c(1-pfpr_means[["Kanungu"]],pfpr_means[["Kanungu"]])),
                       EIR_size = simdat$size,EIR_prob = simdat$prob,pb = FALSE)

  # aggregate data into 2-week blocks
  blocks <- seq(from=0,to=1260,by=14)

  # FOI
  h_hat <-rep(0,length(blocks)-1)

  # attack rate
  AR <-rep(0,length(blocks)-1)

  # estimated FOI
  h_tilde <- h_theory <-rep(0,length(blocks)-1)

  # loop over blocks
  for(t in 1:(length(blocks)-1)){

    # aggregate over this
    block_t <- (blocks[t]+1):blocks[t+1]

    # standardize by the number of susceptibles at the start of this 2-wk block
    N_S <- sum(simout$states[block_t[1],] == 0)

    # FOI (standardize by the number of S individuals at the start of this 2-wk block)
    h_hat[t] <- sum(simout$foi[block_t])/14/N_S

    # FOI-theoretical
    h_theory[t] <- (sum(simout$bites[block_t,])*b)/14/N

    # attack rate (only count amongst proportion of people who started this 2-wk block susceptible)
    ar <- (table(apply(simout$ar[block_t,simout$states[block_t[1],]==0],2,max)) / N_S)
    if(length(ar)>1){
      AR[t] <- ar[["1"]]
    } else {
      AR[t] <- 0
    }

    # estimated FOI
    h_tilde[t] <- -log(1 - AR[t])/14

  }

  # transmission efficiency
  aeff <- h_hat/h_tilde

  # annual metrics
  aEIR <- sum(simout$bites)/(1260/365)/N

  # annual FOI
  aFOI <- sum(simout$foi)/(1260/365)/N

  data.frame(iter=rep(i,90),time=1:90,h_hat=h_hat,AR=AR,h_tilde=h_tilde,
             aeff=aeff,aEIR=aEIR,aFOI=aFOI)
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
  simdat <- make_jinja(N = N,which = 1)

  pfmoi <- rep(0L,N)

  simout <- tiny_pfsi(tmax = 1260,nh = N,init = sample(x = c("S","I"),size = N,replace = TRUE,prob = c(1-pfpr_means[["Jinja"]],pfpr_means[["Jinja"]])),
                       EIR_size = simdat$size,EIR_prob = simdat$prob,pb = FALSE)

  # aggregate data into 2-week blocks
  blocks <- seq(from=0,to=1260,by=14)

  # FOI
  h_hat <-rep(0,length(blocks)-1)

  # attack rate
  AR <-rep(0,length(blocks)-1)

  # estimated FOI
  h_tilde <- h_theory <-rep(0,length(blocks)-1)

  # loop over blocks
  for(t in 1:(length(blocks)-1)){

    # aggregate over this
    block_t <- (blocks[t]+1):blocks[t+1]

    # standardize by the number of susceptibles at the start of this 2-wk block
    N_S <- sum(simout$states[block_t[1],] == 0)

    # FOI (standardize by the number of S individuals at the start of this 2-wk block)
    h_hat[t] <- sum(simout$foi[block_t])/14/N_S

    # FOI-theoretical
    h_theory[t] <- (sum(simout$bites[block_t,])*b)/14/N

    # attack rate (only count amongst proportion of people who started this 2-wk block susceptible)
    ar <- (table(apply(simout$ar[block_t,simout$states[block_t[1],]==0],2,max)) / N_S)
    if(length(ar)>1){
      AR[t] <- ar[["1"]]
    } else {
      AR[t] <- 0
    }

    # estimated FOI
    h_tilde[t] <- -log(1 - AR[t])/14

  }

  # transmission efficiency
  aeff <- h_hat/h_tilde

  # annual metrics
  aEIR <- sum(simout$bites)/(1260/365)/N

  # annual FOI
  aFOI <- sum(simout$foi)/(1260/365)/N

  data.frame(iter=rep(i,90),time=1:90,h_hat=h_hat,AR=AR,h_tilde=h_tilde,
             aeff=aeff,aEIR=aEIR,aFOI=aFOI)
}

close(pb)
stopCluster(cl);rm(cl);gc()



################################################################################
# deprecated
################################################################################
#
# rm(list=ls());gc()
#
# library(here)
#
# source(here::here("data/sampledata.R"))
#
# library(reshape2)
# library(ggplot2)
# library(scales)
#
# N <- 1e3
#
# tdat_gg <- make_tororo(N = N,which = 1)
# # tdat_ff <- make_tororo(N = N,which = 2)
#
# # xi
# tsim_gg <- sapply(X = 1:N,FUN = function(i){
#   # use (size,prob): (xi,gg)
#   # rnbinom(n = 1260,size = tdat_gg$size[[i]],prob = tdat_gg$prob[[i]])
#
#   # use (mu,size): (k,ff)
#   n <- tdat_gg$size[[i]]
#   p <- tdat_gg$prob[[i]]
#   mu <- n*(1-p)/p
#   k <- -(mu*p)/(p-1)
#   rnbinom(n = 1260,mu = mu,size = k)
# })
#
# # tsim_gg_melt <- melt(tsim_gg[,1:1e2])
# # colnames(tsim_gg_melt) <- c("Day","Replicate","EIR")
# # ggplot(data=tsim_gg_melt) +
# #   geom_line(aes(x=Day,group=Replicate,y=EIR),color="firebrick3",alpha=0.05) +
# #   theme_bw()
#
# tsim_gg_mean <- rowMeans(tsim_gg)
# tsim_gg_quant <- t(apply(X = tsim_gg,MARGIN = 1,FUN = function(x){
#   quantile(x,probs=c(0.025,0.975))
# }))
# tsim_gg_dat <- data.frame(Day=1:1260,low=tsim_gg_quant[,1],high=tsim_gg_quant[,2],mean=tsim_gg_mean)
#
# # ggplot(data = tsim_gg_dat) +
# #   geom_line(aes(x=Day,y=mean),color="firebrick3") +
# #   geom_ribbon(aes(x=Day,ymin=low,ymax=high),fill="firebrick3",alpha=0.25) +
# #   theme_bw() +
# #   ylab("EIR") +
# #   ggtitle("Simulated Tororo Data",subtitle = "(size,prob) parameterization (xi)")
#
# kdat_gg <- make_kanungu(N = N,which = 1)
#
# ksim_gg <- sapply(X = 1:N,FUN = function(i){
#   rnbinom(n = 1260,size = kdat_gg$size[[i]],prob = kdat_gg$prob[[i]])
# })
#
# ksim_gg_mean <- rowMeans(ksim_gg)
# ksim_gg_quant <- t(apply(X = ksim_gg,MARGIN = 1,FUN = function(x){
#   quantile(x,probs=c(0.025,0.975))
# }))
# ksim_gg_dat <- data.frame(Day=1:1260,low=ksim_gg_quant[,1],high=ksim_gg_quant[,2],mean=ksim_gg_mean)
#
# # ggplot(data = ksim_gg_dat) +
# #   geom_line(aes(x=Day,y=mean),color="steelblue") +
# #   geom_ribbon(aes(x=Day,ymin=low,ymax=high),fill="steelblue",alpha=0.25) +
# #   theme_bw() +
# #   ylab("EIR") +
# #   ggtitle("Simulated Kanungu Data",subtitle = "(size,prob) parameterization (xi)")
#
# jdat_gg <- make_jinja(N = N,which = 1)
#
# jsim_gg <- sapply(X = 1:N,FUN = function(i){
#   rnbinom(n = 1260,size = jdat_gg$size[[i]],prob = jdat_gg$prob[[i]])
# })
#
# jsim_gg_mean <- rowMeans(jsim_gg)
# jsim_gg_quant <- t(apply(X = jsim_gg,MARGIN = 1,FUN = function(x){
#   quantile(x,probs=c(0.025,0.975))
# }))
# jsim_gg_dat <- data.frame(Day=1:1260,low=jsim_gg_quant[,1],high=jsim_gg_quant[,2],mean=jsim_gg_mean)
#
# # ggplot(data = jsim_gg_dat) +
# #   geom_line(aes(x=Day,y=mean),color="darkorchid3") +
# #   geom_ribbon(aes(x=Day,ymin=low,ymax=high),fill="darkorchid3",alpha=0.25) +
# #   theme_bw() +
# #   ylab("EIR") +
# #   ggtitle("Simulated Jinja Data",subtitle = "(size,prob) parameterization (xi)")
#
# ggplot() +
#   geom_line(data = tsim_gg_dat,aes(x=Day,y=mean),color="firebrick3") +
#   geom_ribbon(data = tsim_gg_dat,aes(x=Day,ymin=low,ymax=high),fill="firebrick3",alpha=0.2) +
#   geom_line(data = ksim_gg_dat,aes(x=Day,y=mean),color="steelblue") +
#   geom_ribbon(data = ksim_gg_dat,aes(x=Day,ymin=low,ymax=high),fill="steelblue",alpha=0.2) +
#   geom_line(data = jsim_gg_dat,aes(x=Day,y=mean),color="darkorchid3") +
#   geom_ribbon(data = jsim_gg_dat,aes(x=Day,ymin=low,ymax=high),fill="darkorchid3",alpha=0.2) +
#   theme_bw() +
#   ylab("EIR") +
#   ggtitle("Simulated PRISM Data",subtitle = "(size,prob) parameterization (xi)") +
#   scale_y_continuous(trans = "log1p")
#
#
# ################################################################################
# #   a tiny little state space ABM
# ################################################################################
#
# library(Rcpp)
#
# N <- 1e3
#
# tdat_gg <- make_tororo(N = N,which = 1)
#
# sourceCpp(here::here("tiny-pfsi.cpp"))
#
# pfpr <- 0.05
#
# simout <- tiny_pfsi(tmax = 1260,nh = N,init = sample(x = c("S","I"),size = N,replace = T,prob = c(1-pfpr,pfpr)),
#                  EIR_size = tdat_gg$size,EIR_prob = tdat_gg$prob,pb = T)
#
#
# df_bite <- data.frame(time=1:nrow(out$bites),mean=rowMeans(out$bites),
#                       low=apply(out$bites,1,function(x){quantile(x,probs = c(0.025))}),
#                       high=apply(out$bites,1,function(x){quantile(x,probs = c(0.975))}))
#
# df_pf <- data.frame(time=1:nrow(out$states),
#                     pfpr=out$states[,2] / rowSums(out$states),
#                     foi=out$foi)
#
#
#
# # aggregate data into 2-week blocks
# blocks <- seq(from=0,to=1260,by=14)
#
# # FOI
# h_hat <-rep(0,length(blocks)-1)
#
# # attack rate
# AR <-rep(0,length(blocks)-1)
#
# # estimated FOI
# h_tilde <- h_theory <-rep(0,length(blocks)-1)
#
# # loop over blocks
# for(t in 1:(length(blocks)-1)){
#
#   # aggregate over this
#   block_t <- (blocks[t]+1):blocks[t+1]
#
#   # standardize by the number of susceptibles at the start of this 2-wk block
#   N_S <- sum(simout$states[block_t[1],] == 0)
#
#   # FOI (standardize by the number of S individuals at the start of this 2-wk block)
#   h_hat[t] <- sum(simout$foi[block_t])/14/N_S
#
#   # FOI-theoretical
#   h_theory[t] <- (sum(simout$bites[block_t,])*b)/14/N
#
#   # attack rate (only count amongst proportion of people who started this 2-wk block susceptible)
#   ar <- (table(apply(simout$ar[block_t,simout$states[block_t[1],]==0],2,max)) / N_S)
#   if(length(ar)>1){
#     AR[t] <- ar[["1"]]
#   } else {
#     AR[t] <- 0
#   }
#
#   # estimated FOI
#   h_tilde[t] <- -log(1 - AR[t])/14
#
# }
#
# ggplot(data = df_bite) +
#   geom_line(aes(x=time,y=mean),color="firebrick3") +
#   geom_ribbon(aes(x=time,ymin=low,ymax=high),fill="firebrick3",alpha=0.25) +
#   scale_y_continuous(trans = scales::log1p_trans()) +
#   ylab("EIR") +
#   theme_bw()
#
# ggplot(data = df_pf[-1,]) +
#   geom_line(aes(x=time,y=foi),color="firebrick3") +
#   theme_bw()
#
#
# ################################################################################
# #   PRISM burnin
# ################################################################################
#
# N <- 1e3
# nrep <- 1e3
#
# library(doSNOW)
# library(foreach)
# library(parallel)
#
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
#
# ggplot(data=melt(burnin)) +
#   geom_boxplot(aes(Var2,value,fill=Var2),alpha=0.5) +
#   theme_bw()
#
#
# ################################################################################
# #   PRISM simulation
# ################################################################################
#
# N <- 1e4
# tdat_gg <- make_tororo(N = N,which = 1)
# pfpr <- pfpr_means[["Tororo"]]
#
# out <- tiny_pfsi(tmax = 1260,nh = N,init = sample(x = c("S","I"),size = N,replace = T,prob = c(1-pfpr,pfpr)),
#                  EIR_size = tdat_gg$size,EIR_prob = tdat_gg$prob,pb = T)
#
# df_bite <- data.frame(time=1:nrow(out$bites),mean=rowMeans(out$bites),
#                       low=apply(out$bites,1,function(x){quantile(x,probs = c(0.025))}),
#                       high=apply(out$bites,1,function(x){quantile(x,probs = c(0.975))}))
#
# df_pf <- data.frame(time=1:nrow(out$states),
#                     pfpr=out$states[,2] / rowSums(out$states),
#                     foi=out$foi,
#                     scale_foi=out$foi/out$states[,1]
#                     )
#
# # ggplot(data = df_bite) +
# #   geom_line(aes(x=time,y=mean),color="firebrick3") +
# #   geom_ribbon(aes(x=time,ymin=low,ymax=high),fill="firebrick3",alpha=0.25) +
# #   scale_y_continuous(trans = scales::log1p_trans()) +
# #   ylab("EIR") +
# #   theme_bw()
#
# ggplot(data = df_pf[-(1:2),]) +
#   geom_line(aes(x=time,y=scale_foi),color="firebrick3") +
#   theme_bw()
#
#
# # run some ensemble sims
# library(doSNOW)
# library(foreach)
# library(parallel)
#
# nrep <- 2e2
# rm(tiny_pfsi)
#
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
# sim_tororo <- foreach(i = 1:nrep, .combine="rbind",.options.snow=opts) %dopar% {
#
#   N <- 1e4
#   dat <- make_tororo(N = N,which = 1)
#   pfpr <- pfpr_means[["Tororo"]]
#
#   out <- tiny_pfsi(tmax = 1260,nh = N,init = sample(x = c("S","I"),size = N,replace = T,prob = c(1-pfpr,pfpr)),
#                    EIR_size = dat$size,EIR_prob = dat$prob,pb = T)
#
#   data.frame(iter=rep(i,nrow(out$states)),
#              time=1:nrow(out$states),
#              scale_foi=out$foi/out$states[,1])
# }
#
# close(pb)
# stopCluster(cl);rm(cl);gc()
#
# ggplot(data = sim_tororo[sim_tororo$time!=c(1,2),]) +
#   geom_line(aes(x=time,y=scale_foi,group=iter),alpha=0.05) +
#   theme_bw()
