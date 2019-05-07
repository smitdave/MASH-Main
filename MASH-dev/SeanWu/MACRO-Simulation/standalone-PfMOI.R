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


################################################################################
# ensemble simulation run for Tororo
################################################################################

library(doSNOW)
library(foreach)
library(parallel)

nrep <- 1e2
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
simout_t <- foreach(i = 1:nrep, .combine="rbind",.options.snow=opts) %dopar% {

  N <- 1e4
  simdat <- make_tororo(N = N,which = 1)

  pfmoi <- rep(0L,N)

  simout <- tiny_pfmoi(tmax = 1260,nh = N,init = pfmoi,
                       EIR_size = simdat$size,EIR_prob = simdat$prob,pb = FALSE)

  # aggregate data into 2-week blocks
  blocks <- seq(from=0,to=1260,by=14)

  # EIR
  EIR <- rep(0,length(blocks)-1)

  # FOI
  h_hat <-rep(0,length(blocks)-1)

  # attack rate
  AR <-rep(0,length(blocks)-1)

  # estimated FOI
  h_tilde <-rep(0,length(blocks)-1)

  # loop over blocks
  for(t in 1:(length(blocks)-1)){

    # aggregate over this
    block_t <- (blocks[t]+1):blocks[t+1]

    # EIR
    EIR[t] <- sum(simout$bites[block_t,])/14/N

    # FOI
    h_hat[t] <- sum(simout$foi[block_t,])/14/N

    # attack rate
    AR[t] <- (table(apply(simout$ar[block_t,],2,max)) / N)[["1"]]

    # estimated FOI
    h_tilde[t] <- -log(1 - AR[t])/14

  }

  # transmission efficiency
  aeff <- h_hat/h_tilde

  data.frame(iter=rep(i,90),time=1:90,h_hat=h_hat,AR=AR,h_tilde=h_tilde,aeff=aeff,EIR=EIR)
}

# saveRDS(object = simout_t,file = here::here("sim/PfMOI_het_t.rds"))

close(pb)
stopCluster(cl);rm(cl);gc()


################################################################################
# ensemble simulation run for Kanungu
################################################################################

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
simout_k <- foreach(i = 1:nrep, .combine="rbind",.options.snow=opts) %dopar% {

  N <- 1e4
  simdat <- make_kanungu(N = N,which = 1)

  pfmoi <- rep(0L,N)

  simout <- tiny_pfmoi(tmax = 1260,nh = N,init = pfmoi,
                       EIR_size = simdat$size,EIR_prob = simdat$prob,pb = FALSE)

  # aggregate data into 2-week blocks
  blocks <- seq(from=0,to=1260,by=14)

  # EIR
  EIR <- rep(0,length(blocks)-1)

  # FOI
  h_hat <-rep(0,length(blocks)-1)

  # attack rate
  AR <-rep(0,length(blocks)-1)

  # estimated FOI
  h_tilde <-rep(0,length(blocks)-1)

  # loop over blocks
  for(t in 1:(length(blocks)-1)){

    # aggregate over this
    block_t <- (blocks[t]+1):blocks[t+1]

    # EIR
    EIR[t] <- sum(simout$bites[block_t,])/14/N

    # FOI
    h_hat[t] <- sum(simout$foi[block_t,])/14/N

    # attack rate
    AR[t] <- (table(apply(simout$ar[block_t,],2,max)) / N)[["1"]]

    # estimated FOI
    h_tilde[t] <- -log(1 - AR[t])/14

  }

  # transmission efficiency
  aeff <- h_hat/h_tilde

  data.frame(iter=rep(i,90),time=1:90,h_hat=h_hat,AR=AR,h_tilde=h_tilde,aeff=aeff,EIR=EIR)
}

# saveRDS(object = simout_k,file = here::here("sim/PfMOI_het_k.rds"))

close(pb)
stopCluster(cl);rm(cl);gc()


################################################################################
# ensemble simulation run for Jinja
################################################################################

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
simout_j <- foreach(i = 1:nrep, .combine="rbind",.options.snow=opts) %dopar% {

  N <- 1e4
  simdat <- make_jinja(N = N,which = 1)

  pfmoi <- rep(0L,N)

  simout <- tiny_pfmoi(tmax = 1260,nh = N,init = pfmoi,
                       EIR_size = simdat$size,EIR_prob = simdat$prob,pb = FALSE)

  # aggregate data into 2-week blocks
  blocks <- seq(from=0,to=1260,by=14)

  # EIR
  EIR <- rep(0,length(blocks)-1)

  # FOI
  h_hat <-rep(0,length(blocks)-1)

  # attack rate
  AR <-rep(0,length(blocks)-1)

  # estimated FOI
  h_tilde <-rep(0,length(blocks)-1)

  # loop over blocks
  for(t in 1:(length(blocks)-1)){

    # aggregate over this
    block_t <- (blocks[t]+1):blocks[t+1]

    # EIR
    EIR[t] <- sum(simout$bites[block_t,])/14/N

    # FOI
    h_hat[t] <- sum(simout$foi[block_t,])/14/N

    # attack rate
    AR[t] <- (table(apply(simout$ar[block_t,],2,max)) / N)[["1"]]

    # estimated FOI
    h_tilde[t] <- -log(1 - AR[t])/14

  }

  # transmission efficiency
  aeff <- h_hat/h_tilde

  data.frame(iter=rep(i,90),time=1:90,h_hat=h_hat,AR=AR,h_tilde=h_tilde,aeff=aeff,EIR=EIR)
}

# saveRDS(object = simout_j,file = here::here("sim/PfMOI_het_j.rds"))

close(pb)
stopCluster(cl);rm(cl);gc()


################################################################################
# transmission efficiency vs. time (Fig 4D)
################################################################################

# transmission inefficiency over time
simout_t$site <- rep("Tororo",nrow(simout_t))
simout_k$site <- rep("Kanungu",nrow(simout_k))
simout_j$site <- rep("Jinja",nrow(simout_j))

simout <- rbind(simout_t,simout_k,simout_j)

saveRDS(object = simout,file = here::here("sim/PfMOI_het.rds"))

# fig4a <- ggplot(data = simout_t) +
#   geom_line(aes(x=time,y=h_hat,group=iter),color="darkred",alpha=0.15) +
#   geom_line(aes(x=time,y=h_tilde,group=iter),color="firebrick3",alpha=0.15) +
#   scale_x_continuous(breaks = (0:3)*26,labels = 0:3,name = "Time (Years)") +
#   ylab("daily FOI (simulated)") +
#   ggtitle("b) PfMOI: Tororo") +
#   theme_bw()
#
# fig4b <- ggplot(data = simout_k) +
#   geom_line(aes(x=time,y=h_hat,group=iter),color="darkblue",alpha=0.15) +
#   geom_line(aes(x=time,y=h_tilde,group=iter),color="steelblue",alpha=0.15) +
#   scale_x_continuous(breaks = (0:3)*26,labels = 0:3,name = "Time (Years)") +
#   ylab("daily FOI (simulated)") +
#   ggtitle("b) PfMOI: Kanungu") +
#   theme_bw()
#
# fig4c <- ggplot(data = simout_j) +
#   geom_line(aes(x=time,y=h_hat,group=iter),color="darkgreen",alpha=0.15) +
#   geom_line(aes(x=time,y=h_tilde,group=iter),color="forestgreen",alpha=0.15) +
#   scale_x_continuous(breaks = (0:3)*26,labels = 0:3,name = "Time (Years)") +
#   ylab("daily FOI (simulated)") +
#   ggtitle("c) PfMOI: Jinja") +
#   theme_bw()
#
# # transmission efficiency (4D: FOI normalized by N
# fig4d <- ggplot(data = simout) +
#   geom_line(aes(x=time,y=aeff,color=site,group=interaction(iter,site)),alpha=0.15) +
#   scale_color_manual(values = c(Tororo="darkred",Kanungu="darkblue",Jinja="darkgreen")) +
#   # scale_color_manual(values = c(Tororo="firebrick3",Kanungu="steelblue",Jinja="darkorchid3")) +
#   # guides(colour = guide_legend(override.aes = list(alpha = 1,size = 2))) +
#   guides(colour = FALSE) +
#   scale_y_log10(breaks = c(2.2, 4.4, 9.5),labels = c("1.7:1","2.7:1","7.4:1")) +
#   scale_x_continuous(breaks = (0:3)*26,labels = as.character(0:3)) +
#   ylab("Transmission Efficiency") +
#   xlab("Time (Years)") +
#   ggtitle("d)") +
#   theme_bw()
#
# fig4 <- grid.arrange(fig4a,fig4b,fig4c,fig4d,nrow=2,ncol=2)
#
# ggsave(filename = here::here("figures/pfmoi_4.pdf"),plot = fig4,device = "pdf",width = 12,height = 10)


################################################################################
#   exposure vs. infection (Fig 2D)
################################################################################

# run sims
N <- 1e4
pfmoi <- rep(0L,N)
Rcpp::sourceCpp(here::here("tiny-pfmoi.cpp"))

simdat_t <- make_tororo(N = N,which = 1)
simout_t <- tiny_pfmoi(tmax = 1260,nh = N,init = pfmoi,
                    EIR_size = simdat_t$size,EIR_prob = simdat_t$prob,pb = TRUE)

simdat_k <- make_kanungu(N = N,which = 1)
simout_k <- tiny_pfmoi(tmax = 1260,nh = N,init = pfmoi,
                      EIR_size = simdat_k$size,EIR_prob = simdat_k$prob,pb = TRUE)

simdat_j <- make_jinja(N = N,which = 1)
simout_j <- tiny_pfmoi(tmax = 1260,nh = N,init = pfmoi,
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

saveRDS(object = ineff_df,file = here::here("sim/PfMOI_het_ineff.rds"))
