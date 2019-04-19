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

nrep <- 1e1
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

    # FOI
    h_hat[t] <- sum(simout$foi[block_t,])/14/N

    # attack rate
    AR[t] <- (table(apply(simout$ar[block_t,],2,max)) / N)[["1"]]

    # estimated FOI
    h_tilde[t] <- -log(1 - AR[t])/14

  }

  # transmission efficiency
  aeff <- h_hat/h_tilde

  data.frame(iter=rep(i,90),time=1:90,h_hat=h_hat,AR=AR,h_tilde=h_tilde,aeff=aeff)
}

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

    # FOI
    h_hat[t] <- sum(simout$foi[block_t,])/14/N

    # attack rate
    AR[t] <- (table(apply(simout$ar[block_t,],2,max)) / N)[["1"]]

    # estimated FOI
    h_tilde[t] <- -log(1 - AR[t])/14

  }

  # transmission efficiency
  aeff <- h_hat/h_tilde

  data.frame(iter=rep(i,90),time=1:90,h_hat=h_hat,AR=AR,h_tilde=h_tilde,aeff=aeff)
}

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

    # FOI
    h_hat[t] <- sum(simout$foi[block_t,])/14/N

    # attack rate
    AR[t] <- (table(apply(simout$ar[block_t,],2,max)) / N)[["1"]]

    # estimated FOI
    h_tilde[t] <- -log(1 - AR[t])/14

  }

  # transmission efficiency
  aeff <- h_hat/h_tilde

  data.frame(iter=rep(i,90),time=1:90,h_hat=h_hat,AR=AR,h_tilde=h_tilde,aeff=aeff)
}

close(pb)
stopCluster(cl);rm(cl);gc()


################################################################################
# combine and plot the results
################################################################################

# transmission inefficiency over time
simout_t$site <- rep("Tororo",nrow(simout_t))
simout_k$site <- rep("Kanungu",nrow(simout_k))
simout_j$site <- rep("Jinja",nrow(simout_j))

simout <- rbind(simout_t,simout_k,simout_j)

ggplot(data = simout) +
  geom_line(aes(x=time,y=aeff,color=site,group=interaction(iter,site)),alpha=0.15) +
  scale_color_manual(values = c(Tororo="darkred",Kanungu="darkgreen",Jinja="darkblue")) +
  # scale_color_manual(values = c(Tororo="firebrick3",Kanungu="steelblue",Jinja="darkorchid3")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size = 2))) +
  scale_y_log10(breaks = c(2.2, 4.4, 9.5),labels = c("1.7:1","2.7:1","7.4:1")) +
  scale_x_continuous(breaks = (0:3)*26,labels = as.character(0:3)) +
  ylab("Transmission Efficiency") +
  xlab("Time (Years)") +
  theme_bw()


################################################################################
# Simultion runs for aEIR:aFOI v. aEIR plot
# just need one run for each site
################################################################################

sourceCpp(here::here("tiny-pfmoi.cpp"))
N <- 1e4
pfmoi <- rep(0L,N)

# tororo
simdat_t <- make_tororo(N = N,which = 1)
simout2_t <- tiny_pfmoi(tmax = 1260,nh = N,init = pfmoi,
                        EIR_size = simdat_t$size,EIR_prob = simdat_t$prob,pb = TRUE)

# aEIR_t <- colSums(simout2_t$bites)/(1260/365)




simdat_k <- make_kanungu(N = N,which = 1)
simout2_k <- tiny_pfmoi(tmax = 1260,nh = N,init = pfmoi,
                        EIR_size = simdat_k$size,EIR_prob = simdat_k$prob,pb = TRUE)

simdat_j <- make_jinja(N = N,which = 1)
simout2_j <- tiny_pfmoi(tmax = 1260,nh = N,init = pfmoi,
                        EIR_size = simdat_j$size,EIR_prob = simdat_j$prob,pb = TRUE)



aEIR_t <- colSums(simout2_t$bites)/(1260/365)
aFOI_t <- colSums(simout2_t$MOI)/(1260/365)

aFOI_ar_t <- -log(1 - rowSums(simout2_t$ar)/N)
aFOI_ar_t <- mean(aFOI_ar_t)*365

aEIR_k <- colSums(simout2_k$bites)/(1260/365)
aFOI_k <- colSums(simout2_k$MOI)/(1260/365)

aFOI_ar_k <- -log(1 - rowSums(simout2_k$ar)/N)
aFOI_ar_k <- mean(aFOI_ar_k)*365

aEIR_j <- colSums(simout2_j$bites)/(1260/365)
aFOI_j <- colSums(simout2_j$MOI)/(1260/365)

aFOI_ar_j <- -log(1 - rowSums(simout2_j$ar)/N)
aFOI_ar_j <- mean(aFOI_ar_j)*365


# # transmission inefficiency vs. aEIR
# aEIR_t <- sapply(unique(simout_t$iter),function(x){unique(simout_t[simout_t$iter==x,"aEIR"])})
# aFOI_t <- sapply(unique(simout_t$iter),function(x){unique(simout_t[simout_t$iter==x,"aFOI"])})
#
# aEIR_k <- sapply(unique(simout_k$iter),function(x){unique(simout_k[simout_k$iter==x,"aEIR"])})
# aFOI_k <- sapply(unique(simout_k$iter),function(x){unique(simout_k[simout_k$iter==x,"aFOI"])})
#
# aEIR_j <- sapply(unique(simout_j$iter),function(x){unique(simout_j[simout_j$iter==x,"aEIR"])})
# aFOI_j <- sapply(unique(simout_j$iter),function(x){unique(simout_j[simout_j$iter==x,"aFOI"])})

aeff_df <- data.frame(
  aEIR = c(aEIR_t,aEIR_k,aEIR_j),
  aFOI = c(aFOI_t,aFOI_k,aFOI_j),
  aFOI_ar = c(aFOI_ar_t,aFOI_ar_k,aFOI_ar_t),
  site = rep(c("Tororo","Kanungu","Jinja"),each = length(aEIR_t))
)

# aeff_df$aeff <- aeff_df$aEIR / aeff_df$aFOI
aeff_df$aeff <- aeff_df$aEIR / aeff_df$aFOI_ar

plot(x = aeff_df$aEIR,y = aeff_df$aeff,type = "n",log = "xy")
points(x = aeff_df[aeff_df$site=="Tororo",c("aEIR","aeff")],col=adjustcolor("darkred",alpha.f = 0.05),pch=16)
points(x = aeff_df[aeff_df$site=="Kanungu",c("aEIR","aeff")],col=adjustcolor("darkgreen",alpha.f = 0.05),pch=16)
points(x = aeff_df[aeff_df$site=="Jinja",c("aEIR","aeff")],col=adjustcolor("darkblue",alpha.f = 0.05),pch=16)

# aEIR_aFOI_df$aeff <- aEIR_aFOI_df$aEIR/aEIR_aFOI_df$aFOI



###############################################################################
# run a single site
################################################################################

# sourceCpp(here::here("tiny-pfmoi.cpp"))
#
# N <- 1e4
# simdat <- make_tororo(N = N,which = 1)
#
# pfmoi <- rep(0L,N)
#
# simout <- tiny_pfmoi(tmax = 1260,nh = N,init = pfmoi,
#                      EIR_size = simdat$size,EIR_prob = simdat$prob,pb = TRUE)
#
# # df_bite <- data.frame(time=1:nrow(simout$bites),
# #                       mean=rowMeans(simout$bites),
# #                       median=apply(simout$bites,1,function(x){quantile(x,probs=c(0.5))}),
# #                       low=apply(simout$bites,1,function(x){quantile(x,probs = c(0.025))}),
# #                       high=apply(simout$bites,1,function(x){quantile(x,probs = c(0.975))}))
# #
# # ggplot(data = df_bite) +
# #   geom_line(aes(x=time,y=mean),color="firebrick3") +
# #   geom_ribbon(aes(x=time,ymin=low,ymax=high),fill="firebrick3",alpha=0.25) +
# #   scale_y_continuous(trans = scales::log1p_trans()) +
# #   ylab("EIR") +
# #   theme_bw()
# #
# # df_foi <- data.frame(time=seq_along(simout$foi),
# #                      foi_scale=simout$foi/N
# #                      )
# #
# # ggplot(data = df_foi) +
# #   geom_line(aes(x=time,y=foi_scale)) +
# #   theme_bw()
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
# h_tilde <-rep(0,length(blocks)-1)
#
# # loop over blocks
# for(t in 1:(length(blocks)-1)){
#
#   # aggregate over this
#   block_t <- (blocks[t]+1):blocks[t+1]
#
#   # FOI
#   h_hat[t] <- sum(simout$foi[block_t])/14/N
#
#   # attack rate
#   AR[t] <- (table(apply(simout$ar[block_t,],2,max)) / N)[["1"]]
#
#   # estimated FOI
#   h_tilde[t] <- -log(1 - AR[t])/14
#
# }
#
# aeff <- h_hat/h_tilde
#
# plot(h_hat,type="l",col="firebrick3",lty=1,lwd=2)
# lines(h_tilde,col="firebrick3",lty=2,lwd=2)
#
#
#
# wks <- 1:90
# plot (wks, aeff, type = "l", lwd=2, xlab = "Time (Years)", yaxt = "n", ylab = "Transmission Efficiency", xaxt = "n", col = "darkred", ylim = range(1,30), log="y")
# # lines(wks, J0$aeff, lwd=2, col = "darkblue")
# # lines(wks, K0$aeff, lwd=2, col = "darkgreen")
# mtext("d)",line=1,at=0)
#
# axis(2, c(2.2, 4.4, 9.5), c("1.7:1","2.7:1","7.4:1"))
# axis(1, c(1,366,731,1096)/14, c(0,1,2,3))
