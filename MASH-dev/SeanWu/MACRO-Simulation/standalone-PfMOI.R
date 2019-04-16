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

# df_bite_ind <- melt(simout$bites)
# 
# ggplot(data = df_bite_ind) +
#   geom_line(aes(x=Var1,y=value,group=Var2),alpha=0.05) +
#   theme_bw() +
#   ylab("EIR")



# pfpr <- 0.05
# 
# out <- tiny_pfsi(tmax = 1260,nh = N,init = sample(x = c("S","I"),size = N,replace = T,prob = c(1-pfpr,pfpr)),
#                  EIR_size = tdat_gg$size,EIR_prob = tdat_gg$prob,pb = T)
# 
# 

# 
# df_pf <- data.frame(time=1:nrow(out$states),
#                     pfpr=out$states[,2] / rowSums(out$states),
#                     foi=out$foi)
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