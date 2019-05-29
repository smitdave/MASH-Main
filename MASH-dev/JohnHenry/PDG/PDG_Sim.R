source('PDG.R')

set.seed(534)
human = PDGHuman$new()
human$infect_Human(5)
dt = 1/26

Nfortnights = 30
for(t in seq(1:Nfortnights)){
  human$update_Human(dt=dt)
}

Pt = human$get_history()$Pt
Gt = human$get_history()$Gt
Imm = human$get_history()$Imm
pFever = human$get_history()$pFever

immCounter = human$get_history()$immCounter

# par(mfrow=c(2,1))
# plot(Pt,type="l",ylim=c(0,5),xlab="fortnights",ylab="log10 Parasite Densities per microliter")
# plot(pFever,type="l",ylim=c(0,1),xlab="fortnights")
# par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(seq(1:Nfortnights),Pt,type="l",xlab="fortnights",ylab="log10 Parasite Densities per microliter",ylim=c(0,5))
lines(Gt,lty=2,col="red")
title(main="Example PDG Parasite Densities")

TE = human$get_history()$TE
plot(TE,ylim=c(0,1),main="Transmission Efficiency",xlab="fortnights")
par(mfrow=c(1,1))

# # C++ checking
# set.seed(534)
# human_cpp <- sim_PDG(tmax = 30,ninf = 5)
# 
# par(mfrow=c(2,1))
# plot(seq(1:Nfortnights),human_cpp$Pt,type="l",xlab="fortnights",ylab="log10 Parasite Densities per microliter",ylim=c(0,5))
# lines(human_cpp$Gt,lty=2,col="red")
# title(main="Example PDG Parasite Densities (C++)")
# 
# plot(human_cpp$TE,ylim=c(0,1),main="Transmission Efficiency (C++)",xlab="fortnights")
# par(mfrow=c(1,1))
