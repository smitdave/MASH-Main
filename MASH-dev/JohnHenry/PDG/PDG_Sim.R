source('PDG.R')

human = PDGHuman$new()
human$infect_Human()


Nfortnights = 30
for(t in seq(1:Nfortnights)){
  human$update_Human()
#  if(rbinom(1,1,.01)==1){
#    human$infect_Human()
#  }
}

Pt = human$get_history()$Pt
Gt = human$get_history()$Gt
Imm = human$get_history()$Imm
pFever = human$get_history()$pFever

par(mfrow=c(2,1))
plot(Pt,type="l",ylim=c(0,5),xlab="fortnights",ylab="log10 Parasite Densities per microliter")
plot(pFever,type="l",ylim=c(0,1),xlab="fortnights")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
plot(seq(1:Nfortnights),Pt,type="l",xlab="fortnights",ylab="log10 Parasite Densities per microliter",ylim=c(0,5))
lines(Gt,lty=2,col="red")
title(main="Example PDG Parasite Densities")

TE = human$get_history()$TE
plot(TE,ylim=c(0,1),main="Transmission Efficiency",xlab="fortnights")
par(mfrow=c(1,1))

plot(Pt>4,xlab="weeks",ylab="Fever Status")
title(main="Example Fever Status (Binary State)")
#plot(seq(1:Nweeks),Imm,type="l",ylim=c(0,1),xlab="Weeks",ylab="Immune Level")
#title(main="Immunity During Example Infection")

