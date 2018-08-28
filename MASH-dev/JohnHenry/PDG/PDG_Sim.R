source('PDG.R')

human = PDGHuman$new()
human$infect_Human()


Nweeks = 50
for(t in seq(1:Nweeks)){
  human$update_Human()
#  if(rbinom(1,1,.01)==1){
#    human$infect_Human()
#  }
}

Pt = log10(10^human$get_history()$Pt/(5*10^6))
Gt = log10(10^human$get_history()$Gt/(5*10^6))
Imm = human$get_history()$Imm

plot(seq(1:Nweeks),Pt,type="l",xlab="weeks",ylab="log10 Parasite Densities per microliter")
lines(Gt,lty=2,col="red")
title(main="Example PDG Parasite Densities")
abline(h=4,lty=2)
plot(Pt>4,xlab="weeks",ylab="Fever Status")
title(main="Example Fever Status (Binary State)")
plot(seq(1:Nweeks),Imm,type="l",ylim=c(0,1),xlab="Weeks",ylab="Immune Level")
title(main="Immunity During Example Infection")
abline(h=1)

