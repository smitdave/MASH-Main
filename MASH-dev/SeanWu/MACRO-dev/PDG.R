rm(list=ls());gc()

tridiag = function(upper, lower, main) {
  out = matrix(0,length(main),length(main))
  diag(out) = main
  indx = seq.int(length(upper))
  out[cbind(indx+1,indx)] = lower
  out[cbind(indx,indx+1)] = upper
  return(out)
}

ageMatrix = function(size){
  u = rep(0,size-1)
  l = rep(1,size-1)
  m = rep(0,size)
  m[size] = 1
  tridiag(u,l,m)
}

pfAges <- 16
Pt0 <- 5.34
Ptmu = log(c(0,8,10,12,11.5,11,10.5,10,9.5,9,8.5,8,7.5,7,7,7)) ## mean of lognormal densities for ages 1:pfAges; must be of length pfAges
Ptvar = rep(.1,16) 
Pf <- rpois(n = 16,lambda = 10)


# john henry's example
source("/Users/slwu89/Desktop/git/MASH-Main/MASH-dev/JohnHenry/PDG/PDG.R")

set.seed(42)

human = PDGHuman$new()
human$infect_Human()


Nweeks = 50
for(t in seq(1:Nweeks)){
  human$update_Human()
}

Pt = log10(10^human$get_history()$Pt/(5*10^6))
Gt = log10(10^human$get_history()$Gt/(5*10^6))
Imm = human$get_history()$Imm

par(mfrow=c(2,2))
plot(seq(1:Nweeks),Pt,type="l",xlab="weeks",ylab="log10 Parasite Densities per microliter")
lines(Gt,lty=2,col="red")
title(main="Example PDG Parasite Densities")
abline(h=4,lty=2)
plot(Pt>4,xlab="weeks",ylab="Fever Status")
title(main="Example Fever Status (Binary State)")
plot(seq(1:Nweeks),Imm,type="l",ylim=c(0,1),xlab="Weeks",ylab="Immune Level")
title(main="Immunity During Example Infection")
abline(h=1)
par(mfrow=c(1,1))

# test c++
Rcpp::sourceCpp('~/Desktop/git/MASH-Main/MASH-dev/SeanWu/MACRO-dev/PDG.cpp')

# defaults used in the constructor of PDGHuman
Ptmu = log(c(0,8,10,12,11.5,11,10.5,10,9.5,9,8.5,8,7.5,7,7,7)) ## mean of lognormal densities for ages 1:pfAges; must be of length pfAges
Ptvar = rep(.1,16) 

set.seed(42)

cppout <- runPDG(tmax = Nweeks,Ptmu = Ptmu,Ptvar = Ptvar)

Pt = log10(10^cppout$Pt/(5*10^6))
Gt = log10(10^cppout$Gt/(5*10^6))
Imm = cppout$Imm

par(mfrow=c(2,2))
plot(seq(1:Nweeks),Pt,type="l",xlab="weeks",ylab="log10 Parasite Densities per microliter")
lines(Gt,lty=2,col="red")
title(main="Example PDG Parasite Densities")
abline(h=4,lty=2)
plot(Pt>4,xlab="weeks",ylab="Fever Status")
title(main="Example Fever Status (Binary State)")
plot(seq(1:Nweeks),Imm,type="l",ylim=c(0,1),xlab="Weeks",ylab="Immune Level")
title(main="Immunity During Example Infection")
abline(h=1)
par(mfrow=c(1,1))