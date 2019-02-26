rm(list=ls());gc()

theta <- 0.5
lambda <- 20
s <- 1-exp(-(1/10)*7)
p <- 0.5

tmax <- 1e3
M <- rep(0,tmax)
J <- rep(0,tmax)

M[1] <- 5
J[1] <- 0.1

for(t in 2:tmax){
  M[t] <- (s*M[t-1]) + (p*J[t-1])
  J[t] <- lambda*(M[t-1]^theta)
}

tail(M)
tail(J)
