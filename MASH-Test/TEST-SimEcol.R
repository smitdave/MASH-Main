###############################################################################
#       __  ______   _____ __  __      __  ______   __________  ____
#      /  |/  /   | / ___// / / /     /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| | \__ \/ /_/ /_____/ /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ |___/ / __  /_____/ /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_/____/_/ /_/     /_/  /_/_/  |_\____/_/ |_|\____/
#
#   MASH-MACRO
#   Testing Ground
#   August 31, 2017
#
###############################################################################

rm(list=ls());gc()

# # NOTE:
# #  we use cosine rather than sine because cos(0) = 1
# #  such that the parameter O references the seasonal peak
# 
# #  A = amplitude
# #  m = mean; if m<1 negative numbers are set to zero
# #  Q = number of peaks per year, can be < 1
# #  O = the peak (relative to 0)
# #  k1,k2  are shape parameters
# seasonalSignal <- function(t,A=1,m=1,O=0,k1=3,k2=1,Q=1){
#   pmax(0, A*(m + cos(2*Q*pi*(t-O)/365))^k1, 0)^k2
# }








# seasonF = function(t,par=c(A=1,m=1,O=0,k1=3,k2=1,Q=1)){with(as.list(par),{
#   # NOTE:
#   #  we use cosine rather than sine because cos(0) = 1
#   #  such that the parameter O references the seasonal peak
# 
#   #  A = amplitude
#   #  m = mean; if m<1 negative numbers are set to zero
#   #  Q = number of peaks per year, can be < 1
#   #  O = the peak (relative to 0)
#   #  k1,k2  are shape parameters
# 
#   pmax(0, A*(m + cos(2*Q*pi*(t-O)/365))^k1, 0)^k2
# })}
# 
# sFi = function(i, t, PAR){
#   seasonF(t,PAR$pars[i,])
# }
# 
# seasonGenF = function(t, PAR){
#   apply(sapply(1:PAR$N,sFi),1,prod,PAR=PAR)
# }
# 
# 
# #tt = c(0:7300)
# 
# #plot(tt, seasonGenF(tt,PAR), type = "l")
# 
# changePar = function(x,name){
#   switch(standardMenu("ask",
#     msg = paste("Changing ", name, "\n: Current value is :", name, "\n"),
#     ttl = "What do you want to do?\n",
#     menuText = c("Increase it by 10 percent", "Reduce it by 10 percent", "Input value"),
#     clrscrn =FALSE),
#     #------------------------
#     "1" = 1.1*x,
#     "2" = 0.9*x,
#     "3" = getNumber(),
#     "0" = x
#   )
# }
# 
# setupSeasonF = function(params=c(A=1,m=1,O=0,k1=1,k2=2,Q=1)){
# 
#   names = c("A", "m", "O", "k1", "k2", "Q")
# 
#   ix = 1
#   while(ix > 0){
#    tt = seq(1:(365*ceiling(1/params[6]+.001)))
#    plot(tt, seasonF(tt, params), type = "l")
#    ix = switch(standardMenu("ask",
#      ttl = "Which parameter do you want to change?\n",
#      menuText = c(names, "No more changes"),
#      clrscrn = FALSE),
#      #------------------------
#      "1" = 1,
#      "2" = 2,
#      "3" = 3,
#      "4" = 4,
#      "5" = 5,
#      "6" = 6,
#      "7" = 0,
#      "0" = 1
#     )
#     if(ix>0) params[ix] = changePar(params[ix], names[ix])
#     with(as.list(par),
#       lines(tt, seasonF(tt, params), type = "l", lwd = 2)
#     )
#   }
#   params
# }

#plotGenF = function(PAR){
#   tt = seq(1:(365*ceiling(max(1/par[[6]]+.001))))
#   par(mfrow = c(2,1))
#   allParts = sapply(1:PAR$N,sFi, PAR=par)
#   plot(tt, allParts[1,], ylim = range(0,allParts))
#   for(i in 2:par$N){
#     lines(tt, allParts[i,])
#   }
#   plot(tt,seasonGenF(tt, PAR), type = "l")
#}
#
#setupGenF= function(
#  par = list(
#    N=3,
#    pars = data.frame(cbind(
#    A=c(1,3,1),
#    m=c(1,.5,1),
#    O=c(0,30,1),
#    k1=c(1,1,1),
#    k2=c(2,1,1),
#    Q =c(1,2,.41))
#
# )
#){
#  ix = 1
#  while(ix>0){
#    plotGenF(PAR)
#    cat("Which component do you want to change? (0 to accept current form)\n")
#    N = getInteger()
#  }
#  par
#}



#par = setupSeasonF()




# Tmax=20
# 
# lambda <- function(x){
#   100*(sin(2*x*pi)+1)
# }
# 
# curve(expr = lambda,from = 0,to = 1,n = 1000,type = "l",main = "time-varying rate function (NHPP)")
# 
# integrate(f = lambda,lower = 0,upper = 1,subdivisions = 1e3)$value
# 
# Lambda <- function(t){
#   integrate(f=lambda,lower=0,upper=t)$value
# }
# 
# Lambda <- Vectorize(FUN = Lambda)
# 
# v=seq(0,Tmax,length=1000)
# s=0
# X=0
# 
# t=min(v[which(Lambda(v)>=s)])
# 
# while(X[length(X)]<Tmax){
#   u=runif(1)
#   s=s-log(u)
#   t=min(v[which(Lambda(v)>=s)])
#   if(t>Tmax){break()}
#   X=c(X,t)
#   print(paste0("sampled t: ",t))
# }
# 
# hist(X,breaks=seq(0,max(X)+1,by=.1),col="steelblue",main="NHPP trajectory",xlab="Time",ylab="")
#    u=seq(0,max(X),by=.02)
#    lines(u,lambda(u)/10,lwd=2,col="red")
# 
# cutX = cut(x = X,breaks=0:20,right = FALSE)
# table(cutX)
