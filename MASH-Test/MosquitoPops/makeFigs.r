source ("MOSYde.R") 

#kappa = rep(.05, 365) 
#lambda = rep(250, 365) 

#kappa = (1+sin(2*pi*(60+c(1:365))/365))*0.05
#lambda = (1+sin(2*pi*c(1:365)/365))*250

makeKappa = function(colorednoise, fgnscale){
#  kk = round(runif(1,12,30))
#  dp = 1.2 + cos(2*pi*c(1:365)/kk) 
#  kappa = (1+sin(2*pi*(270+c(1:365))/365))*0.1*dp
#  kappa = (1+sin(2*pi*(270+c(1:365))/365))*0.1
#  kappa <- pmax(kappa+kappa*colorednoise*fgnscale*(.1/40),0)
#  for (i in 0:11){
#    ix = 1:35 + i*30
#    ot = order(runif(35, 0,1)) 
#    kappa[ix] = kappa[ix][ot] 
#  } 
#  kappa + 10^-6
   kappa = rep(0.1,length(colorednoise))
} 

makeLambda = function(colorednoise, fgnscale){
#  kk = round(runif(1,3,12))
#  fac = runif(1, 0, 0.1) 
#  dp = round(1 + fac + sin(2*pi*c(1:365)/kk)) 
#  lambda = (1+sin(2*pi*c(1:365)/365))*40*dp
  lambda = (1 + sin(2*pi*c(1:length(colorednoise))/365))*40
 #  lambda = 2
   lambda <- pmax(lambda+lambda*colorednoise*fgnscale, 0)
  #  kk = round(runif(1,3,12))
#  lambda = lambda*(1+cos(2*pi*c(1:365)/kk))
#  for (i in 0:11){
#    ix = 1:35 + i*30 
#    ot = order(runif(35, 0,1)) 
#    lambda[ix] = lambda[ix][ot] 
#  }
  lambda + 10^-5 
} 

os = function(L=12, p=1,Hurst=0.5, fgnscale=10,kappaval=0.1,reduction=.5){
  colorednoise <- fgnSim(n = 731, H = Hurst, method = "paxson")[-1]
  lambda = makeLambda(colorednoise, fgnscale)
  #lambda =rep(80 ,length(colorednoise))
  kappa = c(rep(kappaval,364),rep(kappaval*(1-reduction),365))#makeKappa(colorednoise, fgnscale)  
  out = oneSample(lambda, kappa, L,p) 
  out
} 



