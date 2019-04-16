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

# read in data
tororo <- read.table(here::here("data/TOROROnew.txt"), header=T)
jinja <- read.table (here::here("data/JINJAnew.txt"), header=T)
kanungu <- read.table(here::here("data/KANUNGUnew.txt"), header=T)


################################################################################
#   read in data
################################################################################

# get seasonal signal and sporozoite rate
Jd <- sort(unique(jinja$day))
Kd <- sort(unique(kanungu$day))
Td <- sort(unique(tororo$day))

getSd <- function(d, dt){with(dt,{
  Sd[min(which(day == d))]
})}

JSd <- sapply(Jd, getSd,dt =jinja)
KSd <- sapply(Kd, getSd,dt =kanungu)
TSd <- sapply(Td, getSd,dt =tororo)

getSR <- function(dd,dt,wd=.6){with(dt,{
  wts <- exp(-wd*(dd-day)^2)
  z <- sum(wts*npos)/sum(wts*ntested)
  ifelse(is.na(z), 0, z)
})}

Jz <- sapply(Jd, getSR,dt =jinja, wd=.001)
Kz <- sapply(Kd, getSR,dt =kanungu, wd=.001)
Tz <- sapply(Td, getSR,dt =tororo, wd=.001)
TTz <- sum(tororo$npos,na.rm=T)/sum(tororo$ntested, na.rm=T)
KKz <- sum(kanungu$npos, na.rm=T)/sum(kanungu$ntested, na.rm=T)
JJz <- sum(jinja$npos, na.rm=T)/sum(jinja$ntested, na.rm=T)

# parameters for gamma-distributed noise
Tw <- 2.2
Kw <- 0.68
Jw <- 0.82

# interpolated seasonal signals
JGaps <- c(min(Jd):max(Jd))[-Jd]
KGaps <- c(min(Kd):max(Kd))[-Kd]
TGaps <- c(min(Kd):max(Td))[-Td]

interp <- function(y1,yn,n){
  y1 + (1:n)*(yn-y1)/(n+1)
}

fillGaps <- function(dd, Sd){
  ix0 <- which(diff(dd)>1)
  gap <- diff(dd)[ix0]
  
  ddnew <- min(dd):max(dd)
  Sdnew <- 0*ddnew
  Sdnew[dd] <- Sd
  
  for(i in 1:length(ix0)){
    #if(i==172 & ix0[i] ==574) browser()
    d0 <- dd[ix0[i]]
    dn <- dd[ix0[i]+1]
    y0 <- Sd[ix0[i]]
    yn <- Sd[ix0[i]+1]
    nn <- gap[i]
    vals <- interp(y0,yn,nn)
    
    Sdnew[d0+1:nn] <- interp(y0,yn,nn)
  }
  list(dd=ddnew,Sd=Sdnew)
}

newJ <- fillGaps(Jd,JSd)
JJSd <- newJ$Sd[1:1260]
JJd <- newJ$dd[1:1260]

newK <- fillGaps(Kd,KSd)
KKSd <- newK$Sd[1:1260]
KKd <- newK$dd[1:1260]

newT <- fillGaps(Td,TSd)
TTSd <- newT$Sd[-1][1:1260]
TTd <- newT$dd[1:1260]

# prob (xi: gg) and size (k: ff)

# prob (xi: gg)
fitE.gQ <- function(DTA, qq){with(DTA,{
  bks <- quantile(Sd*w, qq)
  dlik <- function(x, ix){
    a <- log(dnbinom(obs[ix],size=Sd[ix]*w[ix]*x,prob=x/(1+x)))
    -sum(a)
  }
  x <- c(0,0)
  for(i in 2:length(bks)){
    ix <- which(Sd*w > bks[i-1] & Sd*w < bks[i])
    ans <- optimize(f=dlik, interval = c(0,20),ix=ix)
    x <- rbind(x, c(ans$minimum,ans$objective))
  }
  mids <- (bks[-1] + bks[-length(qq)])/2
  # col1 = midpoints of the quantiles
  # col2 = MLE estimate for that quantile
  # col3 = the likelihood
  cbind(mids, x[-1,])
})}

gg.mu <- function(mu,P){
  exp(P[1])*exp(log(mu)*P[2])
}

fitit.gQ <- function(N, data, plotit=TRUE, pointsit=FALSE){
  xx <- fitE.gQ(data, c(0:N)/N)
  ix <- which(xx[,1]<.1)
  xx <- xx[-ix,]
  ix <- which(xx[,2]<19)
  mu <- xx[ix,1]
  x <- xx[ix,2]
  llm <- lm(log(x)~log(mu))
  #gg = exp(coef(llm)[1])*exp(log(mu)*coef(llm)[2])
  gg <- gg.mu(mu, coef(llm))
  if(plotit){
    plot(mu, mu*x, log = "xy", xlim = c(0.1, 500), ylim = c(.1,10))
    lines(mu, mu*gg)
  }
  if(pointsit){
    points(mu, mu*x, pch=3, col = "blue")
    lines(mu, mu*gg, col = "blue")
  }
  return(coef(llm))
}

# par(mfrow=c(3,1))
J.g <- fitit.gQ(61, jinja, 0, 0)
fitit.gQ(51, jinja, 0, 0)
# text(10,5, "Jinja")
K.g <- fitit.gQ(60, kanungu, 0,0)
fitit.gQ(30, kanungu, 0,0)
# text(40,.3, "Kanungu")
T.g <- fitit.gQ(61, tororo,0,0)
fitit.gQ(41, tororo,0,0)
# text(40,.3, "Kanungu")
# par(mfrow=c(1,1))

# size (k: ff)
fitE.fQ<- function(DTA, qq){with(DTA,{
  bks<- quantile(Sd*w, qq)
  dlik<- function(k, ix){
    a <- log(dnbinom(obs[ix],mu=Sd[ix]*w[ix],size=k))
    -sum(a)
  }
  x<- c(0,0)
  for(i in 2:length(bks)){
    ix<- which(Sd*w > bks[i-1] & Sd*w < bks[i])
    ans<- optimize(f=dlik, interval = c(0,20),ix=ix)
    x<- rbind(x, c(ans$minimum,ans$objective))
  }
  mids<- (bks[-1] + bks[-length(qq)])/2
  # col1 = midpoints of the quantiles
  # col2 = MLE estimate for that quantile
  # col3 = the likelihood
  cbind(mids, x[-1,])
})}

ff.mu<- function(mu, P){
  P[1]*exp(log(mu)*P[2])
}

fitit.fQ<- function(N, data, plotit=TRUE, pointsit=FALSE){
  xx <- fitE.fQ(data, c(0:N)/N)
  ix <- which(xx[,1]<.1)
  xx <- xx[-ix,]
  ix <- which(xx[,2]<19)
  mu <- xx[ix,1]
  k <- xx[ix,2]
  llm <- lm(k~log(mu))
  ff <- ff.mu(mu, coef(llm))
  if(plotit){
    plot(mu, k, log="x", xlim = c(0.1, 500), ylim = c(0,4))
    lines(mu, ff)
  }
  if(pointsit){
    points(mu, k, pch=3, col = "blue")
    lines(mu, ff, col = "blue")
  }
  return(coef(llm))
}

# par(mfrow = c(3,1))
J.f <- fitit.fQ(61, jinja, 0, 0)
fitit.fQ(51, jinja, 0, 0)
# text(10, 3, "Jinja")
K.f <- fitit.fQ(60, kanungu, 0,0)
fitit.fQ(30, kanungu, 0,1)
# text(50, 3, "Kanungu")
T.f <- fitit.fQ(61, tororo,0,0)
fitit.fQ(41, tororo,0,1)
# text(1, 3, "Tororo")
# par(mfrow=c(1,1))

# both fits (and make ff.T/K/J and gg.T/K/J functions)
# par(mfrow=c(1,1))
# xx <- 10^seq(-.8, 2.8, length.out=100)
ff.T <- function(mu){ff.mu(mu,T.f)}
# plot(xx,ff.T(xx), log ="x", type = "l", col = "darkred", ylim = c(0,3.5), xlab = expression(mu), ylab= expression(k(mu)), main = "Fitted relationships")
gg.T <- function(mu){gg.mu(mu,T.g)}
# lines(xx,xx*gg.T(xx), col = "darkred", lty=2)

# xx <- 10^seq(-.8, 2.3, length.out=100)
ff.K <- function(mu){ff.mu(mu,K.f)}
# lines(xx,ff.K(xx), col = "darkblue")
gg.K <- function(mu){gg.mu(mu,K.g)}
# lines(xx,xx*gg.K(xx), col = "darkblue",lty=2)

# xx <- 10^seq(-.8, 1.7, length.out=100)
ff.J <- function(mu){ff.mu(mu,J.f)}
# lines(xx,ff.J(xx), col = "darkgreen")
gg.J <- function(mu){gg.mu(mu,J.g)}
# lines(xx,xx*gg.J(xx), col = "darkgreen",lty=2)


################################################################################
#   make data for MACRO
################################################################################

# which: 1 for prob (xi: gg), 2 for size (k: ff)
make_tororo <- function(N, which = 1){
  
  dat <- list()
  
  if(which==1){
    
    dat$w <- rgamma(N,Tw,Tw)
    dat$xi <- lapply(dat$w,function(w){
      gg.T(TTSd*w)
    })
    dat$size <- lapply(1:N,function(i){
      TTSd*dat$w[[i]]*dat$xi[[i]]*TTz
    })
    dat$prob <- lapply(dat$xi,function(xi){
      xi/(1 + xi)
    })
    
  } else if(which==2){
    
    dat$w <- rgamma(N,Tw,Tw)
    dat$k <- lapply(dat$w,function(w){
      ff.T(TTSd*w)
    })
    dat$mu <- lapply(1:N,function(i){
      TTSd*dat$w[[i]]*TTz
    })
    
  } else {
    stop("wrong 'else'")
  }
  
  return(dat)
}

make_kanungu <- function(N, which = 1){
  
  dat <- list()
  
  if(which==1){
    
    dat$w <- rgamma(N,Kw,Kw)
    dat$xi <- lapply(dat$w,function(w){
      gg.K(KKSd*w)
    })
    dat$size <- lapply(1:N,function(i){
      KKSd*dat$w[[i]]*dat$xi[[i]]*KKz
    })
    dat$prob <- lapply(dat$xi,function(xi){
      xi/(1 + xi)
    })
    
  } else if(which==2){
    
    dat$w <- rgamma(N,Kw,Kw)
    dat$k <- lapply(dat$w,function(w){
      ff.K(KKSd*w)
    })
    dat$mu <- lapply(1:N,function(i){
      KKSd*dat$w[[i]]*KKz
    })
    
  } else {
    stop("wrong 'else'")
  }
  
  return(dat)
}

make_jinja <- function(N, which = 1){
  
  dat <- list()
  
  if(which==1){
    
    dat$w <- rgamma(N,Jw,Jw)
    dat$xi <- lapply(dat$w,function(w){
      gg.J(JJSd*w)
    })
    dat$size <- lapply(1:N,function(i){
      JJSd*dat$w[[i]]*dat$xi[[i]]*JJz
    })
    dat$prob <- lapply(dat$xi,function(xi){
      xi/(1 + xi)
    })
    
  } else if(which==2){
    
    dat$w <- rgamma(N,Jw,Jw)
    dat$k <- lapply(dat$w,function(w){
      ff.J(JJSd*w)
    })
    dat$mu <- lapply(1:N,function(i){
      JJSd*dat$w[[i]]*JJz
    })
    
  } else {
    stop("wrong 'else'")
  }
  
  return(dat)
}

library(reshape2)
library(ggplot2)
library(scales)

N <- 1e4

tdat_gg <- make_tororo(N = N,which = 1)
# tdat_ff <- make_tororo(N = N,which = 2)

# xi
tsim_gg <- sapply(X = 1:N,FUN = function(i){
  # use (size,prob): (xi,gg)
  # rnbinom(n = 1260,size = tdat_gg$size[[i]],prob = tdat_gg$prob[[i]])

  # use (mu,size): (k,ff)
  n <- tdat_gg$size[[i]]
  p <- tdat_gg$prob[[i]]
  mu <- n*(1-p)/p
  k <- -(mu*p)/(p-1)
  rnbinom(n = 1260,mu = mu,size = k)
})

# tsim_gg_melt <- melt(tsim_gg[,1:1e2])
# colnames(tsim_gg_melt) <- c("Day","Replicate","EIR")
# ggplot(data=tsim_gg_melt) +
#   geom_line(aes(x=Day,group=Replicate,y=EIR),color="firebrick3",alpha=0.05) +
#   theme_bw()

tsim_gg_mean <- rowMeans(tsim_gg)
tsim_gg_quant <- t(apply(X = tsim_gg,MARGIN = 1,FUN = function(x){
  quantile(x,probs=c(0.025,0.975))
}))
tsim_gg_dat <- data.frame(Day=1:1260,low=tsim_gg_quant[,1],high=tsim_gg_quant[,2],mean=tsim_gg_mean)

# ggplot(data = tsim_gg_dat) +
#   geom_line(aes(x=Day,y=mean),color="firebrick3") +
#   geom_ribbon(aes(x=Day,ymin=low,ymax=high),fill="firebrick3",alpha=0.25) +
#   theme_bw() +
#   ylab("EIR") +
#   ggtitle("Simulated Tororo Data",subtitle = "(size,prob) parameterization (xi)")

kdat_gg <- make_kanungu(N = N,which = 1)

ksim_gg <- sapply(X = 1:N,FUN = function(i){
  rnbinom(n = 1260,size = kdat_gg$size[[i]],prob = kdat_gg$prob[[i]])
})

ksim_gg_mean <- rowMeans(ksim_gg)
ksim_gg_quant <- t(apply(X = ksim_gg,MARGIN = 1,FUN = function(x){
  quantile(x,probs=c(0.025,0.975))
}))
ksim_gg_dat <- data.frame(Day=1:1260,low=ksim_gg_quant[,1],high=ksim_gg_quant[,2],mean=ksim_gg_mean)

# ggplot(data = ksim_gg_dat) +
#   geom_line(aes(x=Day,y=mean),color="steelblue") +
#   geom_ribbon(aes(x=Day,ymin=low,ymax=high),fill="steelblue",alpha=0.25) +
#   theme_bw() +
#   ylab("EIR") +
#   ggtitle("Simulated Kanungu Data",subtitle = "(size,prob) parameterization (xi)")

jdat_gg <- make_jinja(N = N,which = 1)

jsim_gg <- sapply(X = 1:N,FUN = function(i){
  rnbinom(n = 1260,size = jdat_gg$size[[i]],prob = jdat_gg$prob[[i]])
})

jsim_gg_mean <- rowMeans(jsim_gg)
jsim_gg_quant <- t(apply(X = jsim_gg,MARGIN = 1,FUN = function(x){
  quantile(x,probs=c(0.025,0.975))
}))
jsim_gg_dat <- data.frame(Day=1:1260,low=jsim_gg_quant[,1],high=jsim_gg_quant[,2],mean=jsim_gg_mean)

# ggplot(data = jsim_gg_dat) +
#   geom_line(aes(x=Day,y=mean),color="darkorchid3") +
#   geom_ribbon(aes(x=Day,ymin=low,ymax=high),fill="darkorchid3",alpha=0.25) +
#   theme_bw() +
#   ylab("EIR") +
#   ggtitle("Simulated Jinja Data",subtitle = "(size,prob) parameterization (xi)")

ggplot() +
  geom_line(data = tsim_gg_dat,aes(x=Day,y=mean),color="firebrick3") +
  geom_ribbon(data = tsim_gg_dat,aes(x=Day,ymin=low,ymax=high),fill="firebrick3",alpha=0.2) +
  geom_line(data = ksim_gg_dat,aes(x=Day,y=mean),color="steelblue") +
  geom_ribbon(data = ksim_gg_dat,aes(x=Day,ymin=low,ymax=high),fill="steelblue",alpha=0.2) +
  geom_line(data = jsim_gg_dat,aes(x=Day,y=mean),color="darkorchid3") +
  geom_ribbon(data = jsim_gg_dat,aes(x=Day,ymin=low,ymax=high),fill="darkorchid3",alpha=0.2) +
  theme_bw() +
  ylab("EIR") +
  ggtitle("Simulated PRISM Data",subtitle = "(size,prob) parameterization (xi)") +
  scale_y_continuous(trans = "log1p")


################################################################################
#   a tiny little state space ABM
################################################################################

make_person <- function(site,which){
  
  # my personal biting history
  person <- NULL
  switch(EXPR = site,
         tororo = {
           person <- make_tororo(N = 1,which = which)
         },
         kanungu = {
           person <- make_kanungu(N = 1,which = which)
         },
         jinja = {
           person <- make_jinja(N = 1,which = which)
         },
         {stop("incorrect site")})
  
  # state space
  person$state <- person$state_next <- "S"
  person$time <- person$time_next <- 0
  
  # RM parameters
  person$r <- 1/200
  person$b <- 0.55
  list2env(person,hash = TRUE)
}

sim_person <- function(t){
  while(person$time_next < t){
    
    person$time <- person$time_next
    person$state <- person$state_next
    
    if(person$state == "S"){
      
      n <- tdat_gg$size[[i]]
      p <- tdat_gg$prob[[i]]
      mu <- n*(1-p)/p
      k <- -(mu*p)/(p-1)
      rnbinom(n = 1260,mu = mu,size = k)
      
      
    }
    
    
    
  }
}


pop <- replicate(n = 1e4,expr = make_person(site = "tororo",which = 1),simplify = FALSE)


