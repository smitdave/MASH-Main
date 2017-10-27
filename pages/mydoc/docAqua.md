---
title: Aquatic Ecology Documentation
sidebar: mydoc_sidebar
permalink: docAqua.html
folder: mydoc
toc: true
summary: "The Aquatic Ecology routines govern how mosquitoes emerge from aquatic habitats/laying sites. There are two main modules provided: EL4P and Emerge. EL4P is a detailed model that incorporates mating and oviposition into aquatic habitats and simulates the maturation of egg batches through the larval and pupal stages to emergence. Emerge is a simplified model of mosquito emergence that takes into account seasonality and stochastic variation in productivity of aquatic habitats but does not simulate the larval or pupal stages of mosquito development."
---
## Emerge
Emerge generates mosquito emergence such that the mean emergence from all aquatic habitats on a landscape over one year is equal to LAMBDA. The seasonal forcing of emergence rate is controlled via 'offset' parameter. Lambda is calculated for each aquatic habitat for each day; the mean lambda for a single day across all habitats varies based on seasonality but the mean emergence aggregated over all days of the year is equal to LAMBDA.

### makeAquaPop Routines

#### makeAquaPop.emerge
makeAquaPop.emerge generates the Aquatic Ecology object, given input parameters.

Initialization Parameters:

* N.l: number of aquatic habitats on landscape
* LAMBDA: total number of emerging adult female mosquitoes per human at equilibrium over all aquatic habitats.
* a: shape parameter for gamma distribution of W
* b: scale parameter for gamma distribution of W
* offset: controls seasonality by adjusting rate of emergence over days in year. An offset of 180 corresponds to maximal emergence at the middle of the year; offset is measured in degrees.
* plot: plot lambda over days in year.

makeAquaPop.emerge generates several parameters in the initialization process:

* W: gamma distributed weights applied to Lambda; W introduces heterogeneity in K.
* K: habitat-specific carrying capacity; must sum to Lambda over all habitats (PLoS 2013: Mosquito Population Regulation...)
* season: matrix of lambda values; rows are habitats, columns are days.

Basic seasonal forcing is calculated for each aquatic habitat i as:  $$\lambda _{i}(t)=K_{i}(1+sin(\frac{2\pi t}{365}))$$.
Seasonally forced emergence is calculated such that summing out seasonality the mean daily emergence from all aquatic habitats is equal to input LAMBDA.

{% highlight r %}
makeAquaPop.emerge = function(N.l, LAMBDA, a=1, b=1, offset=0, plot=TRUE){

  season = matrix(0,N.l,365)
  W = rgamma(N.l,a,b)
  K = LAMBDA * W / sum(W)
  if(length(offset)==1){
    offset = rep(offset,N.l)
  }
  for(i in 1:N.l){
    season[i,] = K[i]*(1+sin(2*pi*(c(1:365)-offset[i])/365))
  }

  if(plot){
    plot(1:365,season[1,],type="l",ylim=range(season),xlab="Days",ylab="Per-habitat Emergence")
    for(i in 1:N.l){
      lines(1:365,season[i,])
    }
  }

  return(
    list(
      Nl = N.l,
      K = K,
      Pops = list(
        season = season,
        offset = offset
        # lambda = matrix(N.l,N.types)
      ),
      oneDay = oneDay.emerge,
      PAR = NULL
    )
  )
}
{% endhighlight %}

#### oneDay.emerge
oneDay.emerge handles daily emergence of mosquitoes from all aquatic habitats. The habitat-specific value of lambda for day 't' is used as the mean parameter for Poisson-distributed emergence for all sites. These counts of mosquito emergence are added to the adult emergence queue (ImagoQ) for each habitat.

{% highlight r %}
oneDay.emerge <- function(t, AquaPOP){

  lambda = AquaPOP$Pops$season[,floor(t)%%365+1]
  lambdaEmerge = rpois(length(lambda),lambda)
  for(i in 1:AquaPOP$Nl){
    addAdults2Q(lambda[i], t, i, 0, 0, 0)
  }

}
{% endhighlight %}

## EL4P
EL4P must be fitted (calibrated) with a given set of DHM parameters (generated from Pset(...) and using equilibrium bionomics calculated through Monte Carlo simulation by dhmBasicCohort(...)) to its equilibrium values prior to running the simulation.

The fitting routine is as follows:
{% highlight r %}
#run DHM-Basic and generate basic diagnostic plots
M <- makeM(1)
P <- Pset()
plotDhmPar(P)
cohortOut <- dhmBasicCohort(P,M=5e3)
plotDhmBasicCohort(cohortOut=cohortOut)

#setup AquaticEcology queue objects
ImagoQ <- allocImagoQ(N=100)
EggQ <- allocEggQ(N=100)

#make AquaPop and setup EL4P
Aq.PAR <- makeAq.PAR(N.l=200,cohortOut=cohortOut,R0=5,nHumans=100)
AquaPOP <- makeAquaPop.EL4P(N.l=200,PAR=Aq.PAR)

#fit EL4P to equilibrium values
{% endhighlight %}

### makeAquaPop Routines (Generate AquaPOP and Aq.PAR)

#### makeAq.PAR
makeAq.PAR generates basic parameters of the Aquatic Ecology model, including the equilibrium value of Lambda to sustain a given value of R0 for a given number of human hosts. makeAq.PAR requires input "cohortOut" which is basic bionomics generated from DHM-Basic run on a cohort of mosquitoes under certain parameters of DHM (parameters of mosquito biology).

Aq.PAR:

* M: habitat-specific ratio of adult female mosquitoes to humans (Int Health 2015: Adult vector control, mosquito ecology and malaria transmission)
* G: lifetime egg production, per adult (Int Health 2015: Adult vector control, mosquito ecology and malaria transmission)
* p: UNCERTAIN: used in calculating larval survival (might be density dependent survival from egg to adult)
* P: density independent survival from egg to adult
* a: shape parameter for gamma distribution of W (see makeAquaPop.EL4P)
* b: scale parameter for gamma distribution of W (see makeAquaPop.EL4P)
* Lambda: total number of emerging adult female mosquitoes per human at equilibrium over all aquatic habitats.

{% highlight r %}
makeAq.PAR <- function(N.l,cohortOut,R0,nHumans,...){
  return(list(
    M = rep(1,N.l),
    G = cohortOut$smry$m.eg,
    p = 0.9,
    P = 0.8,
    a = 1,
    b = 1,
    Lambda = getLAMBDA(R0,cohortOut,nHumans,...)
  ))
}
{% endhighlight %}

#### getLAMBDA
getLAMBDA gives a mean value of Lambda over the entire landscape; eg; it is the total number of emerging adult female mosquitoes per human per day required to maintain R0 at equilibrium for a given population of humans based on mean mosquito bionomics that are calculated via Monte Carlo simulation from DHM-Basic.

getLAMBDA requires several input parameters:

* S: mean mosquito lifespan
* r: mean rate of recovery; inverse of mean duration of infectiousness (1/38)
* b: not sure
* P: proportion of a cohort that survives the extrinsic inoculation period \\( e^{-EIP*g} \\); g is the force of mortality and given by inverse of mean lifespan.

{% highlight r %}
getLAMBDA <- function(R0,cohortOut,nHumans,EIP=21){
  S = cohortOut$smry$N
  r = 1/38
  b = 0.55
  P = exp(-EIP / cohortOut$smry$L)
  return(R0*nHumans/P/S^2/b*r)
}
{% endhighlight %}

#### makeAquaPop.EL4P
makeAquaPop.EL4P generates the Aquatic Ecology object, AquaPOP given Aquatic Ecology parameters (Aq.PAR).

AquaPOP generates several parameters in the initialization process:

* W: gamma distributed weights applied to Lambda; W introduces heterogeneity in K.
* K: habitat-specific carrying capacity; must sum to Lambda over all habitats (PLoS 2013: Mosquito Population Regulation...)
* pp:
* alpha: habitat-specific maturation rate of larvae (PLoS 2013: Mosquito Population Regulation...)
* psi: habitat-specific increase in per-capita mortality in response to larval density (PLoS 2013: Mosquito Population Regulation...)

{% highlight r %}
makeAquaPop.EL4P <- function(N.l,PAR){with(PAR,{

  W = rgamma(N.l, a, b)
  K = Lambda*W/sum(W)
  pp = -log(P^((1-p)/5))
  alpha = abs(rnorm(n = N.l,mean = pp,sd = 4e-3))
  psi = alpha/K

  Pops = vector(mode="list",length=N.l)
  for(i in 1:N.l){
    Pops[[i]] = aquapop()
  }

  list(
    Nl = N.l,
    Np = rep(1,N.l),
    Pops = Pops,
    PAR = PAR,
    oneDay = oneDay.EL4P,
    K = K,
    alpha = alpha,
    psi = psi
  )
})}
{% endhighlight %}

### GEL4P Routines (Generate EL4P)

#### fit.EL4P
fit.EL4P requires Aq.PAR and AquaPOP, which are derived by running DHM-Basic Cohort version, which runs each mosquito as a continuous time Markov Process for a given cohort and summarizes their basic bionomics to estimate values needed to calculate lambda over the entire landscape (in Aq.PAR) and generate AquaPOP, which is the object that contains all the aquatic habitats and relevant parameters. fit.EL4P sets the values of psi so that lambda = K at (p,G) for all aquatic habitats.

### checkDX.GEL4P
checkDX.GEL4P is a function that runs the aquatic ecology model until variance in Lambda summed over all aquatic habitats is below a certain threshold.

{% highlight r %}
checkDX.GEL4P <- function(LL, maxT=30){
  M = popLambda(LL)
  llist = sum(M)
  M = 0.9*M + M
  for(i in 1:maxT){
    LL$PAR$M = M
    LL$Pops = oneDay.GEL4P(LL)
    M = 0.9*M + popLambda(LL)
    llist = c(llist, sum(popLambda(LL)))
  }
  return(llist)
}

{% endhighlight %}

### run2Eq.GEL4P
run2Eq.GEL4P is a function that runs the aquatic ecology model to its equilibrium state.

### burnin.GEL4P
burnin.GEL4P is a helper function for run2Eq.GEL4P to run aquatic ecology dynamics for all aquatic habitats for a given amount of time maxT.

{% highlight r %}
burnin.GEL4P <- function(LL,maxT=800){
  for(i in 1:maxT){
    LL$Pops = oneDay.GEL4P(LL)
  }
  return(LL)
}
{% endhighlight %}

### G2K.GEL4P
G2K.GEL4P is a helper function for run2Eq.GEL4P to run aquatic ecology dynamics for all aquatic habitats for a given amount of time maxT while updating M.

### psiFit
psiFit is a helper function to fit.EL4P, which sets values of psi so that lambda = K (set emergence equal to carrying capacity) at (p,G), which are given in Aq.PAR.

* x: parameter (psi) provided to optimize with respect to
* i: index of aquatic habitat
* LL: AquaPOP/EL4P object
* PAR: Aq.PAR object
* maxT: maximum time to allow aquatic ecology to equilibriate
* Mo: default M value of PAR

{% highlight r %}
psiFit <- function(x, i , LL, PAR, maxT, Mo){
  LL$psi[i] = abs(x)
  K = LL$K[i]
  pop = runOne.GEL4P(i,LL,PAR,maxT,Mo)
  return((pop$lambda - K)^2)
}
{% endhighlight %}

### meshK.EL4P

### runOne.GEL4P
runOne.GEL4P runs a single aquatic habitat for maxT days. Mo is the user-specified equilibrium ratio of mosquitoes to humans.

{% highlight r %}
runOne.GEL4P <- function(i, EL4P, PAR, maxT=200, Mo=10){
  PAR$M = rep(Mo,length=length(PAR$M))
  pop = EL4P$Pops[[i]]
  psi = EL4P$psi[i]
  alpha = EL4P$alpha[i]
  ts = NULL
  for(j in 1:maxT){
    pop = oneDay.GEL4Pi(i,pop,psi,alpha,PAR)
    ts = c(ts,pop$lambda) #update lambda history
    PAR$M = 0.9*Mo + pop$lambda #update mosy density
  }
  return(pop)
}
{% endhighlight %}

### oneDay.GEL4Pi
oneDay.GEL4Pi is a helper function for runOne.GEL4P, which advances aquatic ecology in a single habitat by one day. It assumes that eggs are laid in this habitat to correspond to the equilibrium parameters implied by Aq.PAR and K evaluated in meshK.EL4P.

{% highlight r %}
oneDay.GEL4Pi <- function(i,pop,psi,alpha,PAR){
  with(PAR,{
    eggslaid = M[i]*a*G #number of eggs laid at this site

    L1.0=pop$L1; L2.0=pop$L2; L3.0=pop$L3; L4.0=pop$L4
    density = sum(L1.0,L2.0,L3.0,L4.0)

    surv1 = exp(-alpha) #survival probability of pupae
    surv2 = exp(-(alpha + psi*density)) #survival probability of larvae

    pop$lambda = surv1*pop$P #emerging adults
    pop$P = surv2*p*L4.0
    pop$L4= surv2*(p*L3.0 + (1-p)*L4.0)
    pop$L3= surv2*(p*L2.0 + (1-p)*L3.0)
    pop$L2= surv2*(p*L1.0 + (1-p)*L2.0)
    pop$L1 = pop$eggs + surv2*(1-p)*L1.0
    pop$eggs = eggslaid
    pop$tot = sum(pop$L1,pop$L2,pop$L3,pop$L4,pop$P)

    return(pop)
  })
}
{% endhighlight %}

### oneDay.GEL4P
oneDay.GEL4P will run single day population dynamics for all aquatic habitats

{% highlight r %}
oneDay.GEL4P <- function(EL4P,PAR){
  with(c(EL4P,PAR),{
    for(i in 1:Nl){
      L1.0=Pops[[i]]$L1; L2.0=Pops[[i]]$L2; L3.0=Pops[[i]]$L3; L4.0=Pops[[i]]$L4
      density = sum(L1.0,L2.0,L3.0,L4.0)
      surv1 = exp(-alpha[i])
      surv2 = exp(-(alpha[i] + psi[i]*density))
      Pops[[i]]$lambda = surv1*Pops[[i]]$P
      Pops[[i]]$P = surv2*p*L4.0
      Pops[[i]]$L4= surv2*(p*L3.0 + (1-p)*L4.0)
      Pops[[i]]$L3= surv2*(p*L2.0 + (1-p)*L3.0)
      Pops[[i]]$L2= surv2*(p*L1.0 + (1-p)*L2.0)
      Pops[[i]]$L1 = Pops[[i]]$eggs + surv2*(1-p)*L1.0
      Pops[[i]]$eggs = M[i]*a*G
    }
    return(Pops)
  })
}
{% endhighlight %}
