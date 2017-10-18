

cloneAge = function(t, clone){
  t-clone$t0+1
}

##################################################
#  The tent function parameters
##################################################

Pf.MZ0 = function(){
  #FIX STUB
  rnorm(1,4.2,.1)
}

Pf.PeakD = function(){
  #FIX STUB
  # Day when parasitemia first peaks
  ceiling(18+rlnorm(1,log(3),.5))
}

Pf.Duration = function(N=1,mn=200){
  #FIX STUB
  # Time to last parasitemia
  rgeom(N,1/mn)
}

Pf.MaxPD = function(N=1, mn=10.5, vr=0.5){rnorm(N,mn,vr)}

##################################################
#  The clonal object for the tent function
##################################################

tentPAR = function(t, pfid){
  tEnd          = Pf.Duration()
  mxPD          = Pf.MaxPD()
  peakD         = Pf.PeakD()
  MZ0           = Pf.MZ0()

  gr 		        = (mxPD-MZ0)/peakD
  dr            = mxPD/(tEnd-peakD)
  gtype         = PfPedigree[[pfid]]$gtype

  list(
    pfid	        = pfid,
    t0  	        = t,
    gr            = gr,
    dr            = dr,
    MZ0           = MZ0,
    peakD         = peakD,
    mxPD          = mxPD,
    tEnd          = tEnd,
    gtype         = gtype
  )
}

getGtype = function(pfid=1){
  if(pfid == 1){
    gtype = runif(nAntigenLoci)
    return(gtype)
  } else{
    mic =  PfPedigree[[pfid]]$mic
    micType =  PfPedigree[[mic]]$gtype
    mac =  PfPedigree[[pfid]]$mac
    macType =  PfPedigree[[mac]]$gtype
    micmac = cbind(micType, macType)
    ix = sample(c(1,2), nAntigenLoci, replace =TRUE)
    gtype = NULL
    for(i in 1:nAntigenLoci){
      gtype = c(gtype,micmac[i,ix[i]])
    }
    gtype = mutate(gtype,mu)
    return(gtype)
  }
}

getPtype = function(pfid=1,gtype,nptypes){
  ptype = ceiling(nptypes*gtype)
  return(ptype)
}

mutate = function(gtype,mu){
  #mu is parameter describing mutation probability at each locus - between 0 and 1
  ##assuming julia gog forulation with genotypes lying on unit interval:
  mutgen = which(rbinom(nAntigenLoci,1,mu)==1) #set mutation rate at 10 percent for testing
  if(length(mutgen)!=0) {
    for(i in 1:length(mutgen)) {
      gtype[mutgen[i]] = gtype[mutgen[i]] + runif(1,min=-gtype[mutgen[i]],max=1-gtype[mutgen[i]]) #uniformly random mutation
      }
    }
  return(gtype)
}


##################################################
#  The function describing an infection without
#  pre-existing immunity.
##################################################

gr_tent = function(t, PAR){with(PAR,{
  ifelse(t<peakD, gr, -dr)
})}

dPdt_tent = function(t, P, PAR, PD=0, IM=0){with(PAR,{
  if(NOISY == TRUE) browser()
  age = ifelse(t>=t0, t-t0+1, 0)
  P = ifelse(age>=1 & age<=tEnd,
      pmin(mxPD, P + gr_tent(age,PAR))-PD-IM,
      NaN)
  ifelse(!is.na(P)&P>0, P, NaN)
})}

PDi = function(t, i, Rx){with(Rx,{
  age = t-StartTreatment[i]+1
  PD = 0*age
  ii = as.numeric(Rx$Drug[i])
  ix = which(age>=1 &  age<=RxRegister[[ii]]$Duration)
  if(length(ix)>0) PD[ix] = RxRegister[[ii]]$PfPD[age[ix]]
  return(PD)
})}

GamCyGen = function(t, P, PAR){
  P-2
}

getPD = function(t, Rx){
  N = length(Rx$StartTreatment)
  PD = t*0
  for(i in 1:N){
    PD = PD + PDi(t,i,Rx)
  }
  return(PD)
}

#PD=getPD(20:80, Rx)

Pt_tent = function(PAR, Rx=NULL, plotIt=0, clr="black"){with(PAR,{
  tt = t0 + 1:tEnd
  P = Pt = PAR$MZ0
  PD = getPD(tt,Rx)
  for(t in tt)
  {
    P = dPdt_tent(t, P, PAR, PD[t-t0+1], IM=0)
    if(!is.na(P) & P<0){PAR$tEnd = t}
    Pt = c(Pt, P)
  }
  tt = t0+0:tEnd
  ix = which(Pt>0)
  tt = tt[ix]
  Pt = Pt[ix]
  if(plotIt == 1){
    plot(tt, Pt, type = "l", xlab = "Age of Infection", ylab = expression(log[10](P), ylim = c(0, 20)), col=clr)
    segments(0,6,tEnd,6, lty=2, col = clr)
  }
  if(plotIt == 2){
    lines(tt/365, Pt, col = clr)
  }
  return(list(t=tt,Pt=Pt))
})}

#NOISY = FALSE
#Rx$StartTreatment = c(20, 120)
#Rx$Drug = list(1,2)
#a <- PDi(10, 1, Rx)
#a<-Pt_tent(tentPAR(100), Rx, plotIt=1, clr="red")

##################################################
#  Translating Immune State into Effects on the
#  Time Course of an Infection
##################################################

peakDMod = function(maxInc, Imm, xh, b){
  peakD = 1 - sigmoid01(Imm,xh,b,maxInc)
  return(peakD)
}

mxPDMod = function(maxDec, Imm, xh, b){
  mxPD = 1 - sigmoid01(Imm,xh,b,maxDec)
  return(mxPD)
}

tEndMod = function(maxInc, Imm, xh, b){
  tEnd = 1 - sigmoid01(Imm,xh,b,maxInc)
  return(tEnd)
}

gdkMod = function(maxDec, Imm, xh, b){
  gdk = 1 + sigmoid01(Imm,xh,b,maxDec)
  return(gdk)
}

typeSpecificImmuneModulation = function(t, ixH, clone,dxp,dtp){with(HUMANS[[ixH]]$Pf,{
  # clone$gtype is a list of real numbers of length nAntigenLoci, e.g. x
  # Type immunity is a paired list of real numbers and times, e.g.  x', t'
  # modify peakD, mxPD, tEnd based on abs(x-x') and (t-t')
  # a,b are respectively constants that dictate the decay of immunity in strain space and in time
  pfptype = ptype2Mat(PfPedigree[[pfid]]$ptype,nAntigenLoci,nptypes)
  ptypes = HUMANS[[ixH]]$Pf$History$ptypes
  ptypesTime = HUMANS[[ixH]]$Pf$ptypesTime
  crossImm = crossImmunity(ptypes,ptypesTime,nAntigenLoci,t,dxp,dtp)
  crossImmHalf = 100 # halfway point of cross immunity in sigmoid response
  crossImmSlope = 1 # steepness of sigmoid response
  crossImmSat = 200 # saturation point of sigmoid response
  typeImm = sigmoidX(crossImm,crossImmHalf,crossImmSlope,crossImmSat) ######want to change these params - number of recent exposures vs immune saturation
  HUMANS[[ixH]]$Pf$TypeImmunity <<- typeImm
  immMod = pmax(pfptype-typeImm,0)
  imm = 1-sum(immMod)/nAntigenLoci
  HUMANS[[ixH]]$Pf$SpecImm <<- imm
  peakDxh = .5 ##all params for sigmoid functions
  peakDb = 3
  mxPDxh = .5
  mxPDb = 3
  tEndxh = .5
  tEndb = 3
  clone$peakD <<- clone$peakD*sigmoid01(imm,peakDxh,-peakDb,1)
  clone$mxPD <<- (clone$mxPD-clone$MZ0)*sigmoid01(imm,mxPDxh,-mxPDb,1)+clone$MZ0
  clone$tEnd <<- (clone$tEnd-clone$peakD)*sigmoid01(imm,tEndxh,-tEndb,1)+clone$peakD
  clone$gr    <<- (clone$mxPD-clone$MZ0)/clone$peakD
  clone$dr    <<- clone$mxPD/(clone$tEnd-clone$peakD)
  return(clone)
})}

immuneModulation_tent = function(t, ixH, clone){with(HUMANS[[ixH]]$Pf,{
  # Reduce peak parasite densities
  # Delay the peak
  # Shorten the infection
  for(i in 1:10){
    if(length(BSImm[i]) > 0 & is.numeric(BSImm[i])){
      clone$peakD <<- clone$peakD*(peakDMod(1,BSImm[i],.5,3)+1)/2
      clone$mxPD  <<- (clone$mxPD-clone$MZ0)*(mxPDMod(1,BSImm[i],.5,3)+1)/2+clone$MZ0
      clone$tEnd  <<- (clone$tEnd-clone$peakD)*(tEndMod(1,BSImm[i],.5,3)+1)/2+clone$peakD
      clone$gr    <<- (clone$mxPD-clone$MZ0)/clone$peakD
      clone$dr    <<- clone$mxPD/(clone$tEnd-clone$peakD)
    }
  }
  clone = typeSpecificImmuneModulation(t, ixH, clone,dxp,dtp)
  for(i in 1:10){
    if(length(GSImm[i]) > 0 & is.numeric(GSImm[i])){
    clone$gdk <<- clone$gdk*gdkMod(1,GSImm[i],.5,3)
    }
  }
  return(clone)
})}

##################################################
#  The function Describing Onset of Fever
##################################################

queueFever_tent = function(t, ixH, PAR){with(HUMANS[[ixH]],{
  HUMANS[[ixH]]$Fever$tStart <<- t
  HUMANS[[ixH]]$Fever$tEnd <<- t+rnorm(1,4,1)
})}

##################################################
# makeClone
##################################################

makeClone_tent = function(t, pfid, ixH, nmz=0){
  PAR = tentPAR(t,pfid)
  PAR = immuneModulation(t, ixH, PAR)
  return(PAR)
}

lightMicroscopy_tent = function(t, ixH, parasite){with(HUMANS[[ixH]]$Pf,{
  if(parasite == "Asexual"){
    fp = .01
    fn = .01
    detect = log10(25*10^6*5)
    P = ifelse(is.na(Ptt),0,Ptt)
    steep = 3
    prob = fp + sigmoidX(P,detect,steep)
    result = rbinom(1,1,prob)
    }
  if(parasite == "Gametocyte"){
    fp = .01
    fn = .01
    detect = log10(25*10^6*5)
    G = ifelse(is.na(Gt),0,Gt)
    steep = 3
    prob = fp + sigmoidX(G,detect,steep)
    result = rbinom(1,1,prob)
    return(result)
  }
})}

RDT_tent = function(t, ixH, sensitive, biomarker){with(HUMANS[[ixH]]$Pf,{
  if(biomarker == "pLDH"){
    L = ifelse(is.na(HUMANS[[ixH]]$pLDH,0,10^HUMANS[[ixH]]$pLDH))
    fp = .01
    fn = .01
    steep = 3
    if(sensitive == T){
      detect = log10(11*1000*5)
      prob = fp + sigmoidX(L,detect,steep)
      result = rbinom(1,1,prob)
      return(result)
    }
    if(sensitive == F){
      detect = log10(11*1000*5*10)
      prob = fp + sigmoidX(L,detect,steep)
      result = rbinom(1,1,prob)
      return(result)
    }
  }
  if(biomarker == "HRP2"){
    H = ifelse(is.na(HUMANS[[ixH]]$HRP2,0,10^HUMANS[[ixH]]$HRP2))
    fp = .01
    fn = .01
    steep = 3
    if(sensitive == T){
      detect = log10(11*1000*5)
      prob = fp + sigmoidX(H,detect,steep)
      result = rbinom(1,1,prob)
      return(result)
    }
    if(sensitive == F){
      detect = log10(11*1000*5*10)
      prob = fp + sigmoidX(H,detect,steep,1-fp-fn)
      result = rbinom(1,1,prob)
      return(result)
    }
  }
})}

LAMP_tent = function(t, ixH){with(HUMANS[[ixH]]$Pf,{
    P = ifelse(is.na(Ptt),0,Ptt)
    G = ifelse(is.na(Gt),0,Gt)
    PG = log10(10^P+10^G)
    fp = .01
    fn = .01
    steep = 3
    detect = log10(11*1000*5*10)
    prob = fp + sigmoidX(PG,detect,steep)
    print(prob)
    result = rbinom(1,1,prob)
    return(result)
})}

sigmoid01 = function(x,xh,b,max) {
  #xh is 50th percentile in (0,1)
  #b is slope param - b in R\{0}
  a = tan(pi/2*xh)^b
  max/(a/tan(pi/2*x)^b+1)
}

weight = function(dx,dt,dxp,dtp){ ##weight of cross immunity with dx difference in phenotype, dt difference in time since last exposure
  exp(-(dxp*dx+dtp*dt))
}
