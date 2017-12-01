
##################################################
#  The tent function 
##################################################

Pf.gr = function(N=1, mn=10, vr=1){
  log10(rnorm(N,mn,vr))/2
}

Pf.MZ0 = function(){
  #FIX STUB 
  rnorm(1,4.2,.1)                         
} 

Pf.PeakD = function(){
  #FIX STUB 
  # Day when parasitemia first peaks
  ceiling(9+rlnorm(1,log(3),.5))
}

Pf.Duration = function(N=1,mn=200){
  #FIX STUB 
  # Time to last parasitemia
  rgeom(N,1/mn) 
} 

Pf.MaxPD = function(N=1, mn=10.5, vr=0.5){rnorm(N,mn,vr)} 

tentPAR = function(t, thresh = 3){ 
  lastDetect    = Pf.Duration()
  mxPD          = Pf.MaxPD() 
  peakD         = Pf.PeakD() 
  dr            = (mxPD - thresh)/lastDetect
  tEnd          = peakD + lastDetect + thresh/dr
  
  list(
    t0  	  = t, 
    PfSeed        = .Random.seed, 
    gr            = Pf.gr(),
    MZ0           = Pf.MZ0(),
    peakD         = peakD,
    mxPD          = mxPD, 
    dr            = dr,
    tEnd          = tEnd 
  )
} 

##################################################
#  The function describing an infection without 
#  pre-existing immunity.  
##################################################

Pt_tent = function(t, PAR, Imm){with(c(PAR,Imm),{
  PfAge = t-t0+1
  PP = -1

  if(PfAge>0 & PfAge <= tEnd){
    PP = min(
       MZ0 + (gr*im.gr)*PfAge,
       mxPD*im.mx,
       mxPD*(1-(PfAge-peakD)/(tEnd-peakD)*im.end)
    )
  }
  PP = PP*im.tot
  ifelse(PP>0, PP, NaN)
})}



Imm0 = list(im.gr=1, im.mx=1, im.tot=1,im.end=1)
Imm1 = list(im.gr=.5, im.mx=.9, im.tot=.9, im.end = 1.1)

PAR = tentPAR(0)
tt = 1:(PAR$tEnd+40)
tent0=unlist(sapply(tt, Pt_tent, PAR=PAR, Imm=Imm0)) 
tent1=unlist(sapply(tt, Pt_tent, PAR=PAR, Imm=Imm1)) 
plot(tt, tent0, type = "l")
lines(tt, tent1)

makeClone_tent = function(t, pfid, ixH, nmz=0){ 
  PAR = tent_PAR(t)
 
  ImmState = getImmuneState(t, ixH)  
  # Parasite Densities :: infected RBCs
  Pt = makePt_tent(gr, MZ0, peakD, mxPD, lastDetect)
  if(PAR$red == TRUE){ 
    Pt = pmin(pmax(0,Pt*bumps1(length(Pt))), 13) 
    Pt = redSmooth(Pt,gr) 
  } 

  MZ2GZ = 3
  Gt = Pt2Gt_simple(Pt, MZ2GZ)

  ixG = max(which(!is.na(Gt))) 
  ixP = max(which(!is.na(Pt))) 

  tEnd = max(ixG, ixP) 
  Pt = Pt[1:tEnd] 
  Gt = Gt[1:tEnd] 

  list(
    id    = PfID,                     
    t0    = t,                       
    tEnd  = tEnd,
    Pt    = Pt,
    Gt    = Gt,
    PAR   = list(gr=gr,MZ0=MZ0,peakD=peakD,mxPD=mxPD)
  )
}


redSmooth = function(Pt,gr){
  ix=which(diff(Pt)>gr)
  i=1
  while(length(ix)>0){
    i = i+1 
    if (i > 10) browser() 
    Pt[ix+1] = Pt[ix] + gr - 10^-6
    ix=which(diff(Pt)>gr)
  } 
Pt} 
 
cumGam = function(iG1){
 GG = rep(NaN, 10)
 gg = NaN
 for(i in c(1:length(iG1))){
   gg = addPosLogs(c(gg-0.45,iG1[i]))
   GG = c(GG, gg)
 }
 if(!is.na(gg)){
   while(gg >0){
     gg = gg-0.45
     GG = c(GG, gg)  
   }
 } 
 GG
} 

Pt2Gt_simple = function(Pt, MZ2GZ){
  Gt = c(rep(NaN, 15), Pt-MZ2GZ) 
  Gt[which(Gt < 0)] = NaN
  Gt
}




makePt_tent = function(gr=1, MZ0=4.2, peakD=38, mxPD=10.5, tEnd=220){
  #FIX STUB 
  dr = mxPD/tEnd
  P1 = rep(NaN,6) 
  P2 = pmin(MZ0 + gr*c(1:(peakD-6)), mxPD)
  P3 = max(P2) - dr*c(1:tEnd)
  c(P1,P2,P3)
}

makeGt.tent = function(Pt){
   Gt = Pt-2 
   Gt[which(Gt <0)] = NaN 
   c(rep(-1,10), Gt)
}

PfID = 0
