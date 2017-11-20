################ PfLOME Functions OOPified #####################


tentPAR = function(t, pfid){
  tEnd          = Pf.Duration()
  mxPD          = Pf.MaxPD()
  peakD         = Pf.PeakD()
  MZ0           = Pf.MZ0()
  
  gr 		        = (mxPD-MZ0)/peakD
  dr            = mxPD/(tEnd-peakD)
  gtype         = getGtype(pfid)
  
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

add2Pedigree = function(t,pfid){
  PfPedigree[[pfid]] <<- newPfPed(pfid)
  if(pfid > 1){
    PfPedigree[[pfid]]$mac <<- getParent(ixH)
    PfPedigree[[pfid]]$mic <<- getParent(ixH)
    PfPedigree[[pfid]]$th <<- t
    PfPedigree[[pfid]]$thEnd <<- HUMANS[[ixH]]$Pf$clones[[j]]$tEnd+t
    PfPedigree[[pfid]]$ixH <<- ixH
    if(PfPedigree[[pfid]]$mic != PfPedigree[[pfid]]$mac) {
      PfPedigree[[pfid]]$sib = sample(1:2,2)
    }
  }
  PfPedigree[[pfid]]$gtype <<- getGtype(pfid)
  PfPedigree[[pfid]]$ptype <<- getPtype(pfid,PfPedigree[[pfid]]$gtype,nptypes)
}

getParent = function(){
  ## assigned inside mosquito object
  return(sample(1:pfid,1))
}

newPfPedigree = function(
  pfid,              # Unique identifier for clonal infection
  gtype=0,           # genotype (at each locus; vector)
  ptype=0,           # phenotype (associated with each locus; vector)
  mac=0,mic=0,       # Unique identifier of the parents
  sib=0,             # 11, 12, 21, 22 (from same ookinete)
  mloc=0,tm=0,       # Time & place mosquito was infected
  ixH2M=0,           # Identity of the transmitting human
  hloc=0,th=0,       # Time & place human was infected
  thEnd=0,           # End of infection in human
  ixH=1,             # Who was infected
  nSPZ=0
){
  list(
    pfid    = pfid,
    gtype   = gtype,
    ptype   = ptype,
    mac     = mac,
    mic     = mic,
    sib     = sib,
    mloc    = mloc,
    tm      = tm,
    ixH2M   = ixH2M,
    hloc    = hloc,
    th      = th,
    thEnd   = thEnd,
    ixH     = ixH,
    nSPZ    = nSPZ
  )
}

## calculate moi at each step by going through all the active Pfs inside human
moiCounter = function(){
  moi = 0
  for(i in 1:length(pathogen$PfPathogen)){
    if(pathogen$PfPathogen$activeP > 0){
      moi = moi+1
    }
  }
}