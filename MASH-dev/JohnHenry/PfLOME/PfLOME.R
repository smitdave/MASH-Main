
#########################################################
#  Lifetime Ontogeny of Malaria Epidemiology : LOME
#########################################################

########################################
#  LOME :: Pedigree
########################################

newPfPed = function(
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

PfPedigree = list()

add2Pedigree_PfLOME = function(pfid,ixH,t,j){
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

mPf = c(
  tm=0, mloc=0,   # Time and place where mosquito was infected
  ixH2M   = 0,        # The infecting human
  NOok  = 0,          # Number of ookinetes
  mac  = 1,           # PfIDs of mac (NOok)
  mic  = 1            # PfIDs of mic (NOok)
)

##############################################################
#   LOME: Exposure to Bloodstream Infection
#
#   This group of functions sets up parasite infections
#   in the HUMAN$eventQ
#
#   NOTE: M-BITES calls probeHost(ixH, ixM,  t, x, y, mPf)
##############################################################

probeHost_0 = function(ixH, ixM, t, x, y,  mPf){
  for(n in 1:mPf$n){
    if(mPf[[n]]$spz >0){
      SPZ2MZ(ixH, t,x,y,mPf)
    }
  }
}

probeHost_PfLOME = function(ixH, ixM, t, spz, x, y,  mPf){
  countHBR(ixH)   #OPTION
  for(n in 1:mPf$n){
    if(mPf[[n]]$spz >0){
      countEIR(ixH) #OPTION
      SPZ2MZ(ixH, t,x,y,mPf)
    }
  }
}

SPZ2MZ_0 = function(ixH, t, x, y, mPf){
  nSPZ = M2HnSPZ()
  queueClone_PfLOME(t,x,y,ixH,mPf,nSPZ)
}

SPZ2MZ_notypes = function(ixH, t, x, y, mPf){
  # From Dermis to Liver
  nSPZ = M2HnSPZ(ixH)
  if(nSPZ>0) nSPZ = spzImmunNS(nSPZ, ixH)
  if(nSPZ>0) nSPZ = livImmunNS(nSPZ,ixH)
  if(nSPZ>0){
    countFOI(1, ixH)
    queueClone_PfLOME(t,x,y,ixH,mPf,nSPZ)
  }
}

SPZ2MZ_types = function(ixH, t, x, y, mPf){
  # From Dermis to Liver
  nSPZ = M2HnSPZ(ixH) #OPTION
  nSPZ = spzImmunNS(nSPZ, ixH)
  clones = getSpzClones_PfLOME(nSPZ, mPf)
  clones = spzImmunTP(clones, ixH)
  # In the Liver
  countLVR(sum(clones$nSPZ), ixH)
  clones$nSPZ = livImmunNS(clones$nSPZ,ixH)
  clones = livImmunTP(clones, ixH)
  countFOI(length(clones$mac), ixH)
  queueClones_PfLOME(t,x,y,ixH,mPf,clones)
}

#---------------------------------
# Number of sporozoites
#---------------------------------
M2HnSPZ_0 = function(){
 rnbinom(1,.16,.01) #Make this modular
}

#------------------------
# Exposure Event counters
#------------------------

countHBR_0 = function(ixH){}

countHBR_1 = function(ixH){
  HUMANS[[ixH]]$Pf$xpos$HBR <<- HUMANS[[ixH]]$Pf$HBR + 1
}

countEIR_0 = function(ixH){}

countEIR_1 = function(ixH){
  HUMANS[[ixH]]$Pf$xpos$EIR <<- HUMANS[[ixH]]$Pf$EIR + 1
}

countLVR_0 = function(N, ixH){
}

countLVR_1 = function(N, ixH){
  HUMANS[[ixH]]$Pf$xpos$LVR <<- HUMANS[[ixH]]$Pf$LVR + N
}

countFOI_0 = function(mFOI, ixH){
  HUMANS[[ixH]]$Pf$xpos$FOI <<- HUMANS[[ixH]]$Pf$FOI + 1
  HUMANS[[ixH]]$Pf$xpos$mFOI <<- HUMANS[[ixH]]$Pf$mFOI + mFOI
}

countFOI_1 = function(mFOI, ixH){
  HUMANS[[ixH]]$Pf$xpos$FOI <<- HUMANS[[ixH]]$Pf$FOI + 1
  HUMANS[[ixH]]$Pf$xpos$mFOI <<- HUMANS[[ixH]]$Pf$mFOI + mFOI
}

#--------------------------------------
# Non-specific anti-sporozoite immunity
#--------------------------------------
spzImmunNS_LOME = function(nSPZ, ixH)
{
 rbinom(1,nSPZ,HUMANS[[ixH]]$Pf$PE$bSPZ)
}

spzImmunNS_NULL = function(nSPZ, ixH){nSPZ}

#--------------------------------------
# Non-specific anti-liver-stage immunity
#--------------------------------------
livImmunNS_LOME = function(nSPZ, ixH)
{
 rbinom(length(nSPZ),nSPZ,HUMANS[[ixH]]$Pf$PE$bLIV)
}

livImmunNS_NULL = function(nSPZ, ixH){nSPZ}

#--------------------------------------
# Type-specific anti-sporozoite immunity
#--------------------------------------
spzImmunTP_NULL = function(clones, ixH){clones}

#--------------------------------------
# Type-specific anti-liver-stage immunity
#--------------------------------------
livImmunTP_NULL = function(clones, ixH){clones}

#------------------------------------------------
# Sporozoite
#------------------------------------------------

clonalRecombinants = function(j, mac, mic){
 if(mac[j]==mic[j]){
   #Selfing
   c(mac=mac[j],mic=mic[j],sib=0,wt=4)
  } else {
   #Outcrossing
   c(mac=mac[j],mic=mic[j],sib=1:4,wt=1)
  }
}

getSpzClones_PfLOME = function(nSPZ, mPf){
  # make a data frame of the variants
  with(mPf,{
    PfTypes = clonalRecombinants(1, mac, mic) #OPTION??
    for(i in 2:NOok) PfTypes = rbind(PfTypes, clonalRecombinants(i, mac, mic))
  })
  PfTypes = data.frame(PfTypes)
  PfTypes$nSPZ = rmultinom(1,nSPZ,PfTypes$wt)
  PfTypes[which(PfTypes$nSPZ>0),-4]
}

########################################################################################
#  HUMAN eventQ functions
########################################################################################

queueClone_PfLOME  = function(t,x,y,ixH,mPf,nSPZ){
  pfid <<- pfid+1
  gtype = getGtype(pfid)
  with(mPf,{
    add2Q_PfLOME0(ixH, t, PAR = c(pfid=pfid,nMZ=nSPZ))
})}

queueClones_PfLOME  = function(t,x,y,ixH,mPf,clones){
  with(as.list(c(clones, mPf)),{
    for(i in 1:length(mac)){
      pfid <<- pfid+1
      add2Q_PfLOME0(ixH, t, PAR = c(pfid=pfid, nMZ=nSPZ[i]))
    }
})}

add2Q_PfLOME0 = function(ixH, t, PAR){
  #NOTE: the event is queued to occur when parasites emerge
  #      after 6 days in the liver
  addEvent2Q(ixH, event_PfLOME0(t+7, PAR, tag = "PfLOME0"))
}

event_PfLOME0 = function(t, PAR){list(t=t, PAR=PAR, F=infectHuman_PfLOME)}

########################################################################################
#  This sets up infection by the clone PfID when called by the eventQ
########################################################################################

infectHuman_PfLOME = function(t, ixH, PAR){
  clone = makeClone(t, PAR$pfid, ixH, PAR$nMZ)
  # Update immunity here?
  #clone = immuneModulation_tent(t, ixH, clone)
  addClone_PfLOME(t, ixH, clone)
}


###############################################################
#   Manage Clones
###############################################################

addClone_PfLOME = function(t, ixH, clone){with(HUMANS[[ixH]]$Pf,{

  j=which(activeG<=0)
  j=ifelse(length(j)>0, min(j), alloc_clones+1)

  if(j > alloc_clones){
    HUMANS[[ixH]]$Pf$alloc_clones <<- HUMANS[[ixH]]$Pf$alloc_clones+1
    HUMANS[[ixH]]$Pf$Ptt <<- cbind(HUMANS[[ixH]]$Pf$Ptt,0)
  }

  HUMANS[[ixH]]$Pf$MOI <<- HUMANS[[ixH]]$Pf$MOI+1
  HUMANS[[ixH]]$Pf$clones[[j]] <<- immuneModulation_tent(t,ixH,clone)
  pfid <<- pfid+1 #testing pfid ################################################################################## will want to remove or replace these lines
  add2Pedigree_PfLOME(pfid,ixH,t,j) ################################################################################## if we keep the update at queuing instead
  HUMANS[[ixH]]$Pf$activeP[j] <<- 1
  HUMANS[[ixH]]$Pf$activeG[j] <<- 1
  HUMANS[[ixH]]$Pf$Pt[j] <<- clone$MZ0
  HUMANS[[ixH]]$Pf$Gt[j] <<- NaN
  HUMANS[[ixH]]$Pf$ptypes <<- matrix(0,nAntigenLoci,max(nptypes))
  tt = t%%10+1
  HUMANS[[ixH]]$Pf$Ptt[tt,j] <<- clone$MZ0
})}

removePfClone = function(ixH, ix){with(HUMANS[[ixH]]$Pf,{
    HUMANS[[ixH]]$Pf$MOI <<- MOI-1
    HUMANS[[ixH]]$Pf$activeP[ix] <<- 0
    HUMANS[[ixH]]$Pf$activeG[ix] <<- 0
})}



###############################################################
#   These functions update the
###############################################################


getPt=function(t, ixH){with(HUMANS[[ixH]]$Pf,{
  MOI = sum(activeP>0)
  ix = which(activeP>0) # what clones are active
  tt = t%%10+1 # gametocytes need about 10 days before infectious to mosquitoes
  HUMANS[[ixH]]$Pf$Ptt[tt,] <<- NaN
  for(i in ix){
    PD = getPD(t,Rx) # get pharmacodynamic effects (call RxRegistry to look up kiling effect of that drug that day)
    HUMANS[[ixH]]$Pf$Pt[i] <<- dPdt(t, Pt[i], clones[[i]], PD) # update parasite densities at time t
    if(is.na(HUMANS[[ixH]]$Pf$Pt[i])){
      HUMANS[[ixH]]$Pf$clones[[i]]$tEnd <<- t-HUMANS[[ixH]]$Pf$clones[[i]]$t0 # negative dead is really dead.
    }
    HUMANS[[ixH]]$Pf$Ptt[tt,i] <<-  HUMANS[[ixH]]$Pf$Pt[i] # record parasites in rolling list of time t (not strictly necessary)
  }
  HUMANS[[ixH]]$Pf$Ptot <<- log10sum(Pt) # total asexual stage densities
})}

getGt=function(t, ixH){with(HUMANS[[ixH]]$Pf,{
  tt = (t+1)%%10+1
  ix = which(activeG>0)
  for(i in ix){
    Gt[i] = log10sum(c(Gt[i] +gdk, GamCyGen(t, Ptt[tt,i], clones[[i]]))) # (on day t, i have this many parasite densities created tt days ago)
    HUMANS[[ixH]]$Pf$Gt[i] <<- Gt[i]
  }
  HUMANS[[ixH]]$Pf$Gtot <<- log10sum(Gt[ix]) # total gametocyte densities
})}

updateInfection = function(ixH, t){
  ix = which(HUMANS[[ixH]]$Pf$activeP>0)
  for(i in ix){
    if(HUMANS[[ixH]]$Pf$clones[[i]]$t0+HUMANS[[ixH]]$Pf$clones[[i]]$tEnd<t){
      HUMANS[[ixH]]$Pf$activeP[i] <<- 0
      HUMANS[[ixH]]$Pf$MOI <<- HUMANS[[ixH]]$Pf$MOI-1
    }
  }
  getPt(t, ixH)
  ix = which(HUMANS[[ixH]]$Pf$activeG>0)
  for(i in ix){
    if(HUMANS[[ixH]]$Pf$clones[[i]]$t0+HUMANS[[ixH]]$Pf$clones[[i]]$tEnd<(t+10) & is.na(HUMANS[[ixH]]$Pf$Gt[i]))
     HUMANS[[ixH]]$Pf$activeG[i] <<- 0
  }
  getGt(t, ixH)
}

updateImmunity = function(ixH,t){with(HUMANS[[ixH]]$Pf,{ # update human immunity (counters have an equilibrium value for a counter)
  for(i in 1:nBSImmCounters)
    HUMANS[[ixH]]$Pf$BSImm[i] <<- with(BSImmCounters[[i]], F(BSImm[i], Ptot, PAR)) # bloodstage immune counters
  for(i in 1:nGSImmCounters)
    HUMANS[[ixH]]$Pf$GSImm[i] <<- with(GSImmCounters[[i]], F(GSImm[i], Ptot, PAR)) # gametocyte stage immune counters
})}

updateTypeImmunity = function(ixH,t){with(HUMANS[[ixH]]$Pf, { # parasites have genotype history
  HUMANS[[ixH]]$Pf$ptypes <<- matrix(0,nAntigenLoci,max(nptypes))
  HUMANS[[ixH]]$Pf$ptypesTime <<- matrix(0,nAntigenLoci,max(nptypes))
  for(j in 1:length(PfPedigree)){
    if(PfPedigree[[j]]$th > t-1 & PfPedigree[[j]]$th <= t & PfPedigree[[j]]$ixH == ixH){
      for(i in 1:nAntigenLoci){
        HUMANS[[ixH]]$Pf$ptypes[i,PfPedigree[[j]]$ptype[i]] <<- HUMANS[[ixH]]$Pf$ptypes[i,PfPedigree[[j]]$ptype[i]]+1
        HUMANS[[ixH]]$Pf$ptypesTime[i,PfPedigree[[j]]$ptype[i]] <<- t
      }
    }
  }
})}

################################################
#  Update Fever
################################################

updatePfFeverThreshold = function(ixH,t){with(HUMANS[[ixH]],{
  #################### tab - can modulate with immune counters, NSAIDs, etc.
  feverThresh = 7
  return(feverThresh)
})}

updatePfFever = function(ixH,t){with(HUMANS[[ixH]],{
  feverThresh = updatePfFeverThreshold(ixH,t)
  Ptot = ifelse(is.na(HUMANS[[ixH]]$Pf$Ptot),0,HUMANS[[ixH]]$Pf$Ptot)
  if(Ptot >= feverThresh){queueFever_tent(t,ixH, PAR)}
})}


################################################
#  Update RBC and Anemia
################################################

updateRBC = function(ixH,t){with(HUMANS[[ixH]]$Pf,{
  a = log(2)/120 #RBC halflife
  b = 1
  c = 1.7
  d = .5
  e = 5*10^9
  rhat <<- ifelse(t<7,2.5,HUMANS[[ixH]]$History$RBC[t-6])
  r <<- HUMANS[[ixH]]$RBC
  HUMANS[[ixH]]$RBC <<- ifelse(is.nan(Ptot),
                      r - a*r + b*exp(-c*rhat),
                      r - a*r + b*exp(-c*rhat) - d*10^Ptot/(e+10^Ptot)*r)
})}

updateAnemia = function(ixH,t){with(HUMANS[[ixH]]$Pf,{
  HUMANS[[ixH]]$Anemia <<- ifelse(HUMANS[[ixH]]$RBC > 2.5/3, 0, 1)
})}

updateHRP2 = function(ixH,t){with(HUMANS[[ixH]]$Pf,{
  a = .0019
  b = log(2)/3.67
  HUMANS[[ixH]]$HRP2 <<- ifelse(is.nan(Ptot),log10(10^HUMANS[[ixH]]$HRP2-b*10^HUMANS[[ixH]]$HRP2),log10(10^HUMANS[[ixH]]$HRP2+a*10^Ptot-b*10^HUMANS[[ixH]]$HRP2))
})}

################################################
#  Infect Mosquito
################################################

infectMosquito = function(ixH,t){with(HUMANS[[ixH]]$Pf,{
  nOok = getNook(Gtot, Gt, GsImm)
  mac = replicate(nOok, getParent(ixH))
  mic = replicate(nOok, getParent(ixH))
  c(tm=t,mloc=-1,ixH=ixH,NOok=nOok,mac=mac,mic=mic)
})}

getParent = function(ixH){with(HUMANS[[ixH]]$Pf,{
  ## possibly include some sort of multiplier in the weight for rmultinom
  G = 10^HUMANS[[ixH]]$Pf$Gt
  G[is.na(G)] = 0
  ix  = ifelse(sum(abs(G))==0, 1 ,which(rmultinom(1,1,G)==1))
  return(HUMANS[[ixH]]$Pf$clones[[ix]]$pfid)
})}

gsES = function(GsImm,gsh=.5,gsb=3){
  # John Henry will fix this with better functions
  ## sigmoid is better, can fix saturation levels and effect sizes wrt immune counters based on data; gsh is 50th percentile of effects, gsb is steepness param
  sigmoid01(GsImm,gsh,gsb,1)
}

getNook = function(Gtot, GsImm, a=1, b=.5, c=1, N=1){
  # John Henry will fix this with better functions
  gs.im = gsES(GsImm)
  xp = a + b*Gtot - c*Gtot*gs.im
  rnbinom(N, size=.3, mu=xp)
}

################################################
#  Called from a simulator
################################################

updateHuman_PfLOME = function(t, ixH){
  updateInfection(ixH,t)
  updateImmunity(ixH,t)
  updatePfFever(ixH,t)
  updateTypeImmunity(ixH,t)
  updateRBC(ixH,t)
  updateAnemia(ixH,t)
  updateHRP2(ixH,t)
  updateHistory(ixH,t)
}

dailyUpdate_PfLOME = function(t){
  for(ixH in 1:nHumans)
    updateHuman_PfLOME(t, ixH)
}

updateHistory = function(ixH, t){ with(HUMANS[[ixH]]$Pf, {
  HUMANS[[ixH]]$Pf$History$Ptt <<- c(History$Ptt, Ptot)
  HUMANS[[ixH]]$Pf$History$Gt <<- c(History$Gt, Gtot)
  HUMANS[[ixH]]$Pf$History$BSx <<- rbind(History$BSx, BSImm)
  HUMANS[[ixH]]$Pf$History$PD <<- c(History$PD, getPD(t, Rx))
  HUMANS[[ixH]]$Pf$History$MOI <<- c(History$MOI, MOI)
  HUMANS[[ixH]]$Pf$History$MOI1 <<- c(History$MOI1, sum(activeP>0))
  HUMANS[[ixH]]$History$RBC <<- c(HUMANS[[ixH]]$History$RBC,HUMANS[[ixH]]$RBC)
  HUMANS[[ixH]]$History$Anemia <<- c(HUMANS[[ixH]]$History$Anemia,HUMANS[[ixH]]$Anemia)
  HUMANS[[ixH]]$Pf$History$HRP2 <<- c(History$HRP2,HUMANS[[ixH]]$HRP2)
  if(length(ptypes)>0) {
    HUMANS[[ixH]]$Pf$History$ptypes <<- History$ptypes+ptypes
  }
})}

log10vals = function(x){
  ifelse(!is.na(x) & is.finite(x) & x>=0, x, NaN)
}

log10sum = function(x){
  log10vals(log10(sum(10^log10vals(x), na.rm=TRUE)))
}

#turns ptype into binary matrix, can more easily compare for cross immunity
ptype2Mat = function(ptype,nAntigenLoci,nptypes){
  mat = matrix(0,nAntigenLoci,max(nptypes))
  for(i in 1:nAntigenLoci){
    mat[i,ptype[i]] = mat[i,ptype[i]]+1
  }
  return(mat)
}

shift = function(v,places,dir="right") {
  places = places%%length(v)
  if(places==0) return(v)
  temp = rep(0,length(v))
  if(dir=="left"){
    places = length(v)-places
    dir = "right"}
  if(dir=="right"){
    temp[1:places] = tail(v,places)
    temp[(places+1):length(v)] = v[1:(length(v)-places)]
    return(temp)
  }
}

crossImmunity = function(ptypes,ptypesTime,nAntigenLoci,t,dxp,dtp){
  crossImmune = ptypes
  for(i in 1:nAntigenLoci){
    if(nptypes[i] > 2){
      for(dx in 1:(nptypes[i]-2)){
        ptypestemp = ptypes[i,1:nptypes[i]]
        ptypesTimetemp = ptypesTime[i,1:nptypes[i]]
        dt = t-ptypesTimetemp
        crossImmunetemp = crossImmune[i,1:nptypes[i]]
        crossImmunetemp = crossImmunetemp + weight(dx,dt,dxp,dtp)*(shift(ptypestemp,dx,"right")+shift(ptypestemp,dx,"left"))
        ptypes[i,1:nptypes[i]] = ptypestemp[1:nptypes[i]]
        ptypesTime[i,1:nptypes[i]] = ptypesTimetemp[1:nptypes[i]]
        crossImmune[i,1:nptypes[i]] = crossImmunetemp[1:nptypes[i]]
      }
    }
    if(nptypes[i]==2){
      dx = 1
      ptypestemp = ptypes[i,1:nptypes[i]]
      ptypesTimetemp = ptypesTime[i,1:nptypes[i]]
      dt = t-ptypesTimetemp
      crossImmunetemp = crossImmune[i,1:nptypes[i]]
      crossImmunetemp = crossImmunetemp + weight(dx,dt,dxp,dtp)*shift(ptypestemp,dx)
      ptypes[i,1:nptypes[i]] = ptypestemp[1:nptypes[i]]
      ptypesTime[i,1:nptypes[i]] = ptypesTimetemp[1:nptypes[i]]
      crossImmune[i,1:nptypes[i]] = crossImmunetemp[1:nptypes[i]]
      }
    }
  return(crossImmune)
}
