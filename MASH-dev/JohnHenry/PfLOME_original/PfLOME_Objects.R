################################################
#  The Human Plasmodium falciparum LOME Object
################################################

pathObject_PfLOME = function(nClones=5){
  
  list(
    
    #----------------------------------------
    # Individual Transmission parameters
    #----------------------------------------


    #----------------------------------------
    # Infection
    #----------------------------------------
    MOI = 0,
    alloc_clones = nClones,                # allocated spots, for memory management
    clones       = list(),                 # clonal description
    activeP      = rep(-11,nClones),       # last day of this asexual infection
    activeG      = rep(-11,nClones),       # last day of this asexual infection
    Pt           = rep(NaN,nClones),       # Rolling record of Pt (10 days)
    Gt           = rep(NaN,nClones),       # Current Gt, for each clone
    Ptt          = matrix(NaN,10,nClones), # Rolling record of Pt (10 days)
    Ptot         = NaN,                    # total asexual Pf
    Gtot         = NaN,                    # total Pf mature gametocytes

    #----------------------------------------
    # Immune Counters
    #----------------------------------------
    HBR = 0,
    EIR = 0,
    FOI = 0,
    BSImm = rep(0,nBSImmCounters),
    GSImm = rep(0,nGSImmCounters),
    ptypes = matrix(0,nAntigenLoci,max(nptypes)),
    TypeImmunity = list(),
    FeverThreshold = 7,

    #----------------------------------------
    # Pharmacodynamic Object
    # + for every drug
    # + max log10 reductions in parasite densities
    #----------------------------------------
    Rx  = list(StartTreatment=-1,Drug=-1),

    #----------------------------------------
    # The History Object
    #----------------------------------------
    History  = list(Pt=0, Gt=0, BSx=0, PD=0, ptypes=matrix(0,nAntigenLoci,max(nptypes)))##need to set last as largest nptype
  )
}

#----------------------------------------
# Properties of the bloodstream infection
#----------------------------------------
bsInfection = list(
    P        = NULL,
    G        = NULL,
    MOI      = 0,
    Clones   = list()
)

exposureCounters = list(
  HBR      = 0,
  EIR      = 0,
  LVR      = 0,
  FOI      = 0,
  mFOI     = 0)

PEblock = list(
    bSPZ  = 1,     # sporozoite blocking, non-specific
    mSPZ  = NULL,  # sporozoite blocking, type specific model
    bLIV  = 1,     # liver stage blocking, non-specific
    mLIV  = NULL)  # liver stage blocking, type specific model



pathObject_HUMAN = function(){
  list(
    
    #---------------------------------------------
    # Human Biological Parameters
    #---------------------------------------------
    
    RBC = 2.5, ## x10^13
    Anemia = 0, ## binary status
    Fever = list(), ## simple fever; includes start and end time
    HRP2 = -Inf ## log[HRP2] baseline
  )
}
