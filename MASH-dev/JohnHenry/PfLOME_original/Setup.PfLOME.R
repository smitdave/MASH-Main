
plotting = FALSE
pfid <<- 1

####################################################
# Pedigree Options                                 #
####################################################

####################################################
# ProbeHost Options                                #
####################################################
probeHost = probeHost_0
#ProbeHost = ProbeHost_PfLOME

####################################################
# PE: PRE-ERYTHROCYTIC EVENTS & IMMUNITY
####################################################

####################################################
# Exposure Event Counters                          #
#                                                  #
# Required by:                                     #
#   SPZ2MZ_notypes                                 #
#   SPZ2MZ_types                                   #
#                                                  #
####################################################
#countHBR = countHBR_0
#countEIR = countEIR_0
#countLVR = countLVR_0
#countFOI = countFOI_0

#countHBR = countHBR_1
#countEIR = countEIR_1
#countLVR = countLVR_1
#countFOI = countFOI_1


####################################################
# SPZ2MZ  Options                                  #
#                                                  #
# Functions describing Sporozoites from the        #
# infectious bite to emergence as merozoites       #
####################################################
SPZ2MZ = SPZ2MZ_0
#SPZ2MZ = SPZ2MZ_notypes
#SPZ2MZ = SPZ2MZ_types

#----------------------------------------------------
# How many sporozoites
#----------------------------------------------------
M2HnSPZ = M2HnSPZ_0

#----------------------------------------------------
# non-specific, pre-erythrocytic anti-sporozoite   #
# immunity options                                 #
#                                                  #
# Required by                                      #
#   SPZ2MZ_notypes                                 #
#   SPZ2MZ_types                                   #
#----------------------------------------------------
spzImmnunNS = spzImmunNS_LOME

#----------------------------------------------------
# non-specific, pre-erythrocytic anti-liver        #
# stage immunity options                           #
#                                                  #
# Required by                                      #
#   SPZ2MZ_notypes                                 #
#   SPZ2MZ_types                                   #
#----------------------------------------------------
livImmunNS = livImmunNS_LOME

#----------------------------------------------------
# type-specific, pre-erythrocytic anti-sporozoite
# immunity options
#
# Required by
#   SPZ2MZ_notypes
#   SPZ2MZ_types
#----------------------------------------------------
spzImmunTP = spzImmunTP_NULL

#----------------------------------------------------
# type-specific, pre-erythrocytic anti-liver stage
# immunity options
#
# Required by
#   SPZ2MZ_notypes
#   SPZ2MZ_types
#----------------------------------------------------
livImmunTP = livImmunTP_NULL

#----------------------------------------------------
# type-specific, pre-erythrocytic anti-liver stage
# immunity options
#
# Required by
#   SPZ2MZ_notypes
#   SPZ2MZ_types
#----------------------------------------------------
# Setup an Infection

dPdt = dPdt_tent
gdk = -log10(2)/2.7 #gametocyte death rate

nAntigenLoci = 9
nptypes = c(3,5,4,3,6,3,2,7,9)
dxp = 1
dtp = .1
mu = .1 #mutation probability at each locus during parasite gametogenesis - either scalar or vector of length nAntigenLoci

# John Henry will seed PfPedigree with a set of existing types

#----------------------------------------------------
# Set up the immune modulation for genotypes
#----------------------------------------------------

####################################################
# BS: ASEXUAL BLOOD STAGE DYNAMICS, INFECTION
#     & IMMUNITY
####################################################

#----------------------------------------------------
# Make the time course of infection for a new clone
#----------------------------------------------------

makeClone = makeClone_tent
dPdt=dPdt_tent
nBSImmCounters = 14
BSImmCounters = list()
wx = c(rep(1/30, 3), rep(1/90, 3), rep(1/270, 3), 1/5/365)
wn = c(rep(c(1/30, 1/90, 1/270), 3), 0)

for(i in 1:10){
  BSImmCounters[[i]] = list(
    PAR = gImPAR(wx=wx[i], wn=wn[i], P50=6, Ps=5),
    F = dynamicXdt
  )
}

pthresh = c(6,9)
for(i in length(wx)+1:2){
  BSImmCounters[[i]] = list(
    PAR = list(Pthresh = 1+i%%2),
    F = daysSinceOver
  )
}

for(i in length(wx)+2+c(1,2)){
  BSImmCounters[[i]] = list(
    PAR = list(Pthresh=1+i%%2),
    F = daysSinceUnder
  )
}

nGSImmCounters = 1
GSImmCounters = list()
GSImmCounters[[1]] = list(
    PAR = gImPAR(wx=1/800, wn=0, P50=6),
    F = dynamicXdt
)

#----------------------------------------------------
# Set up the immune modulation for each counter
#----------------------------------------------------

