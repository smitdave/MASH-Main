#################################################################
#
#   MASH/MBITES
#   MBITES Parameters
#   R version
#   Sean Wu
#   January 27, 2017
#
#################################################################

MBITES.PAR <- function(

  # landscape creation parameters
  enterhouse.a = 9, # beta house entry parameters
  enterhouse.b = 1, # beta house entry parameters
  feedHaz.a = 99, # beta feeding site hazard weights
  feedHaz.b = 1, # beta feeding site hazard weights
  aquaHaz.a = 99, # beta aqua site hazard weights
  aquaHaz.b = 1, # beta aqua site hazard weights
  sugarHaz.a = 99, # beta sugar site hazard weights
  sugarHaz.b = 1, # beta sugar site hazard weights
  swarmHaz.a = 99, # beta swarm site hazard weights
  swarmHaz.b = 1, # beta swarm site hazard weights

  #rate parameters for exponential timing
  F.t = 1, #blood feed search bout timing
  B.t = 0.75, #blood feed attempt bout timing
  R.t = 1.5, #resting bout timing
  L.t = 0.75, #egg laying search bout timing
  O.t = 1, #egg laying attempt bout timing
  M.t = 1.5, #mating bout timing
  S.t = 0.5, #sugar feeding bout timing

  #success parameters
  F.s = 0.99,
  B.s = 0.99,
  L.s = 0.99,
  O.s = 0.99,
  M.s = 0.95,
  S.s = 0.95,

  #survival parameters
  F.p = 0.95,
  B.p = 0.98,
  R.p = 0.98,
  L.p = 0.80,
  O.p = 0.98,
  M.p = 0.98,
  S.p = 0.98,

  #male specific parameters
  maleM.s = .99,
  maleM.p = .95,

  #energetics and feeding
  Q = 0.9, #human blood index
  reFeed = 0.01, #probability to refeed post resting bout
  bm.a = 7.5, #shape param for bloodmeal size
  bm.b = 2.5, #shape param for bloodmeal size
  rf.a = 60, #exp param for refeeding as function of bmSize
  rf.b = 5e3, #exp param for refeeding as function of bmSize
  of.a = 5, #exp param for overfeeding as function of bmSize
  of.b = 5e3, #exp param for overfeeding as function of bmSize
  S.a = 20, #exp param for death as function of energy
  S.b = 10, #exp param for death as function of energy
  S.u = 1/7, #energy expenditure for each bout
  S.sa = 15, #exp param for sugar bout as function of energy
  S.sb = 5, #exp param for sugar bout as function of energy
  sf.a = 7.5, #shape param for sugar feed size
  sf.b = 4.5, #shape param for sugar feed size

  B.energy = 1/10, #scaling factor energy from a bloodmeal
  energyPreG = 0, #pre-gonotrophic energy requirement
  preGsugar = 0,
  preGblood = 0,

  #senescence and tattering
  sns.a = 0.085, #exp param for senescence
  sns.b = 100, #exp param for senescence
  ttsz.p = 0.5, #zero-inflation for tattering damage
  ttsz.a = 5, #shape param for tattering damage
  ttsz.b = 95, #shape param for tattering damage
  ttr.a = 15, #exp param for tattering survival
  ttr.b = 500, #exp param for tattering survival

  #egg production
  maxBatch = 30, #maximum batch size

  # estivation
  E.p = 0.5, # probability of surviving estivation
  Emax = 90, # onset of the dry season; a day of the calendar year
  Eb = 0.9,
  eEndm = 180, # end of estivation; a day of the calendar year
  eEndV = 30,

  #human host
  surviveH = 1,
  probeH = 1,
  surviveprobeH = 1,
  feedH = 1,

  #animal host
  surviveL = 1,
  feedL = 1,

  #control parameters
  ESTIVATION = FALSE,
  MATE = TRUE,
  SENESCE = TRUE, #senesce
  TATTER = TRUE, #tattering
  REFEED = TRUE,
  OVERFEED = TRUE,
  HISTORY = TRUE,

  # switches
  batchSize = "bms", # should be {"bms","norm"}
  eggMatT = "off", # should be {"off","norm"}

  #landing and movement
  InAndOut = matrix(
    c(
      c(4,2,1,0,1),
      c(2,1,1,0,1),
      c(2,1,1,0,2),
      c(1,1,1,0,1),
      c(6,4,2,1,0)
    ), byrow=FALSE, ncol=5),

  Fwts = rep(1,5),
  Rwts = rep(1,5),
  Lwts = rep(1,5),
  Owts = rep(1,5),
  Mwts = rep(1,5),
  Swts = rep(1,5),

  # Plasmodium falciparum specifc parameters
  r = 1/38, # recovery rate (mean length of infection)
  b = 0.55, # vector to human transmission efficiency
  c = 0.15, # human to vector transmission efficiency
  EIP = 12 # extrinsic incubation period

){

  # set global flags (will be false until set to true by appropriate function)
  EggQ_TRACK <<- FALSE

  return(list(

    # landscape creation parameters
    enterhouse.a = enterhouse.a, # beta house entry weights
    enterhouse.b = enterhouse.b, # beta house entry weights
    enterhouse.mean = enterhouse.a / (enterhouse.a + enterhouse.b), # mean of beta distributed house entry probabilities
    feedHaz.a = feedHaz.a, # beta feeding site hazard weights
    feedHaz.b = feedHaz.b, # beta feeding site hazard weights
    feedHaz.mean = feedHaz.a / (feedHaz.a + feedHaz.b), # mean
    aquaHaz.a = aquaHaz.a, # beta aqua site hazard weights
    aquaHaz.b = aquaHaz.b, # beta aqua site hazard weights
    aquaHaz.mean = aquaHaz.a / (aquaHaz.a + aquaHaz.b), # mean
    sugarHaz.a = sugarHaz.a, # beta sugar site hazard weights
    sugarHaz.b = sugarHaz.b, # beta sugar site hazard weights
    sugarHaz.mean = sugarHaz.a / (sugarHaz.a + sugarHaz.b), # mean
    swarmHaz.a = swarmHaz.a, # beta swarm site hazard weights
    swarmHaz.b = swarmHaz.b, # beta swarm site hazard weights
    swarmHaz.mean = swarmHaz.a / (swarmHaz.a + swarmHaz.b), # mean

    #timing parameters
    F.t=F.t, B.t=B.t, R.t=R.t, L.t=L.t, O.t=O.t, M.t=M.t, S.t=S.t,

    #success parameters
    F.s=F.s, B.s=B.s, L.s=L.s, O.s=O.s, M.s=M.s, S.s=S.s,

    #survival parameters
    F.p=F.p, B.p=B.p, R.p=R.p, L.p=L.p, O.p=O.p, M.p=M.p, S.p=S.p,

    #male specific parameters
    maleM.s=maleM.s,
    maleM.p=maleM.p,

    #energetics & feeding
    Q=Q, reFeed=reFeed, bm.a=bm.a, bm.b=bm.b, rf.a=rf.a, rf.b=rf.b, of.a=of.a, of.b=of.b, S.a=S.a, S.b=S.b, S.u=S.u, S.sa=S.sa, S.sb=S.sb, sf.a=sf.a, sf.b=sf.b,
    B.energy = B.energy,
    energyPreG = energyPreG,
    preGsugar = preGsugar,
    preGblood = preGblood,

    #senescence & tattering
    sns.a=sns.a, sns.b=sns.b, ttsz.p=ttsz.p, ttsz.a=ttsz.a, ttsz.b=ttsz.b, ttr.a=ttr.a, ttr.b=ttr.b,

    #egg production
    maxBatch=maxBatch,

    # estivation
    E.p = E.p,
    Emax = Emax,
    Eb = Eb,
    eEndm = eEndm,
    eEndV = eEndV,

    #human host
    surviveH=surviveH, probeH=probeH, surviveprobeH=surviveprobeH, feedH=feedH,

    #animal host
    surviveL=surviveL, feedL=feedL,

    # switches
    batchSize = batchSize,
    eggMatT = eggMatT,

    #house entering & resting parameters
    InAndOut=InAndOut, Fwts=Fwts, Rwts=Rwts, Lwts=Lwts, Owts=Owts, Mwts=Mwts, Swts=Swts,

    #control parameters
    SUGAR=SUGAR, ESTIVATION=ESTIVATION, MATE=MATE, SENESCE=SENESCE, TATTER=TATTER, REFEED=REFEED, OVERFEED=OVERFEED, HISTORY=HISTORY,

    r = r, # recovery rate (mean length of infection)
    b = b, # vector to human transmission efficiency
    c = c, # human to vector transmission efficiency
    EIP = EIP # extrinsic incubation period
  ))

}