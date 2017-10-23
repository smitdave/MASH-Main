#################################################################
#
#   MASH
#   R6-ified
#   MBITES Module Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   July 19, 2017
#
#################################################################

#################################################################
# M-BITES FULL Parameters
#################################################################

#' Generate Parameters for M-BITES 'Full' Module
#'
#' Generate named list of parameters used throughout M-BITES 'Full' Module. All arguments have default values which are listed below before the definition.
#'
#' @param enterhouse.a = 9, # beta house entry parameters
#' @param enterhouse.b = 1, # beta house entry parameters
#' @param feedHaz.a = 99, # beta feeding site hazard weights
#' @param feedHaz.b = 1, # beta feeding site hazard weights
#' @param aquaHaz.a = 99, # beta aqua site hazard weights
#' @param aquaHaz.b = 1, # beta aqua site hazard weights
#' @param sugarHaz.a = 99, # beta sugar site hazard weights
#' @param sugarHaz.b = 1, # beta sugar site hazard weights
#' @param swarmHaz.a = 99, # beta swarm site hazard weights
#' @param swarmHaz.b = 1, # beta swarm site hazard weights
#' @param F.t = 1, #blood feed search bout timing
#' @param B.t = 0.75, #blood feed attempt bout timing
#' @param R.t = 1.5, #resting bout timing
#' @param L.t = 0.75, #egg laying search bout timing
#' @param O.t = 1, #egg laying attempt bout timing
#' @param M.t = 1.5, #mating bout timing
#' @param S.t = 0.5, #sugar feeding bout timing
#' @param F.s = 0.99,
#' @param B.s = 0.99,
#' @param L.s = 0.99,
#' @param O.s = 0.99,
#' @param M.s = 0.95,
#' @param S.s = 0.95,
#' @param F.p = 0.95,
#' @param B.p = 0.98,
#' @param R.p = 0.98,
#' @param L.p = 0.80,
#' @param O.p = 0.98,
#' @param M.p = 0.98,
#' @param S.p = 0.98,
#' @param maleM.s = .99,
#' @param maleM.p = .95,
#' @param Q = 0.9, #human blood index
#' @param reFeed = 0.01, #probability to refeed post resting bout
#' @param bm.a = 7.5, #shape param for bloodmeal size
#' @param bm.b = 2.5, #shape param for bloodmeal size
#' @param rf.a = 60, #exp param for refeeding as function of bmSize
#' @param rf.b = 5e3, #exp param for refeeding as function of bmSize
#' @param of.a = 5, #exp param for overfeeding as function of bmSize
#' @param of.b = 5e3, #exp param for overfeeding as function of bmSize
#' @param S.a = 20, #exp param for death as function of energy
#' @param S.b = 10, #exp param for death as function of energy
#' @param S.u = 1/7, #energy expenditure for each bout
#' @param S.sa = 15, #exp param for sugar bout as function of energy
#' @param S.sb = 5, #exp param for sugar bout as function of energy
#' @param sf.a = 7.5, #shape param for sugar feed size
#' @param sf.b = 4.5, #shape param for sugar feed size
#' @param B.energy = 1/10, #scaling factor energy from a bloodmeal
#' @param energyPreG = 0, #pre-gonotrophic energy requirement
#' @param preGsugar = 0,
#' @param preGblood = 0,
#' @param sns.a = 0.085, #exp param for senescence
#' @param sns.b = 100, #exp param for senescence
#' @param ttsz.p = 0.5, #zero-inflation for tattering damage
#' @param ttsz.a = 5, #shape param for tattering damage
#' @param ttsz.b = 95, #shape param for tattering damage
#' @param ttr.a = 15, #exp param for tattering survival
#' @param ttr.b = 500, #exp param for tattering survival
#' @param maxBatch = 30, #maximum batch size
#' @param E.p = 0.5, # probability of surviving estivation
#' @param Emax = 90, # onset of the dry season; a day of the calendar year
#' @param Eb = 0.9,
#' @param eEndm = 180, # end of estivation; a day of the calendar year
#' @param eEndV = 30,
#' @param surviveH = 1,
#' @param probeH = 1,
#' @param surviveprobeH = 1,
#' @param feedH = 1,
#' @param surviveL = 1,
#' @param feedL = 1,
#' @param SUGAR = TRUE,
#' @param ESTIVATION = FALSE,
#' @param MATE = TRUE,
#' @param SENESCE = TRUE, #senesce
#' @param TATTER = TRUE, #tattering
#' @param REFEED = TRUE,
#' @param OVERFEED TRUE; simulate overfeeding? See \code{\link{overFeed}}
#' @param HISTORY TRUE; record history? Needed for \code{\link{cohortBionomics}}, see \code{\link{historyTrack}}
#' @param batchSize "bms"; switches for how egg batch size is calculated, should be either "bms" or "norm", see \code{\link{BatchSize.bms}} or \code{\link{BatchSize.norm}}
#' @param eggMatT "off"; switches for how egg maturation is calculated, should be either "off" or "norm", see \code{\link{eggMaturationTime.off}} or \code{\link{eggMaturationTime.norm}}
#' @param InAndOut matrix of weights for landing spot, see \code{\link{newSpot}}
#' @param Fwts rep(1,5); blood feeding search bout landing spot weights, see \code{\link{newSpot}}
#' @param Rwts rep(1,5); resting bout landing spot weights, see \code{\link{newSpot}}
#' @param Lwts rep(1,5); egg laying search bout landing spot weights, see \code{\link{newSpot}}
#' @param Owts rep(1,5); egg laying attempt bout landing spot weights, see \code{\link{newSpot}}
#' @param Mwts rep(1,5); mating bout landing spot weights, see \code{\link{newSpot}}
#' @param Swts rep(1,5); sugar bout landing spot weights, see \code{\link{newSpot}}
#' @param eggT the minimum time before eggs are mature
#' @param eggP the minimum provision before eggs are mature
#' @param Pathogen name of the PATHOGEN module to use, can be in "PfSI", "PfMOI"
#' @return a named list of parameters
#' @examples
#' P = MBITES.FULL.Parameters()
#' @export
MBITES.FULL.Parameters <- function(

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
  eggP.min = 0.5, # minimum energy provision for eggs to mature

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
  SUGAR = TRUE,
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

  eggT = 0,
  eggP = 0,
  Pathogen = "PfSI"


){

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

    eggT = eggT,
    eggP = eggP,
    energyPreG = energyPreG,
    Pathogen = Pathogen
  ))

}
