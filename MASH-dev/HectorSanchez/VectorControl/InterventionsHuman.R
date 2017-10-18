####################################################################################################
# MASH SOURCE FILES
# HMSC
####################################################################################################

HumanIntervention=R6Class("R6Private",
  #. HumanIntervention(R6): Virtual class to accomodate characteristics common to human-based interventions
  inherit=ControlIntervention,
  portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(
    probeDeterProbability=0,
    feedDeterProbability=0,
    killProbingProbability=0
  ),
  public=list(
    mosquitoKillBinomialEvent=function(){binomialEvent(private$killProbability)},
    mosquitoRepelBinomialEvent=function(){binomialEvent(private$repelProbability)},
    mosquitoProbeDeterBinomialEvent=function(){binomialEvent(private$probeDeterProbability)},
    mosquitoProbeKillBinomialEvent=function(){binomialEvent(private$killProbingProbability)}
  )
)
PersonalRepellant=R6Class("R6Private",
  #. PersonalRepellant(R6): Personal repellant for humans (MBITES-hostEncounter>humanEncounter)
  inherit=HumanIntervention,
  portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(),
  public=list(
    initialize=function(
        id=0,deployedAtTime=0,
        killProbability=0,repelProbability=.75,probeDeterProbability=.5,feedDeterProbability=.25,killProbingProbability=0
      ){
        private$id=id
        private$killProbability=killProbability
        private$repelProbability=repelProbability
        private$probeDeterProbability=probeDeterProbability
        private$feedDeterProbability=feedDeterProbability
        private$killProbingProbability=killProbingProbability
        private$deployedAtTime=deployedAtTime
      }
    )
)
ITN=R6Class("R6Private",
  #. ITN(R6): ITN for humans (MBITES-hostEncounter>humanEncounter)
  inherit=HumanIntervention,
  portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(),
  public=list(
    initialize=function(
        id=0,deployedAtTime=0,
        killProbability=0,repelProbability=.75,probeDeterProbability=.5,feedDeterProbability=.25,killProbingProbability=.5
      ){
        private$id=id
        private$killProbability=killProbability
        private$repelProbability=repelProbability
        private$probeDeterProbability=probeDeterProbability
        private$feedDeterProbability=feedDeterProbability
        private$killProbingProbability=killProbingProbability
        private$deployedAtTime=deployedAtTime
    }
  )
)
Swat=R6Class("R6Private",
  #. Swat(R6): Swatter implementation in humans (MBITES-hostEncounter>humanEncounter)
  inherit=HumanIntervention,
  portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(),
  public=list(
    initialize=function(
        id=0,deployedAtTime=0,
        killProbability=.25,repelProbability=.1,probeDeterProbability=.25,feedDeterProbability=.25,killProbingProbability=.15
      ){
        private$id=id
        private$killProbability=killProbability
        private$repelProbability=repelProbability
        private$probeDeterProbability=probeDeterProbability
        private$feedDeterProbability=feedDeterProbability
        private$killProbingProbability=killProbingProbability
        private$deployedAtTime=deployedAtTime
    }
  )
)

testITN=ITN$new()
testITN$mosquitoRepelBinomialEvent()

testPR=PersonalRepellant$new()
testPR$mosquitoRepelBinomialEvent()
