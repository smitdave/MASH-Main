####################################################################################################
# MASH SOURCE FILES
# HMSC
####################################################################################################
CattleIntervention=R6Class("R6Private",
  #. CattleIntervention(R6): Virtual class to accomodate characteristics common to cattle-based interventions
  inherit=ControlIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(),
  public=list(
    mosquitoKillBinomialEvent=function(){binomialEvent(private$killProbability)},
    mosquitoRepelBinomialEvent=function(){binomialEvent(private$repelProbability)},
    mosquitoProbeDeterBinomialEvent=function(){binomialEvent(private$probeDeterProbability)},
    mosquitoProbeKillBinomialEvent=function(){binomialEvent(private$killProbingProbability)}
  )
)
Ivermectin=R6Class("R6Private",
  #. Ivermectin(R6): Ivermectin mosquito control intervention (MBites-Bouts>boutB)
  inherit=CattleIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(
    name="Ivermectin"
  ),
  public=list(
    initialize=function(id=0,killProbability=.9,repelProbability=0){
      private$id=id
      private$killProbability=killProbability
      private$repelProbability=repelProbability
    }
  )
)
Zoospray=R6Class("R6Private",
  #. Zoospray (R6): Zoospray mosquito control intervention (MBites-Bouts>boutB)
  inherit=CattleIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(),
  public=list(
    initialize=function(id=0,killProbability=.75,repelProbability=0){
      private$id=id
      private$killProbability=killProbability
      private$repelProbability=repelProbability
    }
  )
)
