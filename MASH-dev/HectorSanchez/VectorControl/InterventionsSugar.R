####################################################################################################
# MASH Vector Control
# HMSC
####################################################################################################

SugarIntervention=R6Class("R6Private",
  #. SugarIntervention(R6): Virtual class to accomodate characteristics common to sugar-based interventions
  inherit=ControlIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(),
  public=list()
)

ATSB=R6Class("R6Private",
  #. ATSB (R6): Attractive Sugar Baits Intervention
  inherit=SugarIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(
    name="ATSB"
  ),
  public=list(
    initialize=function(id=0,killProbability=.9,repelProbability=0){
      private$id=id
      private$killProbability=killProbability
      private$repelProbability=repelProbability
    }
  )
)

# atsbInstance=ATSB$new(id=10)
# atsbInstance$getKillProbability()
# atsbInstance$setKillProbability(.5)
# atsbInstance$getKillProbability()
