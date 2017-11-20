####################################################################################################
# MASH Vector Control
# HMSC
####################################################################################################

MultiLocationIntervention=R6Class("R6Private",
  #. SugarIntervention(R6): Virtual class to accomodate characteristics common to multi-location-based interventions
  inherit=ControlIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(),
  public=list()
)

AerialSpray=R6Class("R6Private",
  #. AerialSpray (R6): Aerial Spray
  inherit=MultiLocationIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(
    name="AERO"
  ),
  public=list(
    initialize=function(id=0,killProbability=.5,repelProbability=.05){
      private$id=id
      private$killProbability=killProbability
      private$repelProbability=repelProbability
    }
  )
)
