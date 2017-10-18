####################################################################################################
# MASH Vector Control
# HMSC
####################################################################################################

MatingIntervention=R6Class("R6Private",
  #. MatingIntervention(R6): Virtual class to accomodate characteristics common to mating-based interventions
  inherit=ControlIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(),
  public=list()
)
SwarmSpray=R6Class("R6Private",
  #. SwarmSpray (R6): Swarm Spray mosquito control intervention
  inherit=MatingIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(),
  public=list(
    initialize=function(
      id=0,deployedAtTime=0,
      killProbability=.95,repelProbability=0
    ){
      private$id=id
      private$killProbability=killProbability
      private$repelProbability=repelProbability
      private$deployedAtTime=deployedAtTime
    }
  )
)
