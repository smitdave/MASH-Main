####################################################################################################
# MASH Vector Control
# HMSC
####################################################################################################
AquaticIntervention=R6Class("R6Private",
  #. AquaticIntervention(R6): Virtual class to accomodate characteristics common to aquatic-based interventions
  # Class characteristics ------------------------------------------------------
  inherit=ControlIntervention,
  portable=TRUE,class=TRUE,cloneable=FALSE,
  # Internal variables ---------------------------------------------------------
  private=list(),
  # Methods --------------------------------------------------------------------
  public=list()
)
Ovitrap=R6Class("R6Private",
  #. Ovitrap (R6): Ovitrap mosquito control intervention
  inherit=AquaticIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(),
  public=list(
    initialize=function(id=0,killProbability=.7,repelProbability=0){
      private$id=id
      private$killProbability=killProbability
      private$repelProbability=repelProbability

    }
  )
)
Larvicide=R6Class("R6Private",
  #. Larviciding (R6): Larviciding mosquito control intervention
  inherit=AquaticIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(),
  public=list(
    initialize=function(id=0,killProbability=.9,repelProbability=0){
      private$id=id
      private$killProbability=killProbability
      private$repelProbability=repelProbability
    }
  )
)
SourceReduction=R6Class("R6Private",
  #. SourceReduction (R6): Source reduction mosquito control intervention
  inherit=AquaticIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(),
  public=list(
    initialize=function(id=0,killProbability=.9,repelProbability=0){
      private$id=id
      private$killProbability=killProbability
      private$repelProbability=repelProbability
    }
  )
)
FoulHabitat=R6Class("R6Private",
  #. FoulHabitat (R6): Foul habitat mosquito control intervention
  inherit=AquaticIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(),
  public=list(
    initialize=function(id=0,killProbability=.9,repelProbability=0){
      private$id=id
      private$killProbability=killProbability
      private$repelProbability=repelProbability
    }
  )
)
BiologicalControl=R6Class("R6Private",
  #. BiologicalControl (R6): Biological control mosquito control intervention
  inherit=AquaticIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(),
  public=list(
    initialize=function(id=0,killProbability=.9,repelProbability=0){
      private$id=id
      private$killProbability=killProbability
      private$repelProbability=repelProbability
    }
  )
)
