####################################################################################################
# MASH SOURCE FILES
# HMSC
####################################################################################################

FeedingIntervention=R6Class("R6Private",
  #. FeedingIntervention(R6): Virtual class to accomodate characteristics common to bloodfeeding site-based interventions
  inherit=ControlIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(),
  public=list()
)
OdorBaitedTrap=R6Class("R6Private",
  #. OdorBaitedTrap(R6): B
  inherit=FeedingIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(
    name="OdorBaitedTrap"
  ),
  public=list(
    initialize=function(id=0,killProbability=.75,repelProbability=0){
      private$id=id
      private$killProbability=killProbability
      private$repelProbability=repelProbability
    },
    mosquitoKillBinomialEvent=function(){binomialEvent(private$killProbability)},
    mosquitoRepelBinomialEvent=function(){binomialEvent(private$repelProbability)}
  )
)
IRS=R6Class("R6Private",
  #. IRS(R6): Indoor residual spraying intervention (MBITES-Bouts>boutR)
  inherit=FeedingIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(
    name="IRS"
  ),
  public=list(
    initialize=function(id=0,killProbability=.75,repelProbability=0){
      private$id=id
      private$killProbability=killProbability
      private$repelProbability=repelProbability
    }
  )
)
EaveTube=R6Class("R6Private",
  #. EaveTube(R6): Eave tubes mosquito control intervention (MBITES-Bouts>enterHouse)
  inherit=FeedingIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(
    name="Eave Tube"
  ),
  public=list(
    initialize=function(id=0,killProbability=1,repelProbability=0){
      private$id=id
      private$killProbability=killProbability
      private$repelProbability=repelProbability
    }
  )
)
ImproveHome=R6Class("R6Private",
  #. ImproveHome(R6): Home improvement mosquito control intervention (MBITES-Bouts>enterHouse)
  inherit=FeedingIntervention,portable=TRUE,class=TRUE,cloneable=FALSE,
  private=list(
    name="Improve Home"
  ),
  public=list(
    initialize=function(id=0,killProbability=.1,repelProbability=.8){
      private$id=id
      private$killProbability=killProbability
      private$repelProbability=repelProbability
    }
  )
)
