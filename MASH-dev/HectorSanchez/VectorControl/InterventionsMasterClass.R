####################################################################################################
# MASH Vector Control
# HMSC
####################################################################################################
ControlIntervention=R6Class("R6Private",
  #. ControlIntervention(R6): Virtual class to contain characteristics common to all interventions
  portable=TRUE,class=TRUE,cloneable=TRUE,
  private = list(
    id=0,
    name="CI",
    deployedAtTime=0,
    cyclesSinceDeployment=0,
    killProbability=0,
    repelProbability=0
  ),
  public=list(
    # Initializer: JUST FOR TESTING PURPOSES AS THIS CLASS IS VIRTUAL! SHOULD BE COMMENTED OUT AFTER DEBUGGING!
    initialize=function(killProbability=0){private$killProbability=killProbability},
    # Accessors
    getID=function(){private$id},
    getName=function(){private$name},
    getDeploymentTime=function(){private$deployedAtTime},
    getKillProbability=function(){private$killProbability},
    getCyclesCounter=function(){private$cyclesSinceDeployment},
    getRepel=function(){private$repelProbability},
    # Kill and Repel actions
    mosquitoKillEncounter=function(M,interventionType="VC"){
      if(binomialEvent(private$killProbability)){
        #print(paste0("Killed by: ",interventionType))
        M$stateNew="D"
      }
      return(M)
    },
    mosquitoRepelEncounter=function(M,interventionType="VC"){
      if(binomialEvent(private$repelProbability)){
        #print(paste0("Repelled by: ",interventionType))
        M$lspot="l"
      }
      return(M)
    },
    # Timers
    updateCyclesCounter=function(){
      private$cyclesSinceDeployment=private$cyclesSinceDeployment+1
    },
    # Efficacy decay functions
    constantRateKillProbabilityDecay=function(kFactor=.95){
      private$killProbability=private$killProbability*kFactor
    },
    exponentialRateKillProbabilityDecay=function(lambda=.25){
      private$killProbability=private$killProbability*exp(-lambda*private$cyclesSinceDeployment)
    },
    # Efficacy failure functions
    ratioKillProbabilityReduction=function(reductionRatio=1){
      private$killProbability=private$killProbability*reductionRatio
    }
  )
)

binomialEvent = function(probability){
  bool=FALSE
  if(runif(1)<probability){bool=TRUE}
  return(bool)
}

# testVC=ControlIntervention$new(killProbability=1)
# testVC$getKillProbability()
# testVC$updateCyclesCounter()
# testVC$exponentialRateKillProbabilityDecay()
# testVC$getKillProbability()
#
# testVC$ratioKillProbabilityReduction(.5)
