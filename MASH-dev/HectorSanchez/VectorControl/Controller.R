####################################################################################################
# MASH Vector Control
# HMSC
####################################################################################################
VectorControlInterventionsController=R6Class("R6Private",
  #. VectorControlController(R6): Controller class that deals with intervention's distribution, decay, timing, etcetera.
  portable=TRUE,class=TRUE,cloneable=TRUE,
  private = list(
  ),
  public=list(
    distributeLandscapeInterventionsUniformly=function(LANDSCAPE=LANDSCAPE){
      #. DistributeLandscapeInterventionsUniformly:
      if(BAITED.TRAP_ON){for(i in 1:length(LANDSCAPE$feedSites)){LANDSCAPE$feedSites[[i]]$OdorBaitedTrap=OdorBaitedTrap$new(id=i)}}
      if(ATSB_ON){for(i in 1:length(LANDSCAPE$sugarSites)){LANDSCAPE$sugarSites[[i]]$ATSB=ATSB$new(id=i)}}
      if(OVITRAP_ON){for(i in 1:length(LANDSCAPE$aquaSites)){LANDSCAPE$aquaSites[[i]]$Ovitrap=Ovitrap$new(id=i)}}
      if(BIOLOGICAL.CONTROL_ON){for(i in 1:length(LANDSCAPE$aquaSites)){LANDSCAPE$aquaSites[[i]]$BiologicalControl=BiologicalControl$new(id=i)}}
      if(LARVICIDES_ON){for(i in 1:length(LANDSCAPE$aquaSites)){LANDSCAPE$aquaSites[[i]]$Larvicide=Larvicide$new(id=i)}}
      if(SOURCE.REDUCTION_ON){for(i in 1:length(LANDSCAPE$aquaSites)){LANDSCAPE$aquaSites[[i]]$SourceReduction=SourceReduction$new(id=i)}}
      if(FOUL.HABITAT_ON){for(i in 1:length(LANDSCAPE$aquaSites)){LANDSCAPE$aquaSites[[i]]$FoulHabitat=FoulHabitat$new(id=i)}}
      if(SWARM.SPRAYING_ON){for(i in 1:length(LANDSCAPE$swarmSites)){LANDSCAPE$swarmSites[[i]]$SwarmSpray=SwarmSpray$new(id=i)}}
      if(IRS_ON){for(i in 1:length(LANDSCAPE$feedSites)){LANDSCAPE$feedSites[[i]]$IRS=IRS$new(id=i)}}
      if(EAVE.TUBE_ON){for(i in 1:length(LANDSCAPE$feedSites)){LANDSCAPE$feedSites[[i]]$EaveTube=EaveTube$new(id=i)}}
      if(IMPROVE.HOME_ON){for(i in 1:length(LANDSCAPE$feedSites)){LANDSCAPE$feedSites[[i]]$ImproveHome=ImproveHome$new(id=i)}}
      return(LANDSCAPE)
    },
    distributeHumanInterventionsUniformly=function(HUMANS=HUMANS){
      #. DistributeHumanInterventionsUniformly:
      if(PERSONAL.REPELLANT_ON){for(i in 1:length(HUMANS)){HUMANS[[i]]$PersonalRepellant=PersonalRepellant$new(id=i)}}
      if(ITN_ON){for(i in 1:length(HUMANS)){HUMANS[[i]]$ITN=ITN$new(id=i)}}
      return(HUMANS)
    },
    distributeCattleInterventionsUniformly=function(LANDSCAPE=LANDSCAPE){
      #. DistributeCattleInterventionsUniformly:
      if(IVERMECTIN_ON){for(i in 1:length(LANDSCAPE$feedSites)){LANDSCAPE$feedSites[[i]]$Ivermectin=Ivermectin$new(id=i)}}
      if(ZOOSPRAY_ON){for(i in 1:length(LANDSCAPE$feedSites)){LANDSCAPE$feedSites[[i]]$Zoospray=Zoospray$new(id=i)}}
      return(LANDSCAPE)
    }
  )
)


test=ATSB
LANDSCAPE$feedSites[[1]]$test=test$new(id=0)

testDecay=constantRateKillProbabilityDecay
LANDSCAPE$feedSites[[1]]$ATSB$testDecay()



deployInterventionAtSite=function(interventionObject,LANDSCAPE_site){
  tempName=interventionObject$getName()
  LANDSCAPE_site[[tempName]]=interventionObject
  print(LANDSCAPE_site)
}

testATSB=ATSB$new()
deployIntervention(testATSB,LANDSCAPE$feedSites[[1]])
