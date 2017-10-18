####################################################################################################
# MASH Vector Control
# HMSC
####################################################################################################
DistributeLandscapeInterventionsUniformly=function(LANDSCAPE=LANDSCAPE){
  #. DistributeLandscapeInterventionsUniformly:
  if(BAITED.TRAP_ON){for(i in 1:length(LANDSCAPE$feedSites)){LANDSCAPE$feedSites[[i]]$OdorBaitedTrap=ATSB$new(id=i)}}
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
  if(AERIAL.SPRAY_ON){for(i in 1:length(LANDSCAPE$feedSites)){LANDSCAPE$feedSites[[i]]$AerialSpray=AerialSpray$new(id=i)}}
  if(AERIAL.SPRAY_ON){for(i in 1:length(LANDSCAPE$sugarSites)){LANDSCAPE$sugarSites[[i]]$AerialSpray=AerialSpray$new(id=i)}}
  if(AERIAL.SPRAY_ON){for(i in 1:length(LANDSCAPE$swarmSites)){LANDSCAPE$swarmSites[[i]]$AerialSpray=AerialSpray$new(id=i)}}
  return(LANDSCAPE)
}
DistributeHumanInterventionsUniformly=function(HUMANS=HUMANS){
  #. DistributeHumanInterventionsUniformly:
  if(PERSONAL.REPELLANT_ON){for(i in 1:length(HUMANS)){HUMANS[[i]]$PersonalRepellant=PersonalRepellant$new(id=i)}}
  if(ITN_ON){for(i in 1:length(HUMANS)){HUMANS[[i]]$ITN=ITN$new(id=i)}}
  if(SWAT_ON){for(i in 1:length(HUMANS)){HUMANS[[i]]$Swat=Swat$new(id=i)}}
  return(HUMANS)
}
DistributeCattleInterventionsUniformly=function(LANDSCAPE=LANDSCAPE){
  #. DistributeCattleInterventionsUniformly:
  if(IVERMECTIN_ON){for(i in 1:length(LANDSCAPE$feedSites)){LANDSCAPE$feedSites[[i]]$Ivermectin=Ivermectin$new(id=i)}}
  if(ZOOSPRAY_ON){for(i in 1:length(LANDSCAPE$feedSites)){LANDSCAPE$feedSites[[i]]$Zoospray=Zoospray$new(id=i)}}
  return(LANDSCAPE)
}
