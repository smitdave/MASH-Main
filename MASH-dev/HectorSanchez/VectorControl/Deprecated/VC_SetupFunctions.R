####################################################################################################
# Vector control routines
# HMSC
####################################################################################################

##################################################
# Landscape-based Interventions
##################################################
setup.Manually_LANDSCAPE_BasedInterventions = function(
  #. setup.Manually_LANDSCAPE_BasedInterventions: Adds the interventions vectors to the global LANDSCAPE [WARNING Modifies the global LANDSCAPE]
  ATSB_Vector=createRandomUniformCoverageVector(0,LANDSCAPE$N.s),
  AREA.REPEL_Vector=createRandomUniformCoverageVector(0,LANDSCAPE$N.f),
  BAITED.TRAP_Vector=createRandomUniformCoverageVector(0,LANDSCAPE$N.f),
  BIOLOGICAL.CONTROL_Vector=createRandomUniformCoverageVector(0,LANDSCAPE$N.l),
  EAVE.TUBE_Vector=createRandomUniformCoverageVector(0,LANDSCAPE$N.f),
  FOUL.HABITAT_Vector=createRandomUniformCoverageVector(0,LANDSCAPE$N.l),
  IMPROVE.HOME_Vector=createRandomUniformCoverageVector(0,LANDSCAPE$N.f),
  IRS_Vector=createRandomUniformCoverageVector(0,LANDSCAPE$N.f),
  LARVICIDING_Vector=createRandomUniformCoverageVector(0,LANDSCAPE$N.l),
  OVITRAP_Vector=createRandomUniformCoverageVector(0,LANDSCAPE$N.l),
  SOURCE.REDUCTION_Vector=createRandomUniformCoverageVector(0,LANDSCAPE$N.l),
  SWARM.SPRAYING_Vector=createRandomUniformCoverageVector(0,LANDSCAPE$N.m),
  ZOOSPRAY_Vector=createRandomUniformCoverageVector(0,LANDSCAPE$N.f)
){
  LANDSCAPE$s$ATSB<<-ATSB_Vector
  LANDSCAPE$f$AREA.REPEL<<-AREA.REPEL_Vector
  LANDSCAPE$f$BAITED.TRAP<<-BAITED.TRAP_Vector
  LANDSCAPE$l$BIOLOGICAL.CONTROL<<-BIOLOGICAL.CONTROL_Vector
  LANDSCAPE$f$EAVE.TUBE<<-EAVE.TUBE_Vector
  LANDSCAPE$l$FOUL.HABITAT<<-FOUL.HABITAT_Vector
  #LANDSCAPE$f$GM<<-createRandomUniformCoverageVector(.75,LANDSCAPE$N.f)
  LANDSCAPE$f$IMPROVE.HOME<<-IMPROVE.HOME_Vector
  LANDSCAPE$f$IRS<<-IRS_Vector
  LANDSCAPE$l$LARVICIDING<<-LARVICIDING_Vector
  LANDSCAPE$l$OVITRAP<<-OVITRAP_Vector
  LANDSCAPE$l$SOURCE.REDUCTION<<-SOURCE.REDUCTION_Vector
  LANDSCAPE$m$SWARM.SPRAYING<<-SWARM.SPRAYING_Vector
  LANDSCAPE$f$ZOOSPRAY<<-ZOOSPRAY_Vector
}

setup.Random_Uniform_LANDSCAPE_BasedInterventions = function(
  #. setup.Random_Uniform_LANDSCAPE_BasedInterventions: Generates random uniform coverage on the global LANDSCAPE automatically [WARNING Modifies the global LANDSCAPE]
  ATSB_Coverage = 0, AREA.REPEL_Coverage = 0, BAITED.TRAP_Coverage = 0, BIOLOGICAL.CONTROL_Coverage = 0,
  EAVE.TUBE_Coverage = 0, FOUL.HABITAT_Coverage = 0, IMPROVE.HOME_Coverage = 0,
  IRS_Coverage = 0, LARVICIDING_Coverage = 0, OVITRAP_Coverage = 0,
  SOURCE.REDUCTION_Coverage = 0, SWARM.SPRAYING_Coverage = 0,ZOOSPRAY_Coverage = 0
){
  setup.Manually_LANDSCAPE_BasedInterventions(
    ATSB_Vector=createRandomUniformCoverageVector(ATSB_Coverage,LANDSCAPE$N.s),
    AREA.REPEL_Vector=createRandomUniformCoverageVector(AREA.REPEL_Coverage,LANDSCAPE$N.f),
    BAITED.TRAP_Vector=createRandomUniformCoverageVector(BAITED.TRAP_Coverage,LANDSCAPE$N.f),
    BIOLOGICAL.CONTROL_Vector=createRandomUniformCoverageVector(BIOLOGICAL.CONTROL_Coverage,LANDSCAPE$N.l),
    EAVE.TUBE_Vector=createRandomUniformCoverageVector(EAVE.TUBE_Coverage,LANDSCAPE$N.f),
    FOUL.HABITAT_Vector=createRandomUniformCoverageVector(FOUL.HABITAT_Coverage,LANDSCAPE$N.l),
    IMPROVE.HOME_Vector=createRandomUniformCoverageVector(IMPROVE.HOME_Coverage,LANDSCAPE$N.f),
    IRS_Vector=createRandomUniformCoverageVector(IRS_Coverage,LANDSCAPE$N.f),
    LARVICIDING_Vector=createRandomUniformCoverageVector(LARVICIDING_Coverage,LANDSCAPE$N.l),
    OVITRAP_Vector=createRandomUniformCoverageVector(OVITRAP_Coverage,LANDSCAPE$N.l),
    SOURCE.REDUCTION_Vector=createRandomUniformCoverageVector(SOURCE.REDUCTION_Coverage,LANDSCAPE$N.l),
    SWARM.SPRAYING_Vector=createRandomUniformCoverageVector(SWARM.SPRAYING_Coverage,LANDSCAPE$N.m),
    ZOOSPRAY_Vector=createRandomUniformCoverageVector(ZOOSPRAY_Coverage,LANDSCAPE$N.f)
  )
}
##################################################
# Human-based Interventions
##################################################
setup.HUMAN_Based_Intervention = function(name,coverage){
  #. setup.HUMAN_Based_Intervention: Generates random uniform coverage on HUMANS
  #WARNING: MODIFIES THE GLOBAL HUMANS VARIABLE
  test=HUMANS
  #lapply(HUMANS,function(x){x$ITN<-binomialEvent(coverage)})
  for(i in 1:length(test)){test[[i]]$name<-binomialEvent(coverage)}
  test
}
setup.Uniform_HUMAN_BasedInterventions = function(
  #. setup.Uniform_HUMAN_BasedInterventions: Adds the interventions vectors to the global HUMANS [WARNING Modifies the global HUMANS]
  ITN_Coverage=0,SWAT_Coverage=0,PROTECT_Coverage=0,PERSONAL.REPELLANT_Coverage=0,REPEL_Coverage=0
){
  for(i in 1:length(HUMANS)){HUMANS[[i]]$ITN<<-binomialEvent(ITN_Coverage)}
  for(i in 1:length(HUMANS)){HUMANS[[i]]$SWAT<<-binomialEvent(SWAT_Coverage)}
  for(i in 1:length(HUMANS)){HUMANS[[i]]$PROTECT<<-binomialEvent(PROTECT_Coverage)}
  for(i in 1:length(HUMANS)){HUMANS[[i]]$REPEL<<-binomialEvent(REPEL_Coverage)}
  for(i in 1:length(HUMANS)){HUMANS[[i]]$PERSONAL.REPELLANT<<-binomialEvent(PERSONAL.REPELLANT_Coverage)}
}
getHumanFromID = function(hostID){
  HUMANS[[hostID]]
}
