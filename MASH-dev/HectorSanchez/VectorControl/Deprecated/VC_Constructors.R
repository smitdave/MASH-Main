##################################################
# Vector Control Constructors
# HMSC
##################################################
setup.VectorControlIntervention = function(
    activationBool=FALSE,
    SurvivalProbability=1,
    ProbeProbability=1,
    RepelProbability=0,
    FeedProbability=1,
    SurviveProbeProbability=0,
    CoverageLevel=0,
    CoveragePattern="Uniform"
  ){
  #. setup.VectorControlIntervention: Main structure of probabilities mosquito-control interventions
  # SurviveP: probability of surviving an encounter with the intervention
  # ProbeP: probability of successfully probing in presence of intervention
  # RepelP: probability of being repelled by intervention
  # FeedP: probability of successfully feeding in presence of intervention
  # SurviveProbeP: probability of surviving a probe attempt in presence of intervention
  # CoverageLevel: ratio of coverage in the simulated scenario
  # CoveragePattern: placeholder for different options of coverage programs
  list(
    ACTIVE=activationBool,
    SurviveP=SurvivalProbability,
    ProbeP=ProbeProbability,
    RepelP=RepelProbability,
    FeedP=FeedProbability,
    SurviveProbeP=SurviveProbeProbability,
    CoverageLevel=CoverageLevel,
    CoveragePattern="Uniform"
  )
}
setup.VectorControlPackage = function(
  #. setup.VectorControlPackage: Generates the vectors to be used in the LANDSCAPE (by default sets every probability of killing/repelling to none)
  ATSB_Parameters=setup.VectorControlIntervention(),
  AREA.REPEL_Parameters=setup.VectorControlIntervention(),
  BAITED.TRAP_Parameters=setup.VectorControlIntervention(),
  BIOLOGICAL.CONTROL_Parameters=setup.VectorControlIntervention(),
  EAVE.TUBE_Parameters=setup.VectorControlIntervention(),
  FOGGING_Parameters=setup.VectorControlIntervention(),
  FOUL.HABITAT_Parameters=setup.VectorControlIntervention(),
  GM_Parameters=setup.VectorControlIntervention(),
  IMPROVE.HOME_Parameters=setup.VectorControlIntervention(),
  IRS_Parameters=setup.VectorControlIntervention(),
  ITN_Parameters=setup.VectorControlIntervention(),
  IVERMECTIN_Parameters=setup.VectorControlIntervention(),
  LARVICIDING_Parameters=setup.VectorControlIntervention(),
  OVITRAP_Parameters=setup.VectorControlIntervention(),
  PROTECT_Parameters=setup.VectorControlIntervention(),
  REPEL_Parameters=setup.VectorControlIntervention(),
  PERSONAL.REPELLANT_Parameters=setup.VectorControlIntervention(),
  SOURCE.REDUCTION_Parameters=setup.VectorControlIntervention(),
  SWARM.SPRAYING_Parameters=setup.VectorControlIntervention(),
  SWAT_Parameters=setup.VectorControlIntervention(),
  ZOOSPRAY_Parameters=setup.VectorControlIntervention()
){
  list(
    ATSB=ATSB_Parameters,
    AREA.REPEL=AREA.REPEL_Parameters,
    BAITED.TRAP=BAITED.TRAP_Parameters,
    BIOLOGICAL.CONTROL=BIOLOGICAL.CONTROL_Parameters,
    EAVE.TUBE=EAVE.TUBE_Parameters,
    FOGGING=FOGGING_Parameters,
    FOUL.HABITAT=FOUL.HABITAT_Parameters,
    GM=GM_Parameters,
    IMPROVE.HOME=IMPROVE.HOME_Parameters,
    IRS=IRS_Parameters,
    ITN=ITN_Parameters,
    IVERMECTIN=IVERMECTIN_Parameters,
    LARVICIDING=LARVICIDING_Parameters,
    OVITRAP=OVITRAP_Parameters,
    PROTECT=PROTECT_Parameters,
    REPEL=REPEL_Parameters,
    PERSONAL.REPELLANT=PERSONAL.REPELLANT_Parameters,
    SOURCE.REDUCTION=SOURCE.REDUCTION_Parameters,
    SWARM.SPRAYING=SWARM.SPRAYING_Parameters,
    SWAT=SWAT_Parameters,
    ZOOSPRAY=ZOOSPRAY_Parameters
  )
}
