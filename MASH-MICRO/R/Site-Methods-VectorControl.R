########################################################################################################
########################################################################################################
# Sugar Site Methods
########################################################################################################
########################################################################################################

#' Set attractiveSugarBait
#'
#' Change the definition of the ATSB in the sugar site
#'
#'
#' @param attractiveSugarBait Object
#'
set_attractiveSugarBait=function(atsbDefinition){private$attractiveSugarBait=atsbDefinition}
SugarSite$set(which="public",name="set_attractiveSugarBait",value=set_attractiveSugarBait,overwrite=TRUE)

#' Get attractiveSugarBait
#'
#' Return the ATSB in the sugar site
#'
#'
#' @param attractiveSugarBait Object
#'
get_attractiveSugarBait=function(){return(private$attractiveSugarBait)}
SugarSite$set(which="public",name="get_attractiveSugarBait",value=get_attractiveSugarBait,overwrite=TRUE
)
########################################################################################################
########################################################################################################
# Mating Site Methods
########################################################################################################
########################################################################################################

#' Set aerialSpray
#'
#' Change the definition of the Aerial Spray
#'
#'
#' @param aerialSpray Object
#'
set_aerialSpray=function(aerialSprayDefinition){private$aerialSpray=aerialSprayDefinition}
MatingSite$set(which="public",name="set_aerialSpray",value=set_aerialSpray,overwrite=TRUE)

#' Get aerialSpray
#'
#' Return the definition of the Aerial Spray
#'
#'
#' @param aerialSpray Object
#'
get_aerialSpray=function(){return(private$aerialSpray)}
MatingSite$set(which="public",name="get_aerialSpray",value=get_aerialSpray,overwrite=TRUE)

######################################################

#' Set swarmSpray
#'
#' Change the definition of the Aerial Spray
#'
#'
#' @param swarmSpray Object
#'
set_swarmSpray=function(swarmSprayDefinition){private$swarmSpray=swarmSprayDefinition}
MatingSite$set(which="public",name="set_swarmSpray",value=set_swarmSpray,overwrite=TRUE)

#' Get swarmSpray
#'
#' Return the definition of the Swarm Spray
#'
#'
#' @param swarmSpray Object
#'
get_swarmSpray=function(){return(private$swarmSpray)}
MatingSite$set(which="public",name="get_swarmSpray",value=get_swarmSpray,overwrite=TRUE)

# ######################################################
#
# #' Set areaRepellent
# #'
# #' Change the definition of the Area Repellent
# #'
# #'
# #' @param areaRepellent Object
# #'
# set_areaRepellent=function(areaRepellentDefinition){private$areaRepellent=areaRepellentDefinition}
# MatingSite$set(which="public",name="set_areaRepellent",value=set_areaRepellent,overwrite=TRUE)
#
# #' Get swarmSpray
# #'
# #' Return the definition of the Swarm Spray
# #'
# #'
# #' @param swarmSpray Object
# #'
# get_areaRepellent=function(){return(private$areaRepellent)}
# MatingSite$set(which="public",name="get_areaRepellent",value=get_areaRepellent,overwrite=TRUE)

########################################################################################################
########################################################################################################
# Feeding Site Methods
########################################################################################################
########################################################################################################

#' Set swarmSpray
#'
#' Change the definition of the Aerial Spray
#'
#'
#' @param swarmSpray Object
#'
set_odorBaitedTrap=function(odorBaitedTrapDefinition){private$odorBaitedTrap=odorBaitedTrapDefinition}
FeedingSite$set(which="public",name="set_odorBaitedTrap",value=set_odorBaitedTrap,overwrite=TRUE)

#' Get swarmSpray
#'
#' Return the definition of the Swarm Spray
#'
#'
#' @param swarmSpray Object
#'
get_odorBaitedTrap=function(){return(private$odorBaitedTrap)}
FeedingSite$set(which="public",name="get_odorBaitedTrap",value=get_odorBaitedTrap,overwrite=TRUE)

######################################################

#' Set swarmSpray
#'
#' Change the definition of the Aerial Spray
#'
#'
#' @param swarmSpray Object
#'
set_eaveTube=function(eaveTubeDefinition){private$eaveTube=eaveTubeDefinition}
FeedingSite$set(which="public",name="set_eaveTube",value=set_eaveTube,overwrite=TRUE)

#' Get swarmSpray
#'
#' Return the definition of the Swarm Spray
#'
#'
#' @param swarmSpray Object
#'
get_eaveTube=function(){return(private$eaveTube)}
FeedingSite$set(which="public",name="get_eaveTube",value=get_eaveTube,overwrite=TRUE)

######################################################

########################################################################################################
########################################################################################################
# Aquatic Site Methods
########################################################################################################
########################################################################################################

#' Set swarmSpray
#'
#' Change the definition of the Aerial Spray
#'
#'
#' @param swarmSpray Object
#'
set_ovitrap=function(ovitrapDefinition){private$ovitrap=ovitrapDefinition}
AquaticSite$set(which="public",name="set_ovitrap",value=set_ovitrap,overwrite=TRUE)

#' Get swarmSpray
#'
#' Return the definition of the Swarm Spray
#'
#'
#' @param swarmSpray Object
#'
get_ovitrap=function(){return(private$ovitrap)}
AquaticSite$set(which="public",name="get_ovitrap",value=get_ovitrap,overwrite=TRUE)

######################################################

#' Set swarmSpray
#'
#' Change the definition of the Aerial Spray
#'
#'
#' @param swarmSpray Object
#'
set_larviciding=function(larvicidingDefinition){private$larviciding=larvicidingDefinition}
AquaticSite$set(which="public",name="set_larviciding",value=set_larviciding,overwrite=TRUE)

#' Get swarmSpray
#'
#' Return the definition of the Swarm Spray
#'
#'
#' @param swarmSpray Object
#'
get_larviciding=function(){return(private$larviciding)}
AquaticSite$set(which="public",name="get_larviciding",value=get_larviciding,overwrite=TRUE)

######################################################

#' Set swarmSpray
#'
#' Change the definition of the Aerial Spray
#'
#'
#' @param swarmSpray Object
#'
set_sourceReduction=function(sourceReductionDefinition){private$sourceReduction=sourceReductionDefinition}
AquaticSite$set(which="public",name="set_sourceReduction",value=set_sourceReduction,overwrite=TRUE)

#' Get swarmSpray
#'
#' Return the definition of the Swarm Spray
#'
#'
#' @param swarmSpray Object
#'
get_sourceReduction=function(){return(private$sourceReduction)}
AquaticSite$set(which="public",name="get_sourceReduction",value=get_sourceReduction,overwrite=TRUE)

######################################################

#' Set swarmSpray
#'
#' Change the definition of the Aerial Spray
#'
#'
#' @param swarmSpray Object
#'
set_biologicalControl=function(biologicalControlDefinition){private$biologicalControl=biologicalControlDefinition}
AquaticSite$set(which="public",name="set_biologicalControl",value=set_biologicalControl,overwrite=TRUE)

#' Get swarmSpray
#'
#' Return the definition of the Swarm Spray
#'
#'
#' @param swarmSpray Object
#'
get_biologicalControl=function(){return(private$biologicalControl)}
AquaticSite$set(which="public",name="get_biologicalControl",value=get_biologicalControl,overwrite=TRUE)
