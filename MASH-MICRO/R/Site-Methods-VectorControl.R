########################################################################################################
# Sugar Site Methods
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
SugarSite$set(which="public",name="get_attractiveSugarBait",value=get_attractiveSugarBait,overwrite=TRUE)

########################################################################################################
# Mating Site Methods
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

########################################################################################################
# Feeding Site Methods
########################################################################################################

########################################################################################################
# Aquatic Site Methods
########################################################################################################
