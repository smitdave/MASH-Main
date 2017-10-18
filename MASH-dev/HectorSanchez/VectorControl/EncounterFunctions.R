####################################################################################################
# MASH Vector Control
# HMSC
####################################################################################################

#' ATSB landscape-mosquito interaction.
#'
#' This function is intented to serve as an interface between landscape and mosquitoes feeding on sugar baits.
#' Tests if the current sugar site contains an ATSB and, if so, applies ATSB-related functions.
#' Acts in boutS.
#'
#' @param M Mosquito
#' @return Mosquito with updated parameters
#' @examples
#' fATSB(M)
fATSB=function(M){
  #. fATSB: Interaction between mosquito and attractive sugar bait in landscape (MBITES)
  if(is.null(LANDSCAPE$sugarSites[[M$ix]]$ATSB)==FALSE){
    M=LANDSCAPE$sugarSites[[M$ix]]$ATSB$mosquitoKillEncounter(M,"ATSB")
  }
  return(M)
}

#' Swarm Spraying landscape-mosquito interaction.
#'
#' This function is intented to serve as an interface between landscape and mosquitoes in the presence of swarm sray.
#' Tests if the current mating site contains swarm spray and, if so, applies Swarm Spray-related functions.
#' Acts in boutMF
#'
#' @param M Mosquito
#' @return Mosquito with updated parameters
#' @examples
#' fSwarmSpraying(M)
fSwarmSpraying=function(M){
  #. fATSB: Interaction between mosquito and attractive swarm spray in landscape (MBITES)
  if(is.null(LANDSCAPE$swarmSites[[M$ix]]$SwarmSpray)==FALSE){
    M=LANDSCAPE$swarmSites[[M$ix]]$SwarmSpray$mosquitoKillEncounter(M,"SwarmSpray")
  }
  return(M)
}

#' Ovitrap landscape-mosquito interaction.
#'
#' This function is intented to serve as an interface between landscape and mosquitoes egg-laying in ovitraps.
#' Tests if the current oviposition site contains an ovitrap and, if so, applies ovitrap-related functions.
#' Acts in boutO
#'
#' @param M Mosquito
#' @return Mosquito with updated parameters
#' @examples
#' fOvitrap(M)
fOvitrap=function(M){
  #. fOvitrap; Interaction between adult mosquito and ovitrap (MBITES)
  if(is.null(LANDSCAPE$aquaSites[[M$ix]]$Ovitrap)==FALSE){
    M=LANDSCAPE$aquaSites[[M$ix]]$Ovitrap$mosquitoKillEncounter(M,"Ovitrap")
  }
  return(M)
}

#' Odor-Baited Trap landscape-mosquito interaction.
#'
#' This function is intented to serve as an interface between landscape and mosquitoes blood-feeding on odor-baited trap.
#' Tests if the current feeding site contains an odor-baited trap and, if so, applies odor-baited trap-related functions.
#' Acts in _____
#'
#' @param M Mosquito
#' @return Mosquito with updated parameters
#' @examples
#' fOdorBaitedTrap(M)
fOdorBaitedTrap=function(M){
  #. fOdorBaitedTrap: Interaction between mosquito and odor baited trap in landscape
  if(is.null(LANDSCAPE$feedingSites[[M$ix]]$OdorBaitedTrap)==FALSE){
    M=LANDSCAPE$feedingSites[[M$ix]]$OdorBaitedTrap$mosquitoKillEncounter(M)
  }
  return(M)
}

#' IRS landscape-mosquito interaction.
#'
#' This function is intented to serve as an interface between landscape and mosquitoes resting on IRS.
#' Tests if the current feeding site contains an IRS and, if so, applies IRS-related functions.
#' Acts in boutGeneric
#'
#' @param M Mosquito
#' @return Mosquito with updated parameters
#' @examples
#' fIRS(M)
fIRS=function(M){
  #. fIRS: Interaction between mosquito and indoor wall landing spot
  if(M$lspot=="i" && M$inPointSet=="f"){
    #cat(M$lspot,M$ix,M$inPointSet,"\n")
    treatedWithIRS=(is.null(LANDSCAPE$feedSites[[M$ix]]$IRS)==FALSE)
    if(treatedWithIRS){
      irs=LANDSCAPE$feedSites[[M$ix]]$IRS
      M=irs$mosquitoKillEncounter(M,irs$getName())
      M=irs$mosquitoRepelEncounter(M,irs$getName())
    }
  }
  return(M)
}

#' PersonalRepellant human-mosquito interaction.
#'
#' This function is intented to serve as an interface between humans and mosquitoes attempting to feed on personal-repellant treated persons.
#' Tests if the current host is using personal repellant and, if so, applies personal repellant-related functions.
#' Acts in humanEncounter
#'
#' @param M Mosquito
#' @return List of bools (in order): repelled, killed, probe deterred, probeKilled
#' @examples
#' fPersonalRepellant(M)
fPersonalRepellant=function(M){
  #. fPersonalRepellant: Interaction between mosquito and personal repellant in humans
  hostIsWearingPR=(is.null(HUMANS[[M$hostID]]$PR)==FALSE)
  if(hostIsWearingPR){
    pr=HUMANS[[M$hostID]]$PersonalRepellant
    repelled=pr$mosquitoRepelBinomialEvent()
    killed=pr$mosquitoKillBinomialEvent()
    probeDeterred=pr$mosquitoProbeDeterBinomialEvent()
    probeKilled=pr$mosquitoProbeKillBinomialEvent()
    return(list(repelled,killed,probeDeterred,probeKilled))
  }else{
    return(list(FALSE,FALSE,FALSE,FALSE))
  }
}

#' ITN human-mosquito interaction.
#'
#' This function is intented to serve as an interface between humans and mosquitoes attempting to feed on ITN users.
#' Tests if the current host is using an ITN and, if so, applies ITN-related functions.
#' Acts in humanEncounter
#'
#' @param M Mosquito
#' @return List of bools (in order): repelled, killed, probe deterred, probeKilled
#' @examples
#' fITN(M)
fITN=function(M){
  #. fITN: Interaction between mosquito and ITN
  hostIsWearingITN=(is.null(HUMANS[[M$hostID]]$ITN)==FALSE)
  if(hostIsWearingITN){
    itn=HUMANS[[M$hostID]]$ITN
    repelled=itn$mosquitoRepelBinomialEvent()
    killed=itn$mosquitoKillBinomialEvent()
    probeDeterred=itn$mosquitoProbeDeterBinomialEvent()
    probeKilled=itn$mosquitoProbeKillBinomialEvent()
    return(list(repelled,killed,probeDeterred,probeKilled))
  }else{
    return(list(FALSE,FALSE,FALSE,FALSE))
  }
}

#' Swat human-mosquito interaction.
#'
#' This function is intented to serve as an interface between humans and mosquitoes attempting to feed on swatter users.
#' Tests if the current host is using a swatter and, if so, applies swat-related functions.
#' Acts in humanEncounter
#'
#' @param M Mosquito
#' @return List of bools (in order): repelled, killed, probe deterred, probeKilled
#' @examples
#' fSwat(M)
fSwat=function(M){
  #. fSwat: Interaction between mosquito and swatter
  hostHasSwatter=(is.null(HUMANS[[M$hostID]]$Swat)==FALSE)
  if(hostHasSwatter){
    swat=HUMANS[[M$hostID]]$Swat
    repelled=swat$mosquitoRepelBinomialEvent()
    killed=swat$mosquitoKillBinomialEvent()
    probeDeterred=swat$mosquitoProbeDeterBinomialEvent()
    probeKilled=swat$mosquitoProbeKillBinomialEvent()
    return(list(repelled,killed,probeDeterred,probeKilled))
  }else{
    return(list(FALSE,FALSE,FALSE,FALSE))
  }
}

#' Eave Tube landscape-mosquito interaction.
#'
#' This function is intented to serve as an interface between landscape and mosquitoes attempting to enter a house with eave tubes.
#' Tests if the current feeding site is equiped with eave tubes and, if so, applies eave tube-related functions.
#' Acts in enterHouse
#'
#' @param M Mosquito
#' @return Mosquito with updated parameters
#' @examples
#' fEaveTube(M)
fEaveTube=function(M){
  #. fEaveTub: Interaction between eave tube and mosquito
  eaveTubePresent=(is.null(LANDSCAPE$feedSites[[M$ix]]$EaveTube)==FALSE)
  if(eaveTubePresent){
    eave=LANDSCAPE$feedSites[[M$ix]]$EaveTube
    M=eave$mosquitoKillEncounter(M,"EaveTuve")
    M=eave$mosquitoRepelEncounter(M,"EaveTuve")
  }
  return(M)
}

#' Improved Home landscape-mosquito interaction.
#'
#' This function is intented to serve as an interface between landscape and mosquitoes attempting to enter a house with home improvements.
#' Tests if the current feeding site is equiped with home improvements and, if so, applies home improvement-related functions.
#' Acts in enterHouse
#'
#' @param M Mosquito
#' @return Mosquito with updated parameters
#' @examples
#' fImproveHome(M)
fImproveHome=function(M){
  #. fImproveHome: Interaction between an improved home and mosquito
  improveHomePresent=(is.null(LANDSCAPE$feedSites[[M$ix]]$ImproveHome)==FALSE)
  if(improveHomePresent){
    iprh=LANDSCAPE$feedSites[[M$ix]]$ImproveHome
    M=iprh$mosquitoKillEncounter(M,"ImproveHome")
    M=iprh$mosquitoRepelEncounter(M,"ImproveHome")
  }
  return(M)
}

#' Ivermectin cattle-mosquito interaction.
#'
#' This function is intented to serve as an interface between cattle and mosquitoes attempting to feed on treated cattle.
#' Tests if the current cattle host was treatted with ivermectin and, if so, applies ivermectin-related functions.
#' Acts in zooEncounter
#'
#' @param M Mosquito
#' @return List of bools (in order): repelled, killed
#' @examples
#' fIvermectin(M)
fIvermectin=function(M){
  #. fIvermectin: Interaction between mosquito and ivermectin treated cattle
  if(is.null(LANDSCAPE$feedSites[[M$ix]]$Ivermectin)==FALSE){
    iver=LANDSCAPE$feedSites[[M$ix]]$Ivermectin
    repelled=iver$mosquitoRepelBinomialEvent()
    killed=iver$mosquitoKillBinomialEvent()
    #probeDeterred=iver$mosquitoProbeDeterBinomialEvent()
    #probeKilled=iver$mosquitoProbeKillBinomialEvent()
    return(list(repelled,killed))#,probeDeterred,probeKilled))
  }else{
    return(list(FALSE,FALSE))#,FALSE,FALSE))
  }
}

#' Zoospray cattle-mosquito interaction.
#'
#' This function is intented to serve as an interface between cattle and mosquitoes attempting to feed on treated cattle.
#' Tests if the current cattle host was treatted with zoospray and, if so, applies zoospray-related functions.
#' Acts in zooEncounter
#'
#' @param M Mosquito
#' @return List of bools (in order): repelled, killed
#' @examples
#' fZoospray(M)
fZoospray=function(M){
  #. fZoospray: Interaction between mosquito and ivermectin treated cattle
  if(is.null(LANDSCAPE$feedSites[[M$ix]]$Zoospray)==FALSE){
    zoo=LANDSCAPE$feedSites[[M$ix]]$Zoospray
    repelled=zoo$mosquitoRepelBinomialEvent()
    killed=zoo$mosquitoKillBinomialEvent()
    #probeDeterred=zoo$mosquitoProbeDeterBinomialEvent()
    #probeKilled=zoo$mosquitoProbeKillBinomialEvent()
    return(list(repelled,killed))#,probeDeterred,probeKilled))
  }else{
    return(list(FALSE,FALSE))#,FALSE,FALSE))
  }
}

#' Aerial Spray landscape-mosquito interaction.
#'
#' This function is intented to serve as an interface between cattle and mosquitoes attempting to feed on treated cattle.
#' Tests if the current area was sprayed and, if so, applies home aerial spray-related functions.
#' Acts in boutGeneric
#'
#' @param M Mosquito
#' @return Mosquito with updated parameters
#' @examples
#' fAerialSpray(M)
fAerialSpray=function(M){
  #. fAerialSpray: Interaciton between mosquito and aerial spray
  if(M$lspot=="w" || M$lspot=="v"){
    if(M$inPointSet=="f"){
      if(is.null(LANDSCAPE$feedSites[[M$ix]]$AerialSpray)==FALSE){
        M=LANDSCAPE$feedSites[[M$ix]]$AerialSpray$mosquitoKillEncounter(M,"AerialSpray")
        M=LANDSCAPE$feedSites[[M$ix]]$AerialSpray$mosquitoRepelEncounter(M,"AerialSpray")
      }
    }
    if(M$inPointSet=="s"){
      if(is.null(LANDSCAPE$sugarSites[[M$ix]]$AerialSpray)==FALSE){
        M=LANDSCAPE$sugarSites[[M$ix]]$AerialSpray$mosquitoKillEncounter(M,"AerialSpray")
        M=LANDSCAPE$sugarSites[[M$ix]]$AerialSpray$mosquitoRepelEncounter(M,"AerialSpray")
      }
    }
    if(M$inPointSet=="l"){
      if(is.null(LANDSCAPE$aquaSites[[M$ix]]$AerialSpray)==FALSE){
        M=LANDSCAPE$aquaSites[[M$ix]]$AerialSpray$mosquitoKillEncounter(M,"AerialSpray")
        M=LANDSCAPE$aquaSites[[M$ix]]$AerialSpray$mosquitoRepelEncounter(M,"AerialSpray")
      }
    }
    if(M$inPointSet=="m"){
      if(is.null(LANDSCAPE$swarmSites[[M$ix]]$AerialSpray)==FALSE){
        M=LANDSCAPE$swarmSites[[M$ix]]$AerialSpray$mosquitoKillEncounter(M,"AerialSpray")
        M=LANDSCAPE$swarmSites[[M$ix]]$AerialSpray$mosquitoRepelEncounter(M,"AerialSpray")
      }
    }
  }
  return(M)
}
