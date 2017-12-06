###############################################################################
#
#       __  ____  ____  ______    _   __
#      / / / / / / /  |/  /   |  / | / /
#     / /_/ / / / / /|_/ / /| | /  |/ /
#    / __  / /_/ / /  / / ___ |/ /|  /
#   /_/ /_/\____/_/  /_/_/  |_/_/ |_/
#
#   MASH-MACRO
#   Human Setup
#   MASH Team
#   December 2017
#
###############################################################################


#' MACRO Human Module Setup
#'
#' Initialize movement and biting modules for MACRO Humans.
#'
#' @param pathogen pathogen model
#' @param tripFrequency queues trips in \code{\link{initialize_travel_Human}} and \code{\link{returnHome}}
#' @param tripDuration queues return home events in \code{\link{takeTrip}}
#'
#' @export
MACRO.Human.Setup <- function(pathogen = "PfSI", tripFrequency = 1/365, tripDuration = 14){

  cat("initializing MACRO Human Kappa & Move Modules\n")

  # Kappa

  # bWeight
  HumanPop$set(which = "public",name = "initialize_bWeightHuman",
            value = initialize_bWeightHuman_HumanPop, overwrite = TRUE
  )

  Human$set(which = "public",name = "accumulate_bWeightHuman",
            value = accumulate_bWeightHuman_Human, overwrite = TRUE
  )

  Human$set(which = "public",name = "decrement_bWeightHuman",
            value = decrement_bWeightHuman_Human, overwrite = TRUE
  )

  # kappa
  switch(pathogen,
    PfSI = {
      Human$set(which = "public",name = "updateKappa",
                value = updateKappa_PfSI_Human, overwrite = TRUE
      )
    },
    PfMOI = {
      Human$set(which = "public",name = "updateKappa",
                value = updateKappa_PfMOI_Human, overwrite = TRUE
      )
    },
    {stop("unrecognized Pathogen module selected")}
  )

  HumanPop$set(which = "public",name = "updateKappa",
            value = updateKappa_HumanPop, overwrite = TRUE
  )

  # EIR
  Human$set(which = "public",name = "updateEIR",
            value = updateEIR_Human, overwrite = TRUE
  )

  HumanPop$set(which = "public",name = "updateEIR",
            value = updateEIR_HumanPop, overwrite = TRUE
  )

  Human$set(which = "public",name = "get_EIR",
            value = get_EIR_Human, overwrite = TRUE
  )

  Human$set(which = "public",name = "set_EIR",
            value = set_EIR_Human, overwrite = TRUE
  )

  # queue bites
  switch(pathogen,
    PfSI = {
      Human$set(which = "public",name = "queueInfectiousBites",
                value = queueInfectiousBites_PfSI_Human, overwrite = TRUE
      )
    },
    PfMOI = {
      Human$set(which = "public",name = "queueInfectiousBites",
                value = queueInfectiousBites_PfMOI_Human, overwrite = TRUE
      )
    },
    {stop("unrecognized Pathogen module selected")}
  )

  HumanPop$set(which = "public",name = "queueInfectiousBites",
            value = queueInfectiousBites_HumanPop, overwrite = TRUE
  )

  # Move

  # output
  HumanPop$set(which = "public",name = "initialize_output_Move",
            value = initialize_output_Move_HumanPop, overwrite = TRUE
  )

  # trip frequency
  Human$set(which = "private",name = "tripFrequency",
            value = tripFrequency, overwrite = TRUE
  )

  # trip duration
  Human$set(which = "private",name = "tripDuration",
            value = tripDuration, overwrite = TRUE
  )

  HumanPop$set(which = "public",name = "initialize_travel",
            value = initialize_travel_HumanPop, overwrite = TRUE
  )

  Human$set(which = "public",name = "initialize_travel",
            value = initialize_travel_Human, overwrite = TRUE
  )

  Human$set(which = "public",name = "add2Q_takeTrip",
            value = add2Q_takeTrip, overwrite = TRUE
  )

  Human$set(which = "public",name = "event_takeTrip",
            value = event_takeTrip, overwrite = TRUE
  )

  Human$set(which = "public",name = "takeTrip",
            value = takeTrip, overwrite = TRUE
  )

  Human$set(which = "public",name = "add2Q_returnHome",
            value = add2Q_returnHome, overwrite = TRUE
  )

  Human$set(which = "public",name = "event_returnHome",
            value = event_returnHome, overwrite = TRUE
  )

  Human$set(which = "public",name = "returnHome",
            value = returnHome, overwrite = TRUE
  )

}
