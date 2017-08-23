###############################################################################
#
#       ____       __
#      / __ \___  / /_  __  ______ _
#     / / / / _ \/ __ \/ / / / __ `/
#    / /_/ /  __/ /_/ / /_/ / /_/ /
#   /_____/\___/_.___/\__,_/\__, /
#                          /____/
#   MASH-CPP
#   Debugging C++ Memory through R6 Finalizers
#   Sean Wu
#   August 18, 2017
#
###############################################################################

#'  Debug Finalizers for Classes
#'
#'  Print when objects are garbage collected. For debugging purposes only.
#'
#' @export
DEBUG.MASHCPP <- function(overwrite = TRUE){

  # C++ Classes

  MASHcpp:::.R6_HumanEventQ$set(which = "public",name = "finalize",
    value = function(){message(paste0("HumanEventQ being garbage collected"))},
    overwrite = overwrite)

  MASHcpp:::.R6_EggQ$set(which = "public",name = "finalize",
    value = function(){message(paste0("EggQ being garbage collected"))},
    overwrite = overwrite)

  MASHcpp:::.R6_ImagoQ$set(which = "public",name = "finalize",
    value = function(){message(paste0("ImagoQ being garbage collected"))},
    overwrite = overwrite)

  MASHcpp:::.R6_EL4P$set(which = "public",name = "finalize",
    value = function(){message(paste0("EL4P being garbage collected"))},
    overwrite = overwrite)

  MASHcpp:::.R6_ELP$set(which = "public",name = "finalize",
    value = function(){message(paste0("ELP being garbage collected"))},
    overwrite = overwrite)

  MASHcpp:::.R6_HistoryGeneric$set(which = "public",name = "finalize",
    value = function(){message(paste0("HistoryGeneric being garbage collected"))},
    overwrite = overwrite)

  MASHcpp:::.R6_HistoryTravel$set(which = "public",name = "finalize",
    value = function(){message(paste0("HistoryTravel being garbage collected"))},
    overwrite = overwrite)

  MASHcpp:::.R6_RiskQ$set(which = "public",name = "finalize",
    value = function(){message(paste0("RiskQ being garbage collected"))},
    overwrite = overwrite)

  MASHcpp:::.R6_MosquitoFemaleHistory$set(which = "public",name = "finalize",
    value = function(){message(paste0("MosquitoFemaleHistory being garbage collected"))},
    overwrite = overwrite)

  MASHcpp:::.R6_humanPfMOI$set(which = "public",name = "finalize",
    value = function(){message(paste0("humanPfMOI being garbage collected"))},
    overwrite = overwrite)

  MASHcpp:::.R6_mosquitoPfMOI$set(which = "public",name = "finalize",
    value = function(){message(paste0("mosquitoPfMOI being garbage collected"))},
    overwrite = overwrite)

  MASHcpp:::.R6_humanPfSI$set(which = "public",name = "finalize",
    value = function(){message(paste0("humanPfSI being garbage collected"))},
    overwrite = overwrite)

  MASHcpp:::.R6_mosquitoPfSI$set(which = "public",name = "finalize",
    value = function(){message(paste0("mosquitoPfSI being garbage collected"))},
    overwrite = overwrite)

  # R6 Classes

  MASHcpp:::HashMap$set(which = "public",name = "finalize",
    value = function(){message(paste0("HashMap being garbage collected"))},
    overwrite = overwrite)

}
