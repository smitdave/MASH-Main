###############################################################################
#
#       ____       __
#      / __ \___  / /_  __  ______ _
#     / / / / _ \/ __ \/ / / / __ `/
#    / /_/ /  __/ /_/ / /_/ / /_/ /
#   /_____/\___/_.___/\__,_/\__, /
#                          /____/
#
#   MASH-CPP
#   Debugging C++ Memory through R6 Finalizers
#   August 18, 2017
#
###############################################################################

DEBUG.MASHCPP <- function(overwrite = TRUE){

  HumanEventQ$set(which = "public",name = "finalize",
    value = function(){print(paste0("HumanEventQ being garbage collected"))},
    overwrite = overwrite)

  EggQ$set(which = "public",name = "finalize",
    value = function(){print(paste0("EggQ being garbage collected"))},
    overwrite = overwrite)

  ImagoQ$set(which = "public",name = "finalize",
    value = function(){print(paste0("ImagoQ being garbage collected"))},
    overwrite = overwrite)

  EL4P$set(which = "public",name = "finalize",
    value = function(){print(paste0("EL4P being garbage collected"))},
    overwrite = overwrite)

  HistoryGeneric$set(which = "public",name = "finalize",
    value = function(){print(paste0("HistoryGeneric being garbage collected"))},
    overwrite = overwrite)

  HistoryTravel$set(which = "public",name = "finalize",
    value = function(){print(paste0("HistoryTravel being garbage collected"))},
    overwrite = overwrite)

  RiskQ$set(which = "public",name = "finalize",
    value = function(){print(paste0("RiskQ being garbage collected"))},
    overwrite = overwrite)

  MosquitoFemaleHistory$set(which = "public",name = "finalize",
    value = function(){print(paste0("MosquitoFemaleHistory being garbage collected"))},
    overwrite = overwrite)

  humanPfMOI$set(which = "public",name = "finalize",
    value = function(){print(paste0("humanPfMOI being garbage collected"))},
    overwrite = overwrite)

  mosquitoPfMOI$set(which = "public",name = "finalize",
    value = function(){print(paste0("mosquitoPfMOI being garbage collected"))},
    overwrite = overwrite)

  humanPfSI$set(which = "public",name = "finalize",
    value = function(){print(paste0("humanPfSI being garbage collected"))},
    overwrite = overwrite)

  mosquitoPfSI$set(which = "public",name = "finalize",
    value = function(){print(paste0("mosquitoPfSI being garbage collected"))},
    overwrite = overwrite)

}
