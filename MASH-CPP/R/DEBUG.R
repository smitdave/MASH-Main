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
    value = function(){cat("HumanEventQ being garbage collected\n",sep="")},
    overwrite = overwrite)

  MASHcpp:::.R6_EggQ$set(which = "public",name = "finalize",
    value = function(){cat("EggQ being garbage collected\n",sep="")},
    overwrite = overwrite)

  MASHcpp:::.R6_ImagoQ$set(which = "public",name = "finalize",
    value = function(){cat("ImagoQ being garbage collected\n",sep="")},
    overwrite = overwrite)

  MASHcpp:::.R6_EL4P$set(which = "public",name = "finalize",
    value = function(){cat("EL4P being garbage collected\n",sep="")},
    overwrite = overwrite)

  MASHcpp:::.R6_ELP$set(which = "public",name = "finalize",
    value = function(){cat("ELP being garbage collected\n",sep="")},
    overwrite = overwrite)

  MASHcpp:::.R6_HistoryGeneric$set(which = "public",name = "finalize",
    value = function(){cat("HistoryGeneric being garbage collected\n",sep="")},
    overwrite = overwrite)

  MASHcpp:::.R6_HistoryTravel$set(which = "public",name = "finalize",
    value = function(){cat("HistoryTravel being garbage collected\n",sep="")},
    overwrite = overwrite)

  MASHcpp:::.R6_RiskQ$set(which = "public",name = "finalize",
    value = function(){cat("RiskQ being garbage collected\n",sep="")},
    overwrite = overwrite)

  MASHcpp:::.R6_MosquitoFemaleHistory$set(which = "public",name = "finalize",
    value = function(){cat("MosquitoFemaleHistory being garbage collected\n",sep="")},
    overwrite = overwrite)

  MASHcpp:::.R6_humanPfMOI$set(which = "public",name = "finalize",
    value = function(){cat("humanPfMOI being garbage collected\n",sep="")},
    overwrite = overwrite)

  MASHcpp:::.R6_mosquitoPfMOI$set(which = "public",name = "finalize",
    value = function(){cat("mosquitoPfMOI being garbage collected\n",sep="")},
    overwrite = overwrite)

  MASHcpp:::.R6_humanPfSI$set(which = "public",name = "finalize",
    value = function(){cat("humanPfSI being garbage collected\n",sep="")},
    overwrite = overwrite)

  MASHcpp:::.R6_mosquitoPfSI$set(which = "public",name = "finalize",
    value = function(){cat("mosquitoPfSI being garbage collected\n",sep="")},
    overwrite = overwrite)

  # R6 Classes

  MASHcpp:::HashMap$set(which = "public",name = "finalize",
    value = function(){cat("HashMap being garbage collected\n",sep="")},
    overwrite = overwrite)

}
