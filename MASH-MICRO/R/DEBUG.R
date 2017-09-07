###############################################################################
#
#       ____       __
#      / __ \___  / /_  __  ______ _
#     / / / / _ \/ __ \/ / / / __ `/
#    / /_/ /  __/ /_/ / /_/ / /_/ /
#   /_____/\___/_.___/\__,_/\__, /
#                          /____/
#   MASH-CPP
#   Debugging
#   Sean Wu
#   August 18, 2017
#
###############################################################################

#'  Debug Finalizers for Classes
#'
#'  Print when objects are garbage collected. For debugging purposes only.
#'
#' @export
DEBUG.MASHMICRO <- function(overwrite = TRUE){

  # R6 Classes

  MASHmicro:::MosquitoFemale$set(which = "public",name = "finalize",
    value = function(){message(paste0("MosquitoFemale" ,private$id," being garbage collected"))},
    overwrite = overwrite
  )

  MASHmicro:::MosquitoMale$set(which = "public",name = "finalize",
    value = function(){message(paste0("MosquitoFemale" ,private$id," being garbage collected"))},
    overwrite = overwrite
  )

  MASHmicro:::MosquitoPopFemale$set(which = "public",name = "finalize",
    value = function(){message(paste0("MosquitoPopFemale being garbage collected"))},
    overwrite = overwrite
  )

  MASHmicro:::MosquitoPopMale$set(which = "public",name = "finalize",
    value = function(){message(paste0("MosquitoPopFemale being garbage collected"))},
    overwrite = overwrite
  )
  
}
