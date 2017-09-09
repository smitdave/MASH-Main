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

  # MOSQUITO
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

  # SITE

  MASHmicro:::FeedingSite$set(which = "public",name = "finalize",
    value = function(){message(paste0("FeedingSite ",private$ix," being garbage collected"))},
    overwrite = overwrite
  )

  MASHmicro:::AquaticSite$set(which = "public",name = "finalize",
    value = function(){message(paste0("AquaticSite ",private$ix," being garbage collected"))},
    overwrite = overwrite
  )

  # LANDSCAPE

  MASHmicro:::Landscape$set(which = "public",name = "finalize",
    value = function(){message(paste0("Landscape being garbage collected"))},
    overwrite = overwrite
  )

  # TILE

  MASHmicro:::Tile$set(which = "public",name = "finalize",
    value = function(){message(paste0("Tile being garbage collected"))},
    overwrite = overwrite
  )

}
