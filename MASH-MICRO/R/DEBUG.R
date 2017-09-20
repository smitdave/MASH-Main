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
    value = function(){message(paste0("MosquitoMale" ,private$id," being garbage collected"))},
    overwrite = overwrite
  )

  MASHmicro:::MosquitoPopFemale$set(which = "public",name = "finalize",
    value = function(){message(paste0("MosquitoPopFemale being garbage collected"))},
    overwrite = overwrite
  )

  MASHmicro:::MosquitoPopMale$set(which = "public",name = "finalize",
    value = function(){message(paste0("MosquitoPopMale being garbage collected"))},
    overwrite = overwrite
  )

  MASHmicro:::MosquitoPopFemale$set(which = "public",name = "test_WriteHistoryAndDelete",
    value = test_WriteHistoryAndDelete_MosquitoPopFemale,
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

  MASHmicro:::MatingSite$set(which = "public",name = "finalize",
    value = function(){message(paste0("MatingSite ",private$ix," being garbage collected"))},
    overwrite = overwrite
  )

  MASHmicro:::SugarSite$set(which = "public",name = "finalize",
    value = function(){message(paste0("SugarSite ",private$ix," being garbage collected"))},
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


#'  Debug: Write Out Female Histories and Delete
#'
#'  Testing writing out all female histories to JSON and then having each female delete herself from the MASHcpp::HashMap class.
#'    * This method is bound to \code{MosquitoPopFemale$test_WriteHistoryAndDelete}
test_WriteHistoryAndDelete_MosquitoPopFemale <- function(){
   con = file(description = paste0(private$TilePointer$get_MosquitoDirectory(),"testHistory.json"),open = "wt")
   writeLines(text = "[",con = con)
   private$pop$apply(tag="writeAndDelete",returnVal=FALSE,con=con)
   writeLines(text = "]",con = con)
   close(con);rm(con)
}
