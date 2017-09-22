###############################################################################
#
#       ____       __
#      / __ \___  / /_  __  ______ _
#     / / / / _ \/ __ \/ / / / __ `/
#    / /_/ /  __/ /_/ / /_/ / /_/ /
#   /_____/\___/_.___/\__,_/\__, /
#                          /____/
#   MASH-MACRO
#   Debugging Memory Management through R6 Finalizers (Destructors)
#   August 18, 2017
#
###############################################################################


#'  MASH-MACRO: Debug Methods for MACRO Objects
#'
#'  Print when objects are garbage collected and other additional methods. For debugging purposes only.
#'
#'  * Human:
#'    * finalize: print myID when garbage collected
#'
#' @export
DEBUG.MASHMACRO <- function(overwrite = TRUE){

  MASHmacro:::Human$set(which = "public",name = "finalize",
    value = function(){cat("Human: ",private$myID," has been garbage collected\n",sep="")},
    overwrite = overwrite)

  MASHmacro:::HumanPop$set(which = "public",name = "finalize",
    value = function(){cat("HumanPop in patchID: ",private$patchID," has been garbage collected\n",sep="")},
    overwrite = overwrite)

  MASHmacro:::MacroPatch$set(which = "public",name = "finalize",
    value = function(){cat("MacroPatch: ",private$patchID," has been garbage collected\n",sep="")},
    overwrite = overwrite)

  MASHmacro:::MosquitoRM$set(which = "public",name = "finalize",
    value = function(){cat("MosquitoRM in patchID: ",private$patchID," has been garbage collected\n",sep="")},
    overwrite = overwrite)

}
