#################################################################
#
#   MASH
#   R6-ified
#   MACRO MacroTile Methods Definition
#   David Smith, Hector Sanchez, Sean Wu
#   May 11, 2016
#
#################################################################


#' MACRO \code{MacroTile} Simulate MACRO Dynamics
#'
#' write more stuff!!!!!!!!
#'
#' @param a parameter
#' @return does stuff
#' @examples
#' some_function()
simMacro <- function(tMax){
  while(private$tNow < tMax){
    private$tNow = private$tNow + 1

    private$Patches$oneDay_MacroEmerge()
    private$Patches$addCohort_MacroEmerge()

    private$MosquitoPop$oneDay_RM()
    private$MosquitoPop$layEggs() # doesnt actually do anything in Emerge

    private$HumanPop$simHumans(tPause = private$tNow)

    private$HumanPop$updateKappa()
    private$HumanPop$queueInfectiousBites()
  }
}

# assign to public method
MacroTile$set(which = "public",name = "simMacro",
          value = simMacro,
          overwrite = TRUE
)
