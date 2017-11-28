###############################################################################
#       __  ___                      ____        __       __
#      /  |/  /___ _______________  / __ \____ _/ /______/ /_
#     / /|_/ / __ `/ ___/ ___/ __ \/ /_/ / __ `/ __/ ___/ __ \
#    / /  / / /_/ / /__/ /  / /_/ / ____/ /_/ / /_/ /__/ / / /
#   /_/  /_/\__,_/\___/_/   \____/_/    \__,_/\__/\___/_/ /_/
#
#   MASH-MACRO
#   MACRO: MacroPatch Class Definition
#   David Smith, Hector Sanchez, Sean Wu
#   August 18, 2017
#
###############################################################################

# need to have:
# addCohort_MacroPatch: a generic which can be filled by emerge or EL4P or EL4PG (EL4P with genotypes)
#
#
#
#
#
#
#
#
#

# #' MacroPatch: Run Macrosimulation for Isolated Patch
# #'
# #' Run a single \code{\link{MacroPatch}} as an isolated unit.
# #' Keep in mind that human travel must be correctly initialized for this to work.
# #'
# #' @param tMax maximum time to run simulation
# #'
# run_isolated_MacroPatch <- function(tMax){
#
#   private$tNow = 1
#
#   while(private$tNow < tMax){
#     private$MosquitoPop$run_popDynamics()
#
#     private$HumanPop$simHumans(tPause = private$tNow)
#
#     private$HumanPop$queueInfectiousBites()
#
#     private$tNow = private$tNow + 1
#   }
#
#   # close connections
# }
