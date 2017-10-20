#################################################################
# History and Bionomic Objects
#################################################################

# #' \code{\link{MicroMosquitoFemale}} History Object
# #'
# #' Generate the history object for \code{\link{MicroMosquitoFemale}}
# #'
# #' @param stateH state trajectory
# #' @param timeH transition times
# #' @param ixH sites visited (initialized to site of emergence)
# #' @param pSetH point sets visited (initialized to site of emergence)
# #' @param feedAllH number of blood meals
# #' @param feedAllT times of blood meals
# #' @param feedHumanH number of blood meals on human hosts
# #' @param feedHumanT times of blood meals on human hosts
# #' @param feedIxH ids of all blood hosts
# #' @param bmSizeH blood meal sizes
# #' @param batchH size of egg batch
# #' @return list
# #' @examples
# #' MosquitoFemaleHistory(ixH = 0L, pSetH = "l")
# MosquitoFemaleHistory <- function(
#     stateH = NULL,
#     timeH = NULL,
#     ixH,
#     pSetH,
#     feedAllH = 0L,
#     feedAllT = NULL,
#     feedHumanH = 0L,
#     feedHumanT = NULL,
#     feedIxH = NULL,
#     bmSizeH = NULL,
#     batchH = NULL
#   ){
#     list(
#       stateH     = stateH,
#       timeH      = timeH,
#       ixH        = ixH,
#       pSetH      = pSetH,
#       feedAllH   = feedAllH,
#       feedAllT   = feedAllT,
#       feedHumanH = feedHumanH,
#       feedHumanT = feedHumanT,
#       feedIxH    = feedIxH,
#       bmSizeH    = bmSizeH,
#       batchH     = batchH
#       )
# }


# #' \code{\link{MicroMosquitoMale}} History Object
# #'
# #' Generate the history object for \code{\link{MicroMosquitoMale}}
# #'
# #' @param stateH state trajectory
# #' @param timeH transition times
# #' @param ixH sites visited (initialized to site of emergence)
# #' @param pSetH point sets visited (initialized to site of emergence)
# #' @return list
# #' @examples
# #' MosquitoMaleHistory(ixH = 0L, pSetH = "l")
# MosquitoMaleHistory <- function(
#     stateH = NULL,
#     timeH = NULL,
#     ixH,
#     pSetH
#   ){
#     list(
#       stateH     = stateH,
#       timeH      = timeH,
#       ixH        = ixH,
#       pSetH      = pSetH
#       )
# }
