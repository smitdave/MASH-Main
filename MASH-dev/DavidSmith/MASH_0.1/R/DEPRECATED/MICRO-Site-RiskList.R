# #################################################################
# #
# #   MASH
# #   R6-ified
# #   MICRO RiskList Methods (added to FeedingSite class)
# #   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
# #   April 28, 2017
# #
# #################################################################
#
#
# #################################################################
# # Initialize a risk list
# #################################################################
#
# #' MICRO \code{FeedingSite} Method: Initialize a Risk List
# #'
# #' Initialize a risk list for a feeding site object. This function is a method for \code{\link{FeedingSite}}.
# #'
# #' @param maxH number of human slots to preallocate this site's risk list
# #' @param zooW biting weight on livestock at this site's risk list
# #' @section Structure:
# #' * maxH: maximum number of humans in this risk list (memory allocation)
# #' * N: current number of humans in this risk list
# #' * who: index of who is at risk (myID value of the \code{\link{Human}} objects at risk at this site)
# #' * pTm: person time at risk
# #' * w: biting weight on hosts at risk
# #' * nO: number of non-human hosts
# #' * other: (general list structure for adding non-human hosts)
# #'    * zoo:
# #'      * w: biting weight on livestock
# #'      * typeID: -1 corresponds to livestock host id
# #' @examples
# #' some_function()
# #' @md
# init_riskList <- function(maxH = 20L, zooW = rgamma(1,1,1)){
#   riskList = list(
#     maxH = maxH,
#     N    = 0L,
#     who   = rep(0, maxH), #who: ix of hosts at risk
#     pTm   = rep(0, maxH), #pTm: person time at risk
#     w     = rep(0, maxH), #w: biting weight on hosts
#     nO = 1L,               #nO: number of other hosts
#     other = list(
#       zoo = list(
#           w = zooW,
#           typeID = -1L
#       )
#     )
#   )
#   private$riskList = riskList
# }
#
# FeedingSite$set(which = "public",name = "init_riskList",
#           value = init_riskList,
#           overwrite = TRUE
# )
#
#
# #################################################################
# # Extend a risk list
# #################################################################
#
# #' MICRO \code{FeedingSite} Method: Extend a Risk List
# #'
# #' Extend a site's risk list by one slot. This function is a method for \code{\link{FeedingSite}}.
# #'
# extend_riskList <- function(){
#   private$riskList$who  = c(private$riskList$who,0)
#   private$riskList$pTm  = c(private$riskList$pTm,0)
#   private$riskList$w    = c(private$riskList$w,0)
#   private$riskList$maxH = private$riskList$maxH + 1L
# }
#
# FeedingSite$set(which = "public",name = "extend_riskList",
#           value = extend_riskList,
#           overwrite = TRUE
# )
#
#
# #################################################################
# # Add a human to a risk list
# #################################################################
#
# #' MICRO \code{FeedingSite} Method: Add a Person's Risk to a Risk List
# #'
# #' Add risk information for a single human to a site's risk list. This function is a method for \code{\link{FeedingSite}}.
# #'
# #' @param who integer index of this \code{\link{Human}} myID slot
# #' @param pTm this human's time at risk
# #' @param w this human's biting weight
# add_riskList <- function(who, pTm, w){
#
#   if(who %in% private$riskList$who){
#     riskIx = match(x = who,table = private$riskList$who)
#   } else {
#     riskIx = private$riskList$N + 1
#     if(riskIx > private$riskList$maxH){
#       self$extend_riskList()
#     }
#   }
#
#   private$riskList$N           = riskIx   #update number of humans
#   private$riskList$pTm[riskIx] = pTm    #update time at risk
#   private$riskList$w[riskIx]   = w      #update biting weights
#   private$riskList$who[riskIx] = who    #update who is at risk
# }
#
# FeedingSite$set(which = "public",name = "add_riskList",
#           value = add_riskList,
#           overwrite = TRUE
# )
