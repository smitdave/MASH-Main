# #################################################################
# #
# #   MASH
# #   R6-ified
# #   Aquatic Ecology for Sites:
# #   Generic Queue Structures and Management for Aquatic Habitats
# #   Hector Sanchez & David Smith, Hector Sanchez, Sean Wu
# #   May 10, 2017
# #
# #################################################################
#
#
# #################################################################
# # SETUP
# #################################################################
#
# #' Initialize Aquatic Ecology COMPONENT
# #'
# #' This function initializes generic methods and fields for the Aquatic Ecology COMPONENT.
# #' It modifies \code{\link{AquaticSite}} and \code{\link{Landscape}} classes.
# #'
# #' @param module character
# #' @return stuff
# #' @examples
# #' MICRO.Aqua.Setup()
# #' @export
# MICRO.Aqua.Setup <- function(module = "emerge", overwrite = TRUE){
#
#   message("initializing Aquatic Ecology component queue methods")
#
#   #################################################################
#   # Egg Queue: only used in EL4P
#   #################################################################
#
#   if(module=="EL4P"){
#
#     message("initializing EL4P module queue methods")
#
#     # clear the EggQ
#     AquaticSite$set(which = "public",name = "clear_EggQ",
#               value = clear_MicroEggQ,
#               overwrite = overwrite
#     )
#
#     # extend the EggQ
#     AquaticSite$set(which = "public",name = "extend_EggQ",
#               value = extend_MicroEggQ,
#               overwrite = overwrite
#     )
#
#     # add a egg batch to the EggQ
#     AquaticSite$set(which = "public",name = "add_EggQ",
#               value = add_MicroEggQ,
#               overwrite = overwrite
#     )
#
#     # zeroBatch: zero out an egg batch in EggQ
#     AquaticSite$set(which = "public",name = "zero_EggQ",
#               value = zero_MicroEggQ,
#               overwrite = overwrite
#     )
#
#     # get indices of full slots: return 0 if none
#     AquaticSite$set(which = "public",name = "full_EggQ",
#               value = full_MicroEggQ,
#               overwrite = overwrite
#     )
#
#     # get indices of empty slots: return 0 if none
#     AquaticSite$set(which = "public",name = "empty_EggQ",
#               value = empty_MicroEggQ,
#               overwrite = overwrite
#     )
#
#     # modifiers & accessors
#     AquaticSite$set(which = "public",name = "get_EggQ",
#               value = get_MicroEggQ,
#               overwrite = overwrite
#     )
#     AquaticSite$set(which = "public",name = "set_EggQ",
#               value = set_MicroEggQ,
#               overwrite = overwrite
#     )
#
#     # data logging
#     AquaticSite$set(which = "public",name = "track_EggQ",
#               value = track_MicroEggQ,
#               overwrite = overwrite
#     )
#
#
#   }
#
#
#   #################################################################
#   # Imago Queue
#   #################################################################
#
#   # clear the ImagoQ
#   AquaticSite$set(which = "public",name = "clear_ImagoQ",
#             value = clear_MicroImagoQ,
#             overwrite = overwrite
#   )
#
#   # extend the ImagoQ
#   AquaticSite$set(which = "public",name = "extend_ImagoQ",
#             value = extend_MicroImagoQ,
#             overwrite = overwrite
#   )
#
#   # add a egg batch to the ImagoQ
#   AquaticSite$set(which = "public",name = "add_ImagoQ",
#             value = add_MicroImagoQ,
#             overwrite = overwrite
#   )
#
#   # zeroBatch: zero out an egg batch in ImagoQ
#   AquaticSite$set(which = "public",name = "zero_ImagoQ",
#             value = zero_MicroImagoQ,
#             overwrite = overwrite
#   )
#
#   # get indices of full slots: return 0 if none
#   AquaticSite$set(which = "public",name = "full_ImagoQ",
#             value = full_MicroImagoQ,
#             overwrite = overwrite
#   )
#
#   # get indices of empty slots: return 0 if none
#   AquaticSite$set(which = "public",name = "empty_ImagoQ",
#             value = empty_MicroImagoQ,
#             overwrite = overwrite
#   )
#
#   # modifiers & accessors
#   AquaticSite$set(which = "public",name = "get_ImagoQ",
#             value = get_MicroImagoQ,
#             overwrite = overwrite
#   )
#   AquaticSite$set(which = "public",name = "set_ImagoQ",
#             value = set_MicroImagoQ,
#             overwrite = overwrite
#   )
#
#   # data logging
#   AquaticSite$set(which = "public",name = "track_ImagoQ",
#             value = track_MicroImagoQ,
#             overwrite = overwrite
#   )
#
# }
#
# #################################################################
# # ImagoQ: Emerge and EL4P
# #################################################################
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Clear the ImagoQ
# #'
# #' Clear out all populated slots in an ImagoQ for the EL4P module of Aquatic Ecology; populated slots are found by calling \code{\link{full_MicroImagoQ}}.
# #' This method should be called after calling adult emergence from the ImagoQ to \code{\link{MicroMosquitoPopFemale}} or \code{\link{MicroMosquitoPopMale}} object (whatever function does this should only move those adults that are ready to go, time-wise). WHEN YOU WRITE THESE FUNCTIONS UPDATE THESE DOCS!
# #' This method is bound to \code{AquaticSite$clear_ImagoQ()}.
# #'
# clear_MicroImagoQ <- function(){
#   fullIx = self$full_ImagoQ()
#   if(!is.null(fullIx)){
#     for(ix in fullIx){
#       private$ImagoQ[[ix]]$N         = 0L
#       private$ImagoQ[[ix]]$tEmerge   = 0
#       private$ImagoQ[[ix]]$genotype  = 0L
#       private$ImagoQ[[ix]]$damID     = 0L
#       private$ImagoQ[[ix]]$sireID    = 0L
#     }
#   }
# }
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Extend the ImagoQ
# #'
# #' This method extends the ImagoQ by 2 times its current length of empty emerging adult objects, see \code{\link{newImago}} for the emerging adult structure.
# #' This method is bound to \code{AquaticSite$extend_ImagoQ()}.
# #'
# extend_MicroImagoQ <- function(){
#   offset = length(private$ImagoQ)*2L
#   private$ImagoQ = c(private$ImagoQ,replicate(n=offset,expr=newImago(),simplify=FALSE))
# }
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Add emerging adults to the ImagoQ
# #'
# #' This method adds emerging adults to an ImagoQ, see \code{\link{newImago}} for the emerging adults structure; it uses \code{\link{empty_MicroImagoQ}} to find empty indices and
# #' calls \code{\link{extend_MicroImagoQ}} if no empty slots are found.
# #' This method is bound to \code{AquaticSite$add_ImagoQ()}.
# #'
# #' @param newImago a single emerging adults structure, see \code{\link{newImago}} for the emerging adults structure
# add_MicroImagoQ <- function(newImago){
#   # manage ImagoQ
#   emptyIx = self$empty_ImagoQ()
#   if(is.null(emptyIx)){
#     self$extend_ImagoQ()
#     emptyIx = self$empty_ImagoQ()
#   }
#
#   private$ImagoQ[[emptyIx[1]]]$N        = newImago$N
#   private$ImagoQ[[emptyIx[1]]]$tEmerge  = newImago$tEmerge
#   private$ImagoQ[[emptyIx[1]]]$genotype = newImago$genotype
#   private$ImagoQ[[emptyIx[1]]]$damID    = newImago$damID
#   private$ImagoQ[[emptyIx[1]]]$sireID   = newImago$sireID
#
# }
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Zero out a slot in the ImagoQ
# #'
# #' This method zeros out a slot in the ImagoQ.
# #' This method is bound to \code{AquaticSite$zero_ImagoQ()}.
# #'
# #' @param ixQ the slot in the ImagoQ to zero out
# zero_MicroImagoQ <- function(ixQ){
#   private$ImagoQ[[ixQ]]$N        = 0L
#   private$ImagoQ[[ixQ]]$tEmerge  = 0
#   private$ImagoQ[[ixQ]]$genotype = 0L
#   private$ImagoQ[[ixQ]]$damID    = 0L
#   private$ImagoQ[[ixQ]]$sireID   = 0L
# }
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Get indices of full ImagoQ slots
# #'
# #' This method finds filled slots in the ImagoQ; if all slots are empty it returns \code{NULL}. It is the complement of \code{\link{empty_MicroImagoQ}}
# #' This method is bound to \code{AquaticSite$full_ImagoQ()}.
# #'
# full_MicroImagoQ <- function(){
#   fullIx = vapply(X = private$ImagoQ,FUN = function(x){x$N != 0L},FUN.VALUE = logical(1))
#   if(all(!fullIx)){
#     return(NULL)
#   } else {
#     return(which(fullIx))
#   }
#
# }
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Get indices of empty ImagoQ slots
# #'
# #' This method finds empty slots in the ImagoQ; if all slots are full it returns \code{NULL}. It is the complement of \code{\link{full_MicroImagoQ}}
# #' This method is bound to \code{AquaticSite$empty_ImagoQ()}.
# #'
# empty_MicroImagoQ <- function(){
#   emptyIx = vapply(X = private$ImagoQ,FUN = function(x){x$N == 0L},FUN.VALUE = logical(1))
#   if(all(!emptyIx)){
#     return(NULL)
#   } else {
#     return(which(emptyIx))
#   }
# }
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Get the ImagoQ
# #'
# #' Get either a single slot or the entire ImagoQ.
# #' This method is bound to \code{AquaticSite$get_ImagoQ()}.
# #'
# #' @param ixQ if \code{NULL} return the entire ImagoQ, else, return the slot \code{ixQ}
# get_MicroImagoQ <- function(ixQ = NULL){
#   if(is.null(ixQ)){
#     return(private$ImagoQ)
#   } else {
#     return(private$ImagoQ[[ixQ]])
#   }
# }
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Set the ImagoQ
# #'
# #' Set either a single slot or the entire ImagoQ.
# #' This method is bound to \code{AquaticSite$set_ImagoQ()}.
# #'
# #' @param ImagoQ the object to insert; if \code{ixQ = NULL} then it should be a full ImagoQ object, see \code{\link{allocImagoQ}} for details, else see \code{\link{eggBatch}} for the structure of a single batch.
# #' @param ixQ if \code{NULL} set the entire ImagoQ, else, set the slot \code{ixQ}
# set_MicroImagoQ <- function(ImagoQ ,ixQ = NULL){
#   if(is.null(ixQ)){
#     private$ImagoQ = ImagoQ
#   } else {
#     private$ImagoQ[[ixQ]] = ImagoQ
#   }
# }
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Track the ImagoQ
# #'
# #' Return the total number of emerging adults in this ImagoQ
# #' This method is bound to \code{AquaticSite$track_ImagoQ()}.
# #' DEV_NOTE: eventually may want to add argument time or something and only accumulate those values tm <= time or something..., also might want to filter genotype
# #'
# track_MicroImagoQ <- function(){
#   return(sum(vapply(X = private$ImagoQ,FUN = function(x){x$N},FUN.VALUE = integer(1))))
# }
#
#
#
#
# #################################################################
# # EggQ: only EL4P
# #################################################################
#
# #' MICRO \code{\link{AquaticSite}} Method: Clear the EggQ
# #'
# #' Clear out all populated slots in an EggQ for the EL4P module of Aquatic Ecology; populated slots are found by calling \code{\link{full_MicroEggQ}}.
# #' This method should be called after moving batches from the EggQ to EL4P object (whatever function does this should only move those batches that are ready to go, time-wise). WHEN YOU WRITE THESE FUNCTIONS UPDATE THESE DOCS!
# #' This method is bound to \code{AquaticSite$clear_EggQ()}.
# #'
# clear_MicroEggQ <- function(){
#   fullIx = self$full_EggQ()
#   if(!is.null(fullIx)){
#     for(ix in fullIx){
#       private$EggQ[[ix]]$N           = 0L
#       private$EggQ[[ix]]$tOviposit   = 0
#       private$EggQ[[ix]]$damID       = 0L
#       private$EggQ[[ix]]$sireID      = 0L
#       private$EggQ[[ix]]$genotype    = 0L
#     }
#   }
# }
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Extend the EggQ
# #'
# #' This method extends the EggQ by 2 times its current length of empty egg batch objects, see \code{\link{newEgg}} for the egg batch structure.
# #' This method is bound to \code{AquaticSite$extend_EggQ()}.
# #'
# extend_MicroEggQ <- function(){
#   offset = length(private$EggQ)*2L
#   private$EggQ = c(private$EggQ,replicate(n=offset,expr=newEgg(),simplify=FALSE))
# }
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Add a batch to the EggQ
# #'
# #' This method adds an egg batch to an EggQ, see \code{\link{newEgg}} for the egg batch structure; it uses \code{\link{empty_MicroEggQ}} to find empty indices and
# #' calls \code{\link{extend_MicroEggQ}} if no empty slots are found.
# #' This method is bound to \code{AquaticSite$add_EggQ()}.
# #'
# #' @param eggBatch a single egg batch, see \code{\link{newEgg}} for the egg batch structure
# add_MicroEggQ <- function(eggBatch){
#   # manage EggQ
#   emptyIx = self$empty_EggQ()
#   if(is.null(emptyIx)){
#     self$extend_EggQ()
#     emptyIx = self$empty_EggQ()
#   }
#
#   private$EggQ[[ix]]$N           = N
#   private$EggQ[[ix]]$tOviposit   = tOviposit
#   private$EggQ[[ix]]$damID       = damID
#   private$EggQ[[ix]]$sireID      = sireID
#   private$EggQ[[ix]]$genotype    = genotype
#
# }
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Zero out a slot in the EggQ
# #'
# #' This method zeros out a slot in the EggQ.
# #' This method is bound to \code{AquaticSite$zero_EggQ()}.
# #'
# #' @param ixQ the slot in the EggQ to zero out
# zero_MicroEggQ <- function(ixQ){
#   private$EggQ[[ixQ]]$N           = 0L
#   private$EggQ[[ixQ]]$tOviposit   = 0
#   private$EggQ[[ixQ]]$damID       = 0L
#   private$EggQ[[ixQ]]$sireID      = 0L
#   private$EggQ[[ixQ]]$genotype    = 0L
# }
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Get indices of full EggQ slots
# #'
# #' This method finds filled slots in the EggQ; if all slots are empty it returns \code{NULL}. It is the complement of \code{\link{empty_MicroEggQ}}
# #' This method is bound to \code{AquaticSite$full_EggQ()}.
# #'
# full_MicroEggQ <- function(){
#   fullIx = vapply(X = private$EggQ,FUN = function(x){x$N != 0L},FUN.VALUE = logical(1))
#   if(all(!fullIx)){
#     return(NULL)
#   } else {
#     return(which(fullIx))
#   }
#
# }
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Get indices of empty EggQ slots
# #'
# #' This method finds empty slots in the EggQ; if all slots are full it returns \code{NULL}. It is the complement of \code{\link{full_MicroEggQ}}
# #' This method is bound to \code{AquaticSite$empty_EggQ()}.
# #'
# empty_MicroEggQ <- function(){
#   emptyIx = vapply(X = private$EggQ,FUN = function(x){x$N == 0L},FUN.VALUE = logical(1))
#   if(all(!emptyIx)){
#     return(NULL)
#   } else {
#     return(which(emptyIx))
#   }
# }
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Get the EggQ
# #'
# #' Get either a single slot or the entire EggQ.
# #' This method is bound to \code{AquaticSite$get_EggQ()}.
# #'
# #' @param ixQ if \code{NULL} return the entire EggQ, else, return the slot \code{ixQ}
# get_MicroEggQ <- function(ixQ = NULL){
#   if(is.null(ixQ)){
#     return(private$EggQ)
#   } else {
#     return(private$EggQ[[ixQ]])
#   }
# }
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Set the EggQ
# #'
# #' Set either a single slot or the entire EggQ.
# #' This method is bound to \code{AquaticSite$set_EggQ()}.
# #'
# #' @param EggQ the object to insert; if \code{ixQ = NULL} then it should be a full EggQ object, see \code{\link{allocEggQ}} for details, else see \code{\link{eggBatch}} for the structure of a single batch.
# #' @param ixQ if \code{NULL} set the entire EggQ, else, set the slot \code{ixQ}
# set_MicroEggQ <- function(EggQ ,ixQ = NULL){
#   if(is.null(ixQ)){
#     private$EggQ = EggQ
#   } else {
#     private$EggQ[[ixQ]] = EggQ
#   }
# }
#
#
# #' MICRO \code{\link{AquaticSite}} Method: Track the EggQ
# #'
# #' Return the total number of eggs in this EggQ
# #' This method is bound to \code{AquaticSite$track_EggQ()}.
# #' DEV_NOTE: eventually may want to add argument time or something and only accumulate those values tm <= time or something...
# #'
# track_MicroEggQ <- function(){
#   return(sum(vapply(X = private$EggQ,FUN = function(x){x$N},FUN.VALUE = integer(1))))
# }
