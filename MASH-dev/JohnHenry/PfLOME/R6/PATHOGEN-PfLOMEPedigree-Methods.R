###############################################################################
#
#       ____  ______    ____  __  _________
#      / __ \/ __/ /   / __ \/  |/  / ____/
#     / /_/ / /_/ /   / / / / /|_/ / __/
#    / ____/ __/ /___/ /_/ / /  / / /___
#   /_/   /_/ /_____/\____/_/  /_/_____/
#
#   MASH-PATHOGEN
#   PfLOME: PfPedigree for PfLOME Class Methods
#   MASH Team
#   August 24, 2017
#
###############################################################################


#' PfLOME_Pedigree: Get PfID
#'
#' Return \code{private$pop}
#'  * This method is bound to \code{PfPedigree$add2Pedigree_PfLOME}
#'
add2Pedigree_PfLOME <- function(){
  # getParent: needs some thought; needs to find the HumanPop in either the MacroPatch or MicroTile
}

PfPedigree$set(which = "public",name = "add2Pedigree_PfLOME",
  value = add2Pedigree_PfLOME,
  overwrite = TRUE)
