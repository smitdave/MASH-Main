###############################################################################
#
#       ____  ______    ____  __  _________
#      / __ \/ __/ /   / __ \/  |/  / ____/
#     / /_/ / /_/ /   / / / / /|_/ / __/
#    / ____/ __/ /___/ /_/ / /  / / /___
#   /_/   /_/ /_____/\____/_/  /_/_____/
#
#   MASH-PATHOGEN
#   PfLOME: PfPedigree for PfLOME Class Definition
#   MASH Team
#   August 24, 2017
#
###############################################################################

#' PfLOME Pedigree Class Definition
#'
#' Generate a PfPedigree object for PfLOME module of plasmodium falciparum infection.
#' Each instance of a \code{PfLOME_Pedigree} lives in a \code{\link{PfPedigree}} object.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * PfID: Unique identifier for clonal infection (used to recall this variant from enclosing \code{\link{PfPedigree}}) object.
#'  * gType: genotype (at each locus; vector)
#'  * pType: phenotype (associated with each locus; vector)
#'  * mac: Unique identifier of the parents (macrogametocyte; female)
#'  * mic: Unique identifier of the parents (microgametocyte; male)
#'  * sib: 11, 12, 21, 22 (from same ookinete)
#'  * mIx: index of site mosquito was infected
#'  * mTx: time mosquito was infected
#'  * ixH2M: \code{private$myID} field of transmitting human
#'  * hIx: index of site human was infected
#'  * hTx: time human was infected
#'  * endTx: time infection ended in human
#'  * ixH: \code{private$myID} field of infected human
#'  * nSPZ: number of sporozoites
#'
#'
#' @section **Methods**:
#'
#' @section **Fields**:
#'
#'
#'
#' @md
#' @export
PfLOME_Pedigree <- R6::R6Class(classname = "PfLOME_Pedigree",
                    portable = TRUE,
                    cloneable = TRUE,
                    lock_class = FALSE,
                    lock_objects = FALSE,

                    # public members
                    public = list(

                      #################################################
                      # Constructor
                      #################################################

                      initialize = function(PfID, gType=0, pType=0, mac=0, mic=0, sib=0, mIx=0, mTx=0, ixH2M=0, hIx=0, hTx=0, endTx=0, ixH=0, nSPZ=0){

                        private$PfID = PfID
                        private$gType = gType
                        private$pType = pType
                        private$mac = mac
                        private$mic = mic
                        private$sib = sib
                        private$mIx = mIx
                        private$mTx = mTx
                        private$ixH2M = ixH2M
                        private$hIx = hIx
                        private$hTx = hTx
                        private$endTx = endTx
                        private$ixH = ixH
                        private$nSPZ = nSPZ

                      }

                    ),

                    # private members
                    private = list(

                      PfID = NULL,
                      gType = NULL,
                      pType = NULL,
                      mac = NULL,
                      mic = NULL,
                      sib = NULL,
                      mIx = NULL,
                      mTx = NULL,
                      ixH2M = NULL,
                      hIx = NULL,
                      hTx = NULL,
                      endTx = NULL,
                      ixH = NULL,
                      nSPZ = NULL

                    )
) # end class definition


###############################################################################
# PfLOME_Pedigree: Getters & Setters
###############################################################################

#' PfLOME_Pedigree: Get PfID
#'
#' Return \code{private$pop}
#'
#'
get_PfID_PfLOME_Pedigree <- function(){
  return(private$PfID)
}

PfLOME_Pedigree$set(which = "public",name = "get_PfID",
  value = get_PfID_PfLOME_Pedigree,
  overwrite = TRUE)

#' PfLOME_Pedigree: Set PfID
#'
#' Set \code{private$PfID}
#'
#'
set_PfID_PfLOME_Pedigree <- function(PfID){
  private$PfID = PfID
}

PfLOME_Pedigree$set(which = "public",name = "set_PfID",
  value = set_PfID_PfLOME_Pedigree,
  overwrite = TRUE)
