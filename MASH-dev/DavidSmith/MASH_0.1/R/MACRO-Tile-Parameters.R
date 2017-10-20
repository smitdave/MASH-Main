#################################################################
#
#   MASH
#   R6-ified
#   MACRO MacroTile Class Parameters
#   David Smith, Hector Sanchez, Sean Wu
#   May 26, 2017
#
#################################################################

#' Initialize MACRO Tile Parameters for \code{MacroTile}
#'
#' This is used to generate a list of parameters for \code{\link{MacroTile}} and should be used during its initialization.
#' \code{MACRO.Tile.Parameters} will generate parameters that are passed to the \code{\link[R6]{initialize}} method of
#' the component objects in a tile:
#' * \code{\link{HumanPop}}: creates parameters by calling \code{\link{HumanPop.Parameters}}
#' * \code{\link{MacroPatch}}: creates parameters by calling \code{\link{MACRO.Patch.Parameters}}
#' * \code{\link{MacroMosquitoPop}}: directly initializes parameters
#'
#' @param N number of patches
#' @param patchSize passed to \code{\link{sitePops}}
#' @param patchMin passed to \code{\link{sitePops}}
#' @param EIP length of entomological incubation period
#' @param aquaModule character in 'emerge', 'EL4P'
#' @param aquaPars Aquatic Ecology module specific parameters (see \code{\link{MACRO.Patch.Parameters}} for details)
#' * Emerge: list with elements 'lambda'
#' * EL4P: list with elements
#' @param pathogenModule character in 'PfSI', 'PfMOI'
#' @param humanPars character in 'auto', 'manual' ; if \code{manual} the user should pass
#' @md
#' @return return a list
#' @examples
#' MACRO.Tile.Parameters()
#' @export
MACRO.Tile.Parameters <- function(

  N = 10,
  patchSize = 20,
  patchMin = 10,
  EIP = 10,
  aquaModule = "emerge",
  aquaPars = list(lambda=rep(10,10)),
  pathogenModule = "PfSI",
  humanPars = "auto"

  ){

    # make HumanPop_PARs
    HumanPop_PAR = HumanPop.Parameters(nSite = N, siteSize=patchSize,siteMin=patchMin)

    # find out what patches humans live in
    patch_hhID_helper = rle(x = HumanPop_PAR$homeIDs)
    patch_hhID = mapply(FUN = function(x,y){
        rep(x = x,times=y)
      },x=patch_hhID_helper$values,y=patch_hhID_helper$lengths)

    # make MacroPatch_PAR
    MacroPatch_PAR = MACRO.Patch.Parameters(N=N, hhID=patch_hhID, humanIDs=HumanPop_PAR$siteHumanIDs, aquaModule = aquaModule,aquaPars = aquaPars)

    # make MacroMosquitoPop_PAR
    MacroMosquitoPop_PAR = MACRO.MosquitoPop.Parameters(N=N,EIP=EIP)

  return(
    list(
      N = N,
      aquaModule = aquaModule,
      pathogenModule = pathogenModule,
      HumanPop_PAR = HumanPop_PAR,
      MacroPatch_PAR = MacroPatch_PAR,
      MacroMosquitoPop_PAR = MacroMosquitoPop_PAR
    )
  )
}
