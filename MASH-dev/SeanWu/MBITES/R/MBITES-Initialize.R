###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES-Initialize Mosquito Populations
#     MBITES Team
#     February 2018
#
###############################################################################


###############################################################################
# Initialize mosquito populations
###############################################################################

#' MBITES Initialize Mosquito Populations
#'
#' Initialize mosquito populations
#'
#' @param mosy_init a \code{\link{data.frame}} object where each row corresponds to a mosquito
#'
#' @details
#'
#'
#'
#' @export
MBITES_Initialize <- function(mosy_init){

  n = nrow(mosy_init)
  pb = txtProgressBar(min = 0, max = n, initial = 0)
  time = MBITES:::Globals$get_tNow()

  for(i in 1:n){
    tileID = mosy_init[i,"tileID"]
    siteID = mosy_init[i,"siteID"]
    female = mosy_init[i,"female"]
    id = MBITES:::Globals$get_mosquito_id()
    site = MBITES:::Globals$get_tile(tileID)$get_site(siteID)
    mosy = NULL
    if(female){
      mosy = Mosquito_Female$new(id, bDay=time, site=site, tileID=tileID)
    } else {
      mosy = Mosquito_Male$new(id, bDay=time, site=site, tileID=tileID)
    }
    MBITES:::Globals$get_tile(tileID)$get_mosquitoes()$assign(key=mosy$get_id(),value=mosy)

    setTxtProgressBar(pb,i)
  }
}
