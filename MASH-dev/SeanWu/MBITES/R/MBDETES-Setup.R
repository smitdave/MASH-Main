###############################################################################
#         __  _______  ____  _______________________
#        /  |/  / __ )/ __ \/ ____/_  __/ ____/ ___/
#       / /|_/ / __  / / / / __/   / / / __/  \__ \
#      / /  / / /_/ / /_/ / /___  / / / /___ ___/ /
#     /_/  /_/_____/_____/_____/ /_/ /_____//____/
#
#     MBDETES - Setup MBITES Options to Match MBDETES Assumptions
#     MBITES Team
#     July 2018
#
###############################################################################


#' MBITES: Setup Parameters & Model Options for MBDETES Approximation
#'
#' This function sets up MBITES model parameters and model behavioral options to the reduced
#' MBITES model to which MBDETES is able to approximate.
#'
#' @export
MBITES_Setup_MBDETES <- function(){

  cat("initializing MBITES functional forms for MBDETES approximation\n")

  # time to event parameters
  MBITES::MBITES_Setup_Timing(timing_model = 2,
                    rate_b = 1/(3/24),tmin_b = 0,
                    rate_bs = 1/(6/24),tmin_bs = 0,
                    rate_o = 1/(3/24),tmin_o = 0,
                    rate_os = 1/(6/24),tmin_os = 0,
                    ppr_model = 2,rate_ppr = 1/(18/24),tmin_ppr = 0)

  MBITES:::Globals$set_SETUP("timing")

  # bloodmeal: turn off overfeeding
  Mosquito_Female$set(which = "public",name = "Overfeeding",
      value = mbites_OverfeedingNull, overwrite = TRUE
  )
  MBITES:::Globals$set_SETUP("bloodmeal")

  # Oogenesis options
  # egg batch size proportional to blood meal
  Mosquito_Female$set(which = "public",name = "rBatchSize",
      value = mbites_rBatchSizeBms, overwrite = TRUE
  )

  # main oogenesis model
  Mosquito_Female$set(which = "public",name = "Oogenesis",
      value = mbites_oogenesisMBDETES, overwrite = TRUE
  )

  # refeeding probability is function of blood meal size
  Mosquito_Female$set(which = "public",name = "checkRefeed",
      value = mbites_checkRefeedMBDETES, overwrite = TRUE
  )

  Mosquito_Female$set(which = "public",name = "pReFeed",
      value = mbites_pReFeed_bm, overwrite = TRUE
  )
  MBITES:::Globals$set_SETUP("oogenesis")

  # turn sugar off
  Mosquito_Female$set(which = "public",name = "queueSugarBout",
            value = mbites_queueSugarBout_null, overwrite = TRUE
  )
  MBITES:::Globals$set_SETUP("energetics")

  # oviposition uses emerge
  Mosquito_Female$set(which = "public",name = "layEggs",
            value = mbites_layEggs_Emerge, overwrite = TRUE
  )
  MBITES:::Globals$set_SETUP("oviposition")

  # survival: turn off wing tattering and senescence
  Mosquito$set(which = "public",name = "WingTattering",
      value = mbites_WingTattering_null, overwrite = TRUE
  )

  Mosquito$set(which = "public",name = "Senescence",
      value = mbites_Senescence_null, overwrite = TRUE
  )
  MBITES:::Globals$set_SETUP("survival")
}
