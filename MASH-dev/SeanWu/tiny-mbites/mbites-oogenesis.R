###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES - oogenesis & refeeding
#     Sean Wu
#     March 2019
#
###############################################################################

oogenesis <- function(mosy){
  # zombie mosquitos cant make eggs, also they cant make them on an empty stomach
  if(mosy$statenext != "D" & mosy$bloodfed){

    if(mosy$batch <= 2e-16){
      
    }

  }
}

# # Oogenesis options
# # egg batch size proportional to blood meal
# Mosquito_Female$set(which = "public",name = "rBatchSize",
#     value = mbites_rBatchSizeBms, overwrite = TRUE
# )
#
# # main oogenesis model
# Mosquito_Female$set(which = "public",name = "Oogenesis",
#     value = mbites_oogenesisMBDETES, overwrite = TRUE
# )
#
# # refeeding probability is function of blood meal size
# Mosquito_Female$set(which = "public",name = "checkRefeed",
#     value = mbites_checkRefeedMBDETES, overwrite = TRUE
# )
#
# Mosquito_Female$set(which = "public",name = "pReFeed",
#     value = mbites_pReFeed_bm, overwrite = TRUE
# )
# MBITES:::Globals$set_SETUP("oogenesis")


checkEggMaturation_time <- function(mosy){

  # check the time
  if(mosy$eggT <= mosy$tnow){
    mosy$gravid <- TRUE
  }

}



checkEggMaturation_provision <- function(mosy){

  # check the provision
  if(mosy$eggP <= 0){
    mosy$gravid <- TRUE
  }

}
