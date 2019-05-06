###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     MBITES - energetics (sugar and energy burn from activity)
#     Sean Wu
#     March 2019
#
###############################################################################

# energetics
energetics <- function(mosy){
  flightBurnEnergy(mosy)
  # queueSugarBout(mosy)
}

flightBurnEnergy <- function(mosy){
  mosy$energy <- max(0,mosy$energy - get("parameters",.GlobalEnv)$S_u)
}
