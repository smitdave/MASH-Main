###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     null pathogen object
#     Sean Wu
#     June 2019
#
###############################################################################

probeHost_null <- function(mosy){
  pushProbe_human_null(get("humans")[[mosy$hostID]],mosy$id,mosy$tnow)
}

feedHost_null <- function(mosy){
  pushFeed_human_null(get("humans")[[mosy$hostID]])
}
