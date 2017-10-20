#######################################################################
#
#       __  _______  _____ ____  __  ________________
#      /  |/  / __ \/ ___// __ \/ / / /  _/_  __/ __ \
#     / /|_/ / / / /\__ \/ / / / / / // /  / / / / / /
#    / /  / / /_/ /___/ / /_/ / /_/ // /  / / / /_/ /
#   /_/  /_/\____//____/\___\_\____/___/ /_/  \____/
#
#   MOSQUITO: MacroRM Mosquito Setup
#   David Smith, Hector Sanchez, Sean Wu
#   August 16, 2017
#
#######################################################################

MacroRM.Setup <- function(overwrite = TRUE){

  MacroRMMosquito$set(which = "private",name = "oneDay_RM",
                          value = MacroRM_oneDay_RM,
                          overwrite = overwrite

  MacroRMMosquito$set(which = "private",name = "migrationOut_RM",
                          value = MacroRM_migrationOut_RM,
                          overwrite = overwrite

  MacroRMMosquito$set(which = "private",name = "migrationIn_RM",
                          value = MacroRM_migrationIn_RM,
                          overwrite = overwrite
}
