###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Analysis of peri-domestic simulation experiments
#     MBITES Team
#     August 2018
#
###############################################################################

rm(list = ls());gc()
library(MBITES)


###############################################################################
# load data & calculate bionomics
###############################################################################

library(jsonlite)

# where the files can be found
directory <- "/Users/slwu89/Desktop/mbites/peridom/"

run <- "1"
output_dir <- paste0(directory,"run",run)

mosquitos <- fromJSON(paste0(output_dir,"/mosquito_F_",run,".json"), flatten = TRUE)
# mosquitos <- mosquitos[-which(sapply(mosquitos$id,is.null)),]

bionomics <- Bionomics_MBDETES_Approx(mosquitos)
