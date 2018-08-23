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
directory <- "/Users/slwu89/Desktop/mbites/peridomIHME/"

run <- "1"
output_dir_set1 <- paste0(directory,"set1/","run",run)
output_dir_set2 <- paste0(directory,"set2/","run",run)

mosquitos1 <- fromJSON(paste0(output_dir_set1,"/mosquito_F_",run,".json"), flatten = TRUE)
mosquitos2 <- fromJSON(paste0(output_dir_set2,"/mosquito_F_",run,".json"), flatten = TRUE)

mosquitos <- rbind(mosquitos1,mosquitos2)
mosquitos <- mosquitos[-which(sapply(mosquitos$id,is.null)),]

rm(mosquitos1,mosquitos2)


