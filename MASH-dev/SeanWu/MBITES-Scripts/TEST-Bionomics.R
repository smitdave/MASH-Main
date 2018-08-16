###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Test Bionomics
#     MBITES Team
#     August 2018
#
###############################################################################

rm(list = ls());gc()
library(MBITES)


###############################################################################
# load data
###############################################################################

library(jsonlite)

# where the files can be found
directory <- "/Users/slwu89/Desktop/mbites/trivial/"
output_dir <- paste0(directory,"run2")

mosquitos_df <- fromJSON(paste0(output_dir,"/mosquito_F_2.json"), flatten = TRUE)
mosquitos_df <- mosquitos_df[-which(sapply(mosquitos_df$id,is.null)),]
humans_df <- fromJSON(paste0(output_dir,"/human_2.json"), flatten = TRUE)
humans_df <- humans_df[-which(sapply(humans_df$id,is.null)),]


M <- Bionomics_StateTransition(mosquitos_df)








