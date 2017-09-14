##############################################################################################################
#
#       ___                                __                    __  ___      __            _       __
#      /   |  ____  ____  ____ ___  ____ _/ /___  __  _______   /  |/  /___ _/ /____  _____(_)___ _/ /____
#     / /| | / __ \/ __ \/ __ `__ \/ __ `/ / __ \/ / / / ___/  / /|_/ / __ `/ __/ _ \/ ___/ / __ `/ / ___/
#    / ___ |/ / / / /_/ / / / / / / /_/ / / /_/ / /_/ (__  )  / /  / / /_/ / /_/  __/ /  / / /_/ / (__  )
#   /_/  |_/_/ /_/\____/_/ /_/ /_/\__,_/_/\____/\__,_/____/  /_/  /_/\__,_/\__/\___/_/  /_/\__,_/_/____/
#
#   Test Non-trivia Parallel Applications
#
##############################################################################################################

library(parallel)

# we use a socket cluster because we don't want to fork the current environment
cl = parallel::makeCluster(spec = 2,type = "PSOCK")

writeBin(object = list(id=1L),con = cl[[1]]$con)

serialize(object = list(id=1L),connection = cl[[1]]$con)

parallel::clusterEvalQ(cl = cl,expr = {ls()})

# try using parallel:::sendCall, found within clusterCall

parallel::stopCluster(cl)
rm(cl)
