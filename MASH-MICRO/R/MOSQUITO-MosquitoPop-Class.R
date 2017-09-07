###############################################################################
#
#       __  _______  _____ ____  __  ________________
#      /  |/  / __ \/ ___// __ \/ / / /  _/_  __/ __ \
#     / /|_/ / / / /\__ \/ / / / / / // /  / / / / / /
#    / /  / / /_/ /___/ / /_/ / /_/ // /  / / / /_/ /
#   /_/  /_/\____//____/\___\_\____/___/ /_/  \____/
#
#   MASH-MICRO
#   MICRO: MosquitoPop Class Definitions
#   MASH-MICRO Team
#   September 7, 2017
#
###############################################################################


###############################################################################
# MosquitoPopFemale: Class Definition
###############################################################################

MosquitoPopFemale <- R6::R6Class(classname = "MosquitoPopFemale",
                       portable = TRUE,
                       cloneable = FALSE,
                       lock_class = FALSE,
                       lock_objects = FALSE,

                       public = list(

                         ##############################################################
                         # Initializer
                         ##############################################################

                         initialize = function(N, time_init, ix_init, genotype_init, MBITES_PAR, module){

                           private$pop = MASHcpp::HashMap$new(N = N)

                         } # end initializer

                       ),

                       private = list(

                         # Fields
                         pop = NULL,               # mosquito population
                         initState = character(1),         # initial state for newly emerging females
                         MBITES_PAR = list(),        # MBITES Parameters

                         # Pointers
                         MalePopPointer = NULL,    # Point to MicroMosquitoPopMale in the same microsimulation Tile
                         LandscapePointer = NULL,  # Point to Landscape object in same microsimulation Tile
                         HumansPointer = NULL,     # Point to HumanPop object in same microsimulation Tile
                         TilePointer = NULL        # Point to enclosing microsimulation Tile

                       )
)


###############################################################################
# MosquitoPopMale: Class Definition
###############################################################################


MosquitoPopMale <- R6::R6Class(classname = "MosquitoPopFemale",
                       portable = TRUE,
                       cloneable = FALSE,
                       lock_class = FALSE,
                       lock_objects = FALSE,

                       public = list(

                         ##############################################################
                         # Initializer
                         ##############################################################

                         initialize = function(N, time_init, ix_init, genotype_init, MBITES_PAR, module){

                           private$pop = MASHcpp::HashMap$new(N = N)

                         } # end initializer

                       ),

                       private = list(

                         # Fields
                         pop = NULL,               # mosquito population
                         initState = character(1),         # initial state for newly emerging females
                         MBITES_PAR = list(),        # MBITES Parameters

                         # Pointers
                         FemalePopPointer = NULL,    # Point to MicroMosquitoPopMale in the same microsimulation Tile
                         LandscapePointer = NULL,  # Point to Landscape object in same microsimulation Tile
                         HumansPointer = NULL,     # Point to HumanPop object in same microsimulation Tile
                         TilePointer = NULL        # Point to enclosing microsimulation Tile

                       )
)
