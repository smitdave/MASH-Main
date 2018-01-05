###############################################################################
#
#       __  _______  _____ ____  __  ________________
#      /  |/  / __ \/ ___// __ \/ / / /  _/_  __/ __ \
#     / /|_/ / / / /\__ \/ / / / / / // /  / / / / / /
#    / /  / / /_/ /___/ / /_/ / /_/ // /  / / / /_/ /
#   /_/  /_/\____//____/\___\_\____/___/ /_/  \____/
#
#   MASH-MICRO
#   MICRO: Mosquito Class Definition
#   MASH-MICRO Team
#   September 6, 2017
#
###############################################################################


###############################################################################
# Female Mosquito Class
###############################################################################


#' MosquitoFemale Class Definition
#'
#' im a class!
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: im an agument!
#'
#' @section **Methods**:
#'  * method: im a method!
#'
#' @section **Fields**:
#'  * id: character of format tEmerge_id_genotype (ie; "5_42_2" is the 42nd mosquito emerging on day 5 with genotype 2)
#'  * field: im a field!
#'
#' @export
MosquitoFemale <- R6::R6Class(classname = "MosquitoFemale",
                          portable = TRUE,
                          cloneable = FALSE,
                          lock_class = FALSE,
                          lock_objects = FALSE,

                          # public members
                          public = list(

                            ##############################################################
                            # Initializer
                            ##############################################################

                            # pointers are also passed down from enclosing MosquitoPopFemale object
                            initialize = function(id, time, locNow, genotype, state, pSetNow = "l", eggT = 0, eggP = 0, energyPreG = 0){

                              # initialize general fields

                              # ID and time
                              private$id        = id        # mosquito id
                              private$bDay      = time        # time of emergence
                              private$tNow      = time        # time of last event
                              private$tNext     = time        # time to next event
                              private$genotype  = genotype

                              # State and Location
                              private$state        = state       # {F,B,R,L,O,S,M,E,D}
                              private$stateNew     = state       # {F,B,R,L,O,S,M,E,D}
                              private$pSetNow      = pSetNow       # class of site {f,l,s,m}
                              private$locNow       = locNow       # index of site
                              private$mature       = FALSE       # mature
                              private$periDomestic = FALSE       # used in peri-domestic breeding (see mbites_OvipositSearchCheck)

                              # Other State Variables
                              private$lspot     = "l"        # landing spot (i: inside wall, w: outside wall, v: outside vegetation, r: feed, l: leave)
                              private$damage    = 0        # wing tattering
                              private$energy    = 1        # energy reserves

                              # Egg Batch Variables
                              private$bmSize = 0         # the size of the blood meal, relative to max
                              private$batch  = 0         # female eggs in batch
                              private$eggT   = eggT         # MBITES_PAR the minimum time before eggs are mature
                              private$eggP   = eggP         # MBITES_PAR the mimimum provision for eggs to mature

                              # Maturation & Reproduction
                              private$energyPreG  = energyPreG           # MBITES_PAR pre-gonotrophic energy requirement

                              # initialize PATHOGEN object
                              # PAR must contain a function mapped to the R6ClassGenerator object needed
                              self$init_Pathogens()

                              # initialize history object
                              private$history = MASHcpp::MosquitoFemaleHistory()
                              private$history$historyInit(privateEnv = private)

                            } # end initializer

                          ),

                          # private members
                          private = list(

                            # Generic Fields

                            # ID and time
                            id        = character(1),        # mosquito id
                            bDay      = numeric(1),        # time of emergence
                            tNow      = numeric(1),        # time of last event
                            tNext     = numeric(1),        # time to next event
                            genotype  = integer(1),        # genotype of mosquito

                            # State and Location
                            state        = character(1),       # {F,B,R,L,O,S,M,E,D}
                            stateNew     = character(1),       # {F,B,R,L,O,S,M,E,D}
                            pSetNow      = character(1),       # class of site {f,l,s,m}
                            pSetOld      = character(1),
                            locNow       = integer(1),       # index of site
                            locOld       = integer(1),
                            mature       = logical(1),       # mature
                            periDomestic = logical(1),

                            # Other State Variables
                            lspot     = character(1),        # landing spot (i: inside wall, w: outside wall, v: outside vegetation, r: feed, l: leave)
                            damage    = numeric(1),        # wing tattering
                            energy    = numeric(1),        # energy reserves

                            history   = NULL,        # history object

                            # Female Fields

                            # Egg Batch Variables
                            bmSize = numeric(1),         # the size of the blood meal, relative to max
                            batch  = numeric(1),         # female eggs in batch
                            eggT   = numeric(1),         # the minimum time before eggs are mature
                            eggP   = numeric(1),         # the mimimum provision for eggs to mature

                            # Maturation & Reproduction
                            mateID        = character(1),
                            mateGenotype  = integer(1),
                            energyPreG    = numeric(1),          # pre-gonotrophic energy requirement
                            hostID        = character(1),           # the id of the host: -1::none; 0::not human

                            # Pathogens
                            Pathogens      = NULL,

                            # Pointers
                            FemalePopPointer = NULL,  # Point to enclosing MosquitoPopFemale
                            MalePopPointer = NULL,    # Point to MosquitoPopMale in the same microsimulation Tile
                            LandscapePointer = NULL,  # Point to Landscape object in same microsimulation Tile
                            HumansPointer = NULL,     # Point to HumanPop object in same microsimulation Tile
                            TilePointer = NULL        # Point to enclosing microsimulation Tile

                          )

)


###############################################################################
# Male Mosquito Class
###############################################################################

#' MosquitoMale Class Definition
#'
#' im a class!
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * argument: im an agument!
#'
#' @section **Methods**:
#'  * method: im a method!
#'
#' @section **Fields**:
#'  * field: im a field!
#'
#' @export
MosquitoMale <- R6::R6Class(classname = "MosquitoMale",
                        portable = TRUE,
                        cloneable = FALSE,
                        lock_class = FALSE,
                        lock_objects = FALSE,

                        # public members
                        public = list(

                          ##############################################################
                          # Initializer
                          ##############################################################

                          # pointers are also passed down from enclosing MosquitoPopFemale object
                          initialize = function(id, time, locNow, genotype, state, pSetNow = "l", mateFitness = 1){

                            # initialize general fields

                            # ID and time
                            private$id        = id        # mosquito id
                            private$bDay      = time        # time of emergence
                            private$tNow      = time        # time of last event
                            private$tNext     = time        # time to next event
                            private$genotype  = genotype

                            # State and Location
                            private$state      = state       # {F,B,R,L,O,S,M,E,D}
                            private$stateNew   = state       # {F,B,R,L,O,S,M,E,D}
                            private$pSetNow    = pSetNow       # class of site {f,l,s,m}
                            private$locNow     = locNow       # index of site
                            private$mature     = FALSE       # mature

                            # Other State Variables
                            private$lspot     = "l"        # landing spot (i: inside wall, w: outside wall, v: outside vegetation, r: feed, l: leave)
                            private$damage    = 0        # wing tattering
                            private$energy    = 1        # energy reserves
                            private$mateFitness = mateFitness

                            # initialize history object
                            private$history = MASHcpp::MosquitoMaleHistory()
                            private$history$historyInit(privateEnv = private)

                          } # end initializer

                        ),

                        # private members
                        private = list(

                          # Biological Fields

                          # ID and time
                          id        = character(1),        # mosquito id
                          bDay      = numeric(1),        # time of emergence
                          tNow      = numeric(1),        # time of last event
                          tNext     = numeric(1),        # time to next event
                          genotype  = integer(1),        # genotype of mosquito

                          # State and Location
                          state      = character(1),       # {F,B,R,L,O,S,M,E,D}
                          stateNew   = character(1),       # {F,B,R,L,O,S,M,E,D}
                          pSetNow    = character(1),       # class of site {f,l,s,m}
                          pSetOld   = character(1),
                          locNow     = integer(1),       # index of site
                          locOld    = integer(1),
                          mature     = logical(1),       # mature

                          # Other State Variables
                          lspot     = character(1),        # landing spot (i: inside wall, w: outside wall, v: outside vegetation, r: feed, l: leave)
                          damage    = numeric(1),        # wing tattering
                          energy    = numeric(1),        # energy reserves
                          mateFitness = numeric(1),      # mating fitness

                          history = NULL,          # history

                          # Pointers
                          FemalePopPointer = NULL,  # Point to enclosing MosquitoPopFemale
                          MalePopPointer = NULL,    # Point to MosquitoPopMale in the same microsimulation Tile
                          LandscapePointer = NULL,  # Point to Landscape object in same microsimulation Tile
                          HumansPointer = NULL,     # Point to HumanPop object in same microsimulation Tile
                          TilePointer = NULL        # Point to enclosing microsimulation Tile

                        )
)
