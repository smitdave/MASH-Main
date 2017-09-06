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

                            # initialize: needs to take PAR as a parameter because certain module-specific parameter values will be stored there
                            # pointers are also passed down from enclosing MicroMosquitoPopFemale object
                            initialize = function(id, time, ix, genotype, state, eggT = 0, eggP = 0, energyPreG = 0){

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
                              private$inPointSet = "l"       # class of site {f,l,s,m}
                              private$ix         = ix       # index of site
                              private$mature     = FALSE       # mature

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
                              sire        = 0
                              energyPreG  = energyPreG           # MBITES_PAR pre-gonotrophic energy requirement
                              hostID      = 0           # the id of the host: -1::none; 0::not human

                              # initialize PATHOGEN object
                              # PAR must contain a function mapped to the R6ClassGenerator object needed
                              self$init_Pathogens()

                              # initialize history object
                              private$history = MASHcpp::MosquitoFemaleHistory()
                              private$history$historyInit(privateEnv = private)

                            }, # end initializer

                            ##############################################################
                            # Pointers
                            ##############################################################

                            # MicroMosquitoPopFemale
                            get_FemalePopPointer = function(){
                              return(private$FemalePopPointer)
                            },
                            set_FemalePopPointer = function(FemalePopPointer){
                              private$FemalePopPointer = FemalePopPointer
                            },

                            # MicroMosquitoPopMale
                            get_MalePopPointer = function(){
                              return(private$MalePopPointer)
                            },
                            set_MalePopPointer = function(MalePopPointer){
                              private$MalePopPointer = MalePopPointer
                            },

                            # Landscape
                            get_LandscapePointer = function(){
                              return(private$LandscapePointer)
                            },
                            set_LandscapePointer = function(LandscapePointer){
                              private$LandscapePointer = LandscapePointer
                            },

                            # HumanPop
                            get_HumansPointer = function(){
                              return(private$HumansPointer)
                            },
                            set_HumansPointer = function(HumansPointer){
                              private$HumansPointer = HumansPointer
                            },

                            # MicroTile
                            get_TilePointer = function(){
                              return(private$TilePointer)
                            },
                            set_TilePointer = function(TilePointer){
                              private$TilePointer = TilePointer
                            },

                            ##############################################################
                            # Getters & Setters
                            ##############################################################

                            # Generic Fields

                            # ID and time
                            get_id = function(){
                              return(private$id)
                            },

                            get_bDay = function(){
                              return(private$bDay)
                            },

                            get_tNow = function(){
                              return(private$tNow)
                            },

                            get_tNext = function(){
                              return(private$tNext)
                            },

                            get_genotype = function(){
                              return(private$genotype)
                            },

                            # State & Location
                            get_state = function(){
                              return(private$state)
                            },

                            get_stateNew = function(){
                              return(private$stateNew)
                            },

                            get_inPointSet = function(){
                              return(private$inPointSet)
                            },

                            get_ix = function(){
                              return(private$ix)
                            },

                            get_mature = function(){
                              return(private$mature)
                            },

                            # Other State Variables
                            get_lspot = function(){
                              return(private$lspot)
                            },

                            get_damage = function(){
                              return(private$damage)
                            },

                            get_energy = function(){
                              return(private$energy)
                            },

                            # Female Fields

                            # Egg Batch Variables
                            get_bmSize = function(){
                              return(private$bmSize)
                            },

                            get_batch = function(){
                              return(private$batch)
                            },

                            get_eggT = function(){
                              return(private$eggT)
                            },

                            get_eggP = function(){
                              return(private$eggP)
                            },

                            # Maturation & Reproduction
                            get_sire = function(){
                              return(private$sire)
                            },

                            get_energyPreG = function(){
                              return(private$energyPreG)
                            },

                            get_hostID = function(){
                              return(private$hostID)
                            },

                            # Pathogens
                            get_Pathogens = function(){
                              return(private$Pathogens)
                            },
                            init_Pathogens = function(){
                              private$Pathogens = NULL
                            },

                            ##############################################################
                            # Return Data from MosquitoFemaleHistory object
                            ##############################################################

                            # history
                            get_history = function(){
                              return(
                                private$history$exportHistory()
                              )
                            },

                            # bionomics
                            get_bionomics = function(){
                              return(
                                private$history$exportBionomics()
                              )
                            }

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
                            state      = character(1),       # {F,B,R,L,O,S,M,E,D}
                            stateNew   = character(1),       # {F,B,R,L,O,S,M,E,D}
                            inPointSet = character(1),       # class of site {f,l,s,m}
                            ix         = integer(1),       # index of site
                            mature     = logical(1),       # mature

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
                            sire        = integer(1),
                            energyPreG  = numeric(1),          # pre-gonotrophic energy requirement
                            hostID      = integer(1),           # the id of the host: -1::none; 0::not human

                            # Pathogens
                            Pathogens      = NULL,

                            # Pointers
                            FemalePopPointer = NULL,  # Point to enclosing MicroMosquitoPopFemale
                            MalePopPointer = NULL,    # Point to MicroMosquitoPopMale in the same microsimulation Tile
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
                        inherit = MicroMosquito,
                        portable = TRUE,
                        cloneable = FALSE,
                        lock_class = FALSE,
                        lock_objects = FALSE,

                        # public members
                        public = list(),

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
                          inPointSet = character(1),       # class of site {f,l,s,m}
                          ix         = integer(1),       # index of site
                          mature     = logical(1),       # mature

                          # Other State Variables
                          lspot     = character(1),        # landing spot (i: inside wall, w: outside wall, v: outside vegetation, r: feed, l: leave)
                          damage    = numeric(1),        # wing tattering
                          energy    = numeric(1),        # energy reserves

                          history = NULL,          # history

                          # Pointers
                          FemalePopPointer = NULL,  # Point to enclosing MicroMosquitoPopFemale
                          MalePopPointer = NULL,    # Point to MicroMosquitoPopMale in the same microsimulation Tile
                          LandscapePointer = NULL,  # Point to Landscape object in same microsimulation Tile
                          HumansPointer = NULL,     # Point to HumanPop object in same microsimulation Tile
                          TilePointer = NULL        # Point to enclosing microsimulation Tile

                        )
)
