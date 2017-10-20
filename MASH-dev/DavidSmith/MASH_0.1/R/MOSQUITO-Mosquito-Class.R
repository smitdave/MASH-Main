#################################################################
#
#   MASH
#   R6-ified
#   MICRO Class definition for mosquito
#   David Smith, Hector Sanchez, Sean Wu
#   April 25, 2017
#
#################################################################


#################################################################
# Female Mosquito Class
#################################################################


#' MICRO Female Mosquito Class Definition
#'
#' This is a female mosquito class definition for MICRO; it inherits (superclass) from \code{\link{MicroMosquito}}.
#'
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Fields:
#' * **ID and Time**
#'    * id: mosquito ID (integer)
#'    * bDay: time of emergence (numeric)
#'    * tNow: time of current behavioral state (numeric)
#'    * tNext: time to next behavioral state change (numeric)
#'    * genotype: genotype of mosquito (integer)
#' * **State and Location**
#'    * state: current behavioral state of mosquito (character)
#'      * F: Blood Feeding Search Bout
#'      * B: Blood Feeding Attempt Bout
#'      * R: Post-Prandial Resting Bout
#'      * L: Egg Laying Search Bout
#'      * O: Egg Laying Attempt Bout
#'      * S: Sugar Feeding Attempt Bout
#'      * M: Female Mating Bout
#'      * E: Estivation Bout
#'      * D: Death
#'    * stateNew: next behavioral state of mosquito (see above)
#'    * inPointSet: class of site (character)
#'      * f: feeding site \code{\link{FeedingSite}}
#'      * l: aquatic habitat \code{\link{AquaticSite}}
#'      * s: sugar feeding site \code{\link{SugarSite}}
#'      * m: mating site \code{\link{MatingSite}}
#'    * ix: index of site (integer)
#'    * MATURE: mature (logical)
#'    * ALIVE: alive or dead? (logical)
#' * **Other State Fields**
#'    * lspot: landing spot (character)
#'      * l: Leave the area
#'      * r: Reattempt Without Resting
#'      * v: Rest on vegetation
#'      * w: Rest on the Outside wall of a structure
#'      * i: Rest on the Inside wall of a structure
#'    * damage: wing tattering (numeric)
#'    * energy: energy reserves (numeric)
#'    * history: list; see \code{\link{MosquitoFemaleHistory}}
#' * **Egg Production**
#'    * bmSize: the size of the blood meal, relative to max
#'    * batch: female eggs in batch
#'    * eggT: the minimum time before eggs are mature
#'    * eggP: the mimimum provision for eggs to mature
#' * **Maturation & Reproduction**
#'    * sire: ID of mate
#'    * energyPreG: pre-gonotrophic energy requirement
#'    * hostID: the id of the host: -1 zoo, 0 null host, otherwise human ID
#' * **Pathogen Module Specific Fields**
#'    * Pathogen: Module specific Pathogen object
#'      * PfSI: \code{\link{mosquitoPfSI}}
#' @section Methods:
#'  * **Constructor**
#'    * new: initialize a new \code{MicroMosquitoFemale} object
#'      * Arguments:
#'        * arg1: something.
#'  * **Getters & Setters**
#'    * get_id:
#'  * **Pointers**
#'    * get_FemalePopPointer: get pointer to enclosing \code{\link{MicroMosquitoPopFemale}}
#'    * set_FemalePopPointer: set pointer to enclosing \code{\link{MicroMosquitoPopFemale}}
#'    * get_MalePopPointer: get pointer to \code{\link{MicroMosquitoPopMale}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * set_MalePopPointer: set pointer to \code{\link{MicroMosquitoPopMale}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * get_LandscapePointer: get pointer to \code{\link{Landscape}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * set_LandscapePointer: set pointer to \code{\link{Landscape}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * get_HumansPointer: get pointer to \code{\link{HumanPop}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * set_HumansPointer: set pointer to \code{\link{HumanPop}} in same enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * get_TilePointer: get pointer to enclosing microsimulation Tile \code{\link{MicroTile}}
#'    * set_TilePointer: set pointer to enclosing microsimulation Tile \code{\link{MicroTile}}
#'
#'
#' @md
#' @export
MicroMosquitoFemale <- R6::R6Class(classname = "MicroMosquitoFemale",
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
                              private$lspot     = 5L        # landing spot (i: inside wall, w: outside wall, v: outside vegetation, r: feed, l: leave)
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
                              private$history = MASH::MosquitoFemaleHistory()
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
                            id        = NULL,        # mosquito id
                            bDay      = NULL,        # time of emergence
                            tNow      = NULL,        # time of last event
                            tNext     = NULL,        # time to next event
                            genotype  = NULL,        # genotype of mosquito

                            # State and Location
                            state      = NULL,       # {F,B,R,L,O,S,M,E,D}
                            stateNew   = NULL,       # {F,B,R,L,O,S,M,E,D}
                            inPointSet = NULL,       # class of site {f,l,s,m}
                            ix         = NULL,       # index of site
                            mature     = NULL,       # mature
                            ALIVE      = NULL,       # alive or dead?

                            # Other State Variables
                            lspot     = NULL,        # landing spot (i: inside wall, w: outside wall, v: outside vegetation, r: feed, l: leave)
                            damage    = NULL,        # wing tattering
                            energy    = NULL,        # energy reserves

                            history   = NULL,        # history object

                            # Female Fields

                            # Egg Batch Variables
                            bmSize = 0,         # the size of the blood meal, relative to max
                            batch  = 0,         # female eggs in batch
                            eggT   = 0,         # the minimum time before eggs are mature
                            eggP   = 0,         # the mimimum provision for eggs to mature

                            # Maturation & Reproduction
                            sire        = 0,
                            energyPreG  = NULL,          # pre-gonotrophic energy requirement
                            hostID      = 0,           # the id of the host: -1::none; 0::not human

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


#################################################################
# Male Mosquito Class
#################################################################

#' MICRO Male Mosquito Class Definition
#'
#' This is a male mosquito class definition for MICRO; it inherits (superclass) from \code{\link{MicroMosquito}}.
#'
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Fields:
#' * **ID and Time**
#'    * id: mosquito ID (integer)
#'    * bDay: time of emergence (numeric)
#'    * tNow: time of current behavioral state (numeric)
#'    * tNext: time to next behavioral state change (numeric)
#'    * genotype: genotype of mosquito (integer)
#' * **State and Location**
#'    * state: current behavioral state of mosquito (character)
#'      * M: Male Mating Bout
#'      * S: Sugar Feeding Attempt Bout
#'      * R: Male Resting Bout
#' @section Methods:
#'  * **Constructor**
#'    * new: initialize a new \code{MicroMosquitoMale} object
#'      * Arguments:
#'        * arg1: something.
#'  * **Getters & Setters**
#'    * a getter:
#'  * **Pointers**
#'    * a pointer
#' @md
#' @export
MicroMosquitoMale <- R6::R6Class(classname = "MicroMosquitoMale",
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
                          id        = NULL,        # mosquito id
                          bDay      = NULL,        # time of emergence
                          tNow      = NULL,        # time of last event
                          tNext     = NULL,        # time to next event
                          genotype  = NULL,        # genotype of mosquito

                          # State and Location
                          state      = NULL,       # {F,B,R,L,O,S,M,E,D}
                          stateNew   = NULL,       # {F,B,R,L,O,S,M,E,D}
                          inPointSet = NULL,       # class of site {f,l,s,m}
                          ix         = NULL,       # index of site
                          mature     = NULL,       # mature
                          ALIVE      = NULL,       # alive or dead?

                          # Other State Variables
                          lspot     = NULL,        # landing spot (i: inside wall, w: outside wall, v: outside vegetation, r: feed, l: leave)
                          damage    = NULL,        # wing tattering
                          energy    = NULL,        # energy reserves

                          history = NULL,          # history

                          # Pointers
                          FemalePopPointer = NULL,  # Point to enclosing MicroMosquitoPopFemale
                          MalePopPointer = NULL,    # Point to MicroMosquitoPopMale in the same microsimulation Tile
                          LandscapePointer = NULL,  # Point to Landscape object in same microsimulation Tile
                          HumansPointer = NULL,     # Point to HumanPop object in same microsimulation Tile
                          TilePointer = NULL        # Point to enclosing microsimulation Tile

                        )
)
