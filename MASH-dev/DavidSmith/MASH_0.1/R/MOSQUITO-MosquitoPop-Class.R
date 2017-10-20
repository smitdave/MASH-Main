#################################################################
#
#   MASH
#   MICRO Mosquito Populations
#   Class Definitions
#   David Smith, Hector Sanchez, Sean Wu
#   April 28, 2017
#
#################################################################


#################################################################
# Female Mosquito Population Class
#################################################################

# MicroMosquitoPopFemale:
MicroMosquitoPopFemale <- R6::R6Class(classname = "MicroMosquitoPopFemale",
                       portable = TRUE,
                       cloneable = FALSE,
                       lock_class = FALSE,
                       lock_objects = FALSE,

                       public = list(

                         ##############################################################
                         # Initializer
                         ##############################################################

                         # N: size of initial cohort
                         # ix_init should be a vector of initial site index
                         # genotype_init should be a vector of genotypes
                         initialize = function(N, time_init, ix_init, genotype_init, MBITES_PAR, module){

                             # Initialize population level fields prior to allocating container
                             private$MBITES_PAR = MBITES_PAR
                             switch(module,
                                BRO = {private$initState = "B"},
                                BROM = {private$initState = "M"},
                                BROS = {private$initState = "B"},
                                BROMS = {private$initState = "M"},
                                FULL = {private$initState = "M"},
                                {stop("unrecognized M-BITES lifecycle module selection")}
                              )

                             # allocate population
                             private$pop = vector(mode="list",length=(N*5))
                             if(length(ix_init) != length(genotype_init)){
                               stop("one or more of the input vectors to MicroMosquitoPopFemale initializer is not the same length")
                             }

                             # allocate initial cohort
                             for(ix in 1:N){
                               private$pop[[ix]] = MicroMosquitoFemale$new(id = paste0(time_init,"_",ix), time = time_init, ix = ix_init[ix], genotype = genotype_init[ix], state = private$initState)
                               private$pop[[ix]]$set_FemalePopPointer(self)
                             }
                             # find NULL indices
                             self$update_nullPop()

                         }, # end initializer

                         #################################################################
                         # Getters & Setters
                         #################################################################

                         get_MosquitoIxM = function(ixM = NULL){
                           if(is.null(ixM)){
                             return(private$pop)
                           } else {
                             return(private$pop[[ixM]])
                           }
                         },
                         get_MosquitoID = function(ID){
                           stop("this hasn't been written yet")
                         },

                        #  which_alive = NULL,

                         # getter for nullPop
                         get_nullPop = function(){return(private$nullPop)},
                         # update nullPop
                        #  update_nullPop = NULL,

                         # return parameter list or element of list by name
                         get_MBITES_PAR = function(ixP = NULL){
                           if(is.null(ixP)){
                             return(private$MBITES_PAR)
                           } else {
                             return(private$MBITES_PAR[[ixP]])
                           }
                         },
                         set_MBITES_PAR = function(MBITES_PAR){
                           private$MBITES_PAR = MBITES_PAR
                         },

                         ##############################################################
                         # Cohort Methods
                         ##############################################################

                         # push_pop: from a single ImagoSlot; add a cohort.
                        #  push_pop = NULL,

                         # extend_pop: extend the pop vecor
                        #  extend_pop = NULL,

                         # clear_pop: manage the pop vector (find dead mosquitoes; if 'con' is provided, write their histories out to JSON)
                        #  clear_pop = NULL,

                        #  update_pop = NULL


                         #################################################################
                         # Pointers
                         #################################################################

                         get_MalePopPointer = function(){
                           return(private$MalePopPointer)
                         },
                         set_MalePopPointer = function(MalePopPointer){
                           private$MalePopPointer = MalePopPointer
                         },

                         get_LandscapePointer = function(){
                           return(private$LandscapePointer)
                         },
                         set_LandscapePointer = function(LandscapePointer){
                           private$LandscapePointer = LandscapePointer
                         },

                         get_HumansPointer = function(){
                           return(private$HumansPointer)
                         },
                         set_HumansPointer = function(HumansPointer){
                           private$HumansPointer = HumansPointer
                         },

                         get_TilePointer = function(){
                           return(private$TilePointer)
                         },
                         set_TilePointer = function(TilePointer){
                           private$TilePointer = TilePointer
                         }

                       ),

                       private = list(

                         # Fields
                         pop = NULL,               # mosquito population
                         nullPop = NULL,           # null entries in list for memory allocation
                         initState = NULL,         # initial state for newly emerging females
                         MBITES_PAR = NULL,        # MBITES Parameters

                         # Pointers
                         MalePopPointer = NULL,    # Point to MicroMosquitoPopMale in the same microsimulation Tile
                         LandscapePointer = NULL,  # Point to Landscape object in same microsimulation Tile
                         HumansPointer = NULL,     # Point to HumanPop object in same microsimulation Tile
                         TilePointer = NULL        # Point to enclosing microsimulation Tile

                       )
)


#################################################################
# Male Mosquito Population Class
#################################################################

MicroMosquitoPopMale <- R6::R6Class(classname = "MicroMosquitoPopMale",
                             portable = TRUE,
                             cloneable = FALSE,
                             lock_class = FALSE,
                             lock_objects = FALSE,

                             public = list(

                               #################################################################
                               # Getters & Setters
                               #################################################################

                               get_MosquitoIxM = function(ixM = NULL){
                                 if(is.null(ixM)){
                                   return(private$pop)
                                 } else {
                                   return(private$pop[[ixM]])
                                 }
                               },
                               get_MosquitoID = function(ID){
                                 stop("this hasn't been written yet")
                               },

                               # getter for nullPop
                               get_nullPop = function(){return(private$nullPop)},
                               # update nullPop
                               update_nullPop = function(){
                                 private$nullPop = which(vapply(X = private$pop,FUN = is.null,FUN.VALUE = logical(1)))
                               },

                               # return parameter list or element of list by name
                               get_MBITES_PAR = function(ixP = NULL){
                                 if(is.null(ixP)){
                                   return(private$MBITES_PAR)
                                 } else {
                                   return(private$MBITES_PAR[[ixP]])
                                 }
                               },
                               set_MBITES_PAR = function(MBITES_PAR){
                                 private$MBITES_PAR = MBITES_PAR
                               },

                               #################################################################
                               # Pointers
                               #################################################################

                               get_LandscapePointer = function(){
                                 return(private$LandscapePointer)
                               },
                               set_LandscapePointer = function(LandscapePointer){
                                 private$LandscapePointer = LandscapePointer
                               },

                               get_HumansPointer = function(){
                                 return(private$HumansPointer)
                               },
                               set_HumansPointer = function(HumansPointer){
                                 private$HumansPointer = HumansPointer
                               },

                               get_TilePointer = function(){
                                 return(private$TilePointer)
                               },
                               set_TilePointer = function(TilePointer){
                                 private$TilePointer = TilePointer
                               }


                             ),

                             private = list(

                               # Fields
                               pop = NULL,               # mosquito population
                               nullPop = NULL,           # null entries in list for memory allocation
                               MBITES_PAR = NULL,        # MBITES Parameters

                               # Pointers
                               LandscapePointer = NULL,  # Point to Landscape object in same microsimulation Tile
                               HumansPointer = NULL,     # Point to HumanPop object in same microsimulation Tile
                               TilePointer = NULL        # Point to enclosing microsimulation Tile

                             )
)
