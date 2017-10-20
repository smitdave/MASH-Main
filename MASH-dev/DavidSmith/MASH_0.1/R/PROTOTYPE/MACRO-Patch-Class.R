#######################################################################
#
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   MACRO: MacroPatch Class Definition
#   David Smith, Hector Sanchez, Sean Wu
#   August 16, 2017
#
#######################################################################

#' MACRO MacroPatch Class Definition
#'
#' This is an R6 class definition for a well-mixed patch in a macrosimulation tile (\code{\link{MacroTile}}).
#'
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section Fields:
#' * **Some Fields**
#'    * I'm a field!: Talk about this field!
#'    * I'm a field!: Talk about this field!
#'    * I'm a field!: Talk about this field!
#'    * I'm a field!: Talk about this field!
#' * **Some Other Fields**
#'    * I'm a field!: Talk about this field!
#'      * Detail 1: some details!
#'      * Detail 2: some details!
#'    * I'm a field!
#' @section Methods:
#'  * **Constructor**
#'    * new: initialize a new \code{MacroPatch} object
#'      * Arguments:
#'        * arg1: something.
#'  * **Getters & Setters**
#'    * Get a field!:
#'  * **Pointers**
#'    * A pointer!: it's pointers all the way down, then its a memory leak.
#'
#' @md
#' @export
MacroPatch <- R6::R6Class(classname = "MacroPatch",
            portable = TRUE,
            cloneable = FALSE,
            lock_class = FALSE,
            lock_objects = FALSE,

            # public memebers
            public = list(

                #################################################
                # Constructor
                #################################################

                initialize = function(){


                # end constructor
                },

                #################################################
                # Getters & Setters
                #################################################

                get_patchID = function(){
                  return(private$patchID)
                },
                set_patchID = function(patchID){
                  private$patchID = patchID
                },

                get_MosquitoPop = function(){
                  return(private$MosquitoPop)
                },
                set_MosquitoPop = function(MosquitoPop){
                  private$MosquitoPop = MosquitoPop
                },

                # Pointers
                get_TilePointer = function(){
                  return(private$TilePointer)
                },
                set_TilePointer = function(TilePointer){
                  private$TilePointer = TilePointer
                },

                get_HumansPointer = function(){
                  return(private$HumansPointer)
                },
                set_HumansPointer = function(HumansPointer){
                  private$HumansPointer = HumansPointer
                }

              ),

            # private members
            private = list(

                patchID = NULL,

                MosquitoPop = NULL,


                # pointers
                TilePointer = NULL, # point to the enclosing metapopulation TILE
                HumansPointer = NULL # point to the HumanPop in this enclosing metapopulation TILE

              )
)
