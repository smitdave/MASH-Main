#######################################################################
#
#       __  _______  _____ ____  __  ________________
#      /  |/  / __ \/ ___// __ \/ / / /  _/_  __/ __ \
#     / /|_/ / / / /\__ \/ / / / / / // /  / / / / / /
#    / /  / / /_/ /___/ / /_/ / /_/ // /  / / / /_/ /
#   /_/  /_/\____//____/\___\_\____/___/ /_/  \____/
#
#   MOSQUITO: MacroRM Mosquito Class
#   David Smith, Hector Sanchez, Sean Wu
#   August 16, 2017
#
#######################################################################

#' MACRO MacroRM Mosquito Class Definition
#'
#' This is an R6 class definition for a Ross-MacDonald mosquito population that lives in a \code{\link{MacroPatch}}
#' embedded in a network within a macrosimulation tile (\code{\link{MacroTile}}).
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
MacroRMMosquito <- R6::R6Class(classname = "MacroRMMosquito",
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



              ),

            # private members
            private = list(

                patchID = NULL, # patchID of the enclosing MacroPatch


                # pointers
                TilePointer = NULL, # point to the enclosing MacroTile
                PatchPointer = NULL, # point to the enclosing MacroPatch
                HumansPointer = NULL # point to the HumanPop in the enclosing MacroTile

              )
)
