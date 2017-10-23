#######################################################################
#
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   MACRO: MacroTile Class Definition
#   David Smith, Hector Sanchez, Sean Wu
#   August 16, 2017
#
#######################################################################

#' MACRO MacroTile Class Definition
#'
#' This is an R6 class definition for a macrosimulation tile.
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
MacroTile <- R6::R6Class(classname = "MacroPatch",
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

                MosquitoParameters = NULL

              )
)
