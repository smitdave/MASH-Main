###############################################################################
#       __  __           __    __  ___
#      / / / /___ ______/ /_  /  |/  /___ _____
#     / /_/ / __ `/ ___/ __ \/ /|_/ / __ `/ __ \
#    / __  / /_/ (__  ) / / / /  / / /_/ / /_/ /
#   /_/ /_/\__,_/____/_/ /_/_/  /_/\__,_/ .___/
#                                      /_/
#
#   MASH-CPP
#   HashMap Class Definition
#   Sean Wu
#   August 18, 2017
#
###############################################################################

###############################################################################
# HashMap Class Definition
###############################################################################

#'  MASH-CPP: HashMap Class Definition
#'
#'  Provide a R6 implementation of a hash table to store population vectors using native R environments.
#'
#' @section Methods:
#'  * rm: given a character key remove that value from the storage hash table.
#'  * exists: given a character key return a boolean if the value associated to that key exists in the storage hash table
#'
#'
#'
#'
#'
#' @export
HashMap <- R6::R6Class(classname="Human",
                     portable = TRUE,
                     cloneable = TRUE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       #################################################
                       # Constructor
                       #################################################

                       initialize = function(N = 100L){

                         private$storage = new.env(hash=TRUE,size=N)

                       },

                     ),

                     #private members
                     private = list(

                       storage = NULL

                     )

) #end class definition


###############################################################################
# HashMap Class Methods
###############################################################################

#'  MASH-CPP: HashMap Value Removal Method
#'
#' Remove a value from \code{private$storage} given a key.
#'
#' @param key a character key remove that value from the storage hash table.
#'
rm_HashMap <- function(key){
  if(!is.character(key)){stop(paste0("key: ",key,"must be a character"))}
  rm(key,envir = private$storage,inherits = FALSE)
}

HashMap$set(which = "public",name = "rm",
  value = rm_HashMap,
  overwrite = TRUE)

#' MASH-CPP: HashMap Check if Value Exists
#'
#' Does a value exist in \code{private$storage} or not.
#'
#' @param key given a character key return a boolean if the value associated to that key exists in the storage hash table
#'
exists_HashMap <- function(key){
  if(!is.character(key)){stop(paste0("key: ",key,"must be a character"))}
  exists(x = key,envir = private$storage,inherits = FALSE)
}

HashMap$set(which = "public",name = "exists",
  value = exists_HashMap,
  overwrite = TRUE)
