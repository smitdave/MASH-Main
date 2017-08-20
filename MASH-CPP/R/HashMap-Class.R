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
# HashMap: Class Definition
###############################################################################

#'  HashMap Class Definition
#'
#'  Provide a R6 implementation of a hash table to store population vectors using native R environments.
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#'
#' @section **Constructor**:
#'  * N = 100L: size to allocate to the HashMap
#'
#' @section **Methods**:
#'  * rm: see \code{\link{rm_HashMap}}
#'  * exists: see \code{\link{exists_HashMap}}
#'  * ls: see \code{\link{ls_HashMap}}
#'  * assign: see \code{\link{assign_HashMap}}
#'  * get: see \code{\link{get_HashMap}}
#'  * eapply: see \code{\link{eapply_HashMap}}
#'
#' @section **Fields**:
#'  * storage: a hashed R environment, see \code{\link[base]{new.env}} for more details
#'
#' @export
HashMap <- R6::R6Class(classname="HashMap",
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
                       }

                     ),

                     #private members
                     private = list(

                       storage = NULL

                     )

) #end class definition


###############################################################################
# HashMap: Class Methods
###############################################################################

#' HashMap: Value Removal Method
#'
#' Remove a value from \code{private$storage} given a key.
#'
#' @param key a character key; remove that value from the storage hash table.
#'
rm_HashMap <- function(key){
  if(!is.character(key)){stop(paste0("key: ",key,"must be a character"))}
  rm(key,envir = private$storage,inherits = FALSE)
}

HashMap$set(which = "public",name = "rm",
  value = rm_HashMap,
  overwrite = TRUE)

#' HashMap: Check if Value Exists
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

#' HashMap: Views all Objects in Hash Table
#'
#' Returns character vector of all keys in the environment.
#'
ls_HashMap <- function(){
  ls(env = private$storage)
}

HashMap$set(which = "public",name = "ls",
  value = ls_HashMap,
  overwrite = TRUE)

#' HashMap: Assign a Key-Value Pair to the Hash Table
#'
#' Assign a key-value pair to \code{private$storage}.
#'
#' @param key character key
#' @param value a value to be assigned
#'
assign_HashMap <- function(key, value){
  if(!is.character(key)){stop(paste0("key: ",key,"must be a character"))}
  assign(x = key, value = value, envir = private$storage,inherits = FALSE)
}

HashMap$set(which = "public",name = "assign",
  value = assign_HashMap,
  overwrite = TRUE)

#' HashMap: Get the Value Assigned to a Key
#'
#' Assign a key-value pair to \code{private$storage}.
#'
#' @param key character key
#' @param value a value to be assigned
#'
get_HashMap <- function(key){
  if(!is.character(key)){stop(paste0("key: ",key,"must be a character"))}
  get(x = key,envir = private$storage,inherits = FALSE)
}

HashMap$set(which = "public",name = "get",
  value = get_HashMap,
  overwrite = TRUE)


###############################################################################
# HashMap: Apply Method Over Environment
###############################################################################

#' MASH-CPP: HashMap Call a Method Over All Values in HashMap
#'
#' Use \code{\link[base]{eapply}} to call a method found by argument \code{tag} for all values stored in the HashMap \code{private$storage}.
#' The HashMap class is intended to only store instantiations of R6 objects from a single class, it must not store mixed data types.
#'
#' @param tag character tag of the method to call for each element in the HashMap
#' @param returnVal do we want to capture the ouput of \code{\link[base]{eapply}} and return; if \code{FALSE} apply the function invisibly and return \code{NULL}
#' @param ... additional named arguments passed to method
#'
eapply_HashMap <- function(tag, returnVal = FALSE, ...){
  # if we want the output
  if(returnVal){
    return(
      eapply(env = private$storage,FUN = function(x,tag,...){
          x[[tag]](...)
        },tag=tag,... = ...,all.names = TRUE,USE.NAMES = TRUE)
    )
  # if we don't want the output (eventually need to write new version of eapply w/out alloc output memory)
  } else {
    invisible(
      eapply(env = private$storage,FUN = function(x,tag,...){
          x[[tag]](...)
        },tag=tag,... = ...,all.names = TRUE,USE.NAMES = FALSE)
    )
    return(NULL)
  }
}

HashMap$set(which = "public",name = "eapply",
  value = eapply_HashMap,
  overwrite = TRUE)
