###############################################################################
#       __  __           __    __  ___
#      / / / /___ ______/ /_  /  |/  /___ _____
#     / /_/ / __ `/ ___/ __ \/ /|_/ / __ `/ __ \
#    / __  / /_/ (__  ) / / / /  / / /_/ / /_/ /
#   /_/ /_/\__,_/____/_/ /_/_/  /_/\__,_/ .___/
#                                      /_/
#     Hash Table
#     MBITES Team
#     February 2018
#
###############################################################################


#' Map a function over a hash table
#'
#' This is an internal function for MBITES that rewrites eapply such that
#' no memory is allocated for return values, the function is mapped with optional
#' arguments across all objects in the hash table.
#' It is only for functions being called for their side effects.
#'
#'
#' @param X a hashed \code{\link[base]{environment}}
#' @param FUN a function
#' @param ... optional named arguments
#'
#' @name hash_apply
#' @useDynLib MBITES hash_apply_c
#'
hash_apply <- function(X,FUN,...){
  call <- match.call(expand.dots = FALSE)
  .Call(hash_apply_c,call,environment())
}

#' Get size of hash table
#'
#' Return the total number of objects stored in the hash table.
#'
#' @param rho a hashed \code{\link[base]{environment}}
#'
#' @name hash_size
#' @useDynLib MBITES hash_size_c
hash_size <- function(rho){
  .Call(hash_size_c,rho)
}

###############################################################################
# HashMap: Class Definition
###############################################################################

#'  HashMap Class Definition
#'
#'  Provide a R6 implementation of a hash table to store population vectors using native R environments.
#'  Note that the 'keys' are integers, automatically coerced to characters in the accessing methods.
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
#'  * rm_all: see \code{\link{rm_all_HashMap}}
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
                     cloneable = FALSE,
                     lock_class = FALSE,
                     lock_objects = FALSE,

                     #public members
                     public = list(

                       # begin constructor
                       initialize = function(N = 100L){
                         private$storage = new.env(hash=TRUE,size=N)
                       }, # end constructor

                       # begin destructor
                       finalize = function(){
                         self$rm_all()
                         invisible(gc())
                       } # end destructor

                     ),

                     #private members
                     private = list(storage = NULL)

) #end class definition


###############################################################################
# HashMap: Class Methods
###############################################################################

#' HashMap: Value Removal Method
#'
#' Remove a value from \code{private$storage} given a key.
#'
#' @param key a integer key (will be coerced to character); remove that value from the storage hash table.
#'
rm_HashMap <- function(key){
  rm(list = as.character(key),envir = private$storage,inherits = FALSE)
}

HashMap$set(which = "public",name = "rm",
  value = rm_HashMap, overwrite = TRUE
)

#' HashMap: Remove all Values
#'
#' Remove all values from \code{private$storage} and call garbage collection.
#'
rm_all_HashMap <- function(){
  rm(list = ls(env = private$storage),envir = private$storage,inherits = FALSE)
}

HashMap$set(which = "public",name = "rm_all",
  value = rm_all_HashMap, overwrite = TRUE
)


#' HashMap: Check if Value Exists
#'
#' Does a value exist in \code{private$storage} or not.
#'
#' @param key given a character key return a boolean if the value associated to that key exists in the storage hash table
#'
exists_HashMap <- function(key){
  exists(x = as.character(key),envir = private$storage,inherits = FALSE)
}

HashMap$set(which = "public",name = "exists",
  value = exists_HashMap, overwrite = TRUE
)

#' HashMap: Views all Objects in Hash Table
#'
#' Returns character vector of all keys in the environment.
#'
ls_HashMap <- function(){
  ls(env = private$storage)
}

HashMap$set(which = "public",name = "ls",
  value = ls_HashMap, overwrite = TRUE
)

#' HashMap: Assign a Key-Value Pair to the Hash Table
#'
#' Assign a key-value pair to \code{private$storage}.
#'
#' @param key integer key
#' @param value a value to be assigned
#'
assign_HashMap <- function(key, value){
  assign(x = as.character(key), value = value, envir = private$storage,inherits = FALSE)
}

HashMap$set(which = "public",name = "assign",
  value = assign_HashMap, overwrite = TRUE
)

#' HashMap: Get the Value Assigned to a Key
#'
#' Assign a key-value pair to \code{private$storage}.
#'
#' @param key key value (usually integer, will be coerced to character)
#' @param value a value to be assigned
#'
get_HashMap <- function(key){
  get(x = as.character(key),envir = private$storage,inherits = FALSE)
}

HashMap$set(which = "public",name = "get",
  value = get_HashMap, overwrite = TRUE
)


###############################################################################
# HashMap: Apply Method Over Environment
###############################################################################

#' MASH-CPP: HashMap Call a Method Over All Values in HashMap
#'
#' Use \code{\link[base]{eapply}} to call a method found by argument \code{tag} for all values stored in the HashMap \code{private$storage}.
#' The HashMap class is intended to only store instantiations of R6 objects from a single class, it must not store mixed data types.
#'
#' @param tag character tag of the method to call for each element in the HashMap
#' @param return do we want to capture the ouput of \code{\link[base]{eapply}} and return; if \code{FALSE} apply the function invisibly and return \code{NULL}
#' @param ... additional named arguments passed to method
#'
eapply_HashMap <- function(tag, return = FALSE, ...){
  # if we want the output
  if(return){
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
  }
}

HashMap$set(which = "public",name = "apply",
  value = eapply_HashMap, overwrite = TRUE
)
