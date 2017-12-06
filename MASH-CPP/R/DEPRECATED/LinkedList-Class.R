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
#   Biyonka Liang & Sean Wu
#   August 18, 2017
#
###############################################################################

###############################################################################
# Double Linked List: Class Definition
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

dLinkedList <- R6::R6Class(classname="dLinkedList",
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
                        #  private$storage = new.env(parent = create_emptyenv())
                        # just want to leave parent blank because then the parent will be the enclosing R6 object's envir
                         private$storage = new.env(hash = FALSE, size = N)
                       }

                     ),

                     #private members
                     private = list(

                        storage = NULL

                     )

) #end class definition



###############################################################################
# Sean Tests
###############################################################################



###############################################################################
# dLinkedList: Class Methods
###############################################################################

#' dLinkedList: Check Emptiness
#'
#' Checks if linked list is empty
#'
#' @param llist is a doubly linked list
#'

isEmpty_dLinkedList <- function(llist) {
  if(class(llist)!= "dLinkedList") warning("Not dLinkedList class")
  identical(private$storage, new.env(parent = create_emptyenv()))
}

dLinkedList$set(which = "public",name = "isEmpty",
  value = isEmpty_dLinkedList, overwrite = TRUE
)


# isEmpty_dLinkedList <- function(){
#   numKeys = length(ls(envir = private$storage,all.names = TRUE))
#   if(numKeys == 0){
#     return(TRUE)
#   } else if(numKeys > 0){
#     return(FALSE)
#   }
# }


#' dLinkedList: Node Creation
#'
#' Initializes a double linked list node
#'
#' @param val is a primitive type, prevnode and node are either NULL or other doubly linked lists
#'

makeNode_dLinkedList <- function(val, prevnode=NULL, nextnode=NULL) {
  llist <- new.env(parent=private$storage)
  llist$prevnode <- prevnode
  llist$element <- val
  llist$nextnode <- node
  class(llist) <- "dlinkList"
  llist
}

dLinkedList$set(which = "public",name = "makeNode",
  value = makeNode_dLinkedList, overwrite = TRUE
)

# makeNode_dLinkedList <- function(value, prevnode = NULL, nextnode = NULL){
#   node = new.env(hash = FALSE, size = 3L)
#   node$element = value
#   node$nextnode = nextnode
#   node$prevnode = prevnode
#
# }


#' dLinkedList: Add Element
#'
#' Adds element to end of a doubly linked list
#'
#' @param llist is a doubly linked list
#'
addElem_dLinkedList <-function(new, prev, nex){
  #if (isEmpty_dLinkedList(prev)) {
   # assign(element = val, nextnode = nex, envir = list2env(prev))
   # }
 # else {
    llist<-makeNode(val=new, prevnode=prev, nextnode=nex)
   # }
llist
}

dLinkedList$set(which = "public",name = "addElem",
  value = addElem_dLinkedList,
  overwrite = TRUE)

#' dLinkedList: Return Size of List
#'
#' Returns the total number of nodes in the list
#'
#' @param llist is a doubly linked list
#'
size_dLinkedList<-function(llist, size=0){
  if (isEmpty(llist)) {
    return(size)
    }
  else {
    size<-size+1L
    size_dLinkedList(llist$nextnode, size)
    }
}

dLinkedList$set(which = "public",name = "size",
  value = size_dLinkedList,
  overwrite = TRUE)


#' dLinkedList: Check if Value Exists
#'
#' Does a value exist in \code{private$storage} or not.
#'
#' @param llist is an instance of the doubly linked list class
#'
exists_dLinkedList<-function(llist, value, exists = FALSE){
  if (isEmpty(llist)) {
    return(exists)
    }
  else if (llist$element == value) {
    exists <- TRUE
    return (exists)
  }
  else {
    exists_dLinkedList(llist$nextnode, value, exists)
    }
}


dLinkedList$set(which = "public",name = "exists",
  value = exists_dLinkedList,
  overwrite = TRUE)

#' dLinkedList: Views all Objects in a doubly linked list
#'
#' Returns node elements of all nodes in the environment.
#'
ls_dLinkedList <- function(){
  ls(env = private$storage)
}

dLinkedList$set(which = "public",name = "ls",
  value = ls_dLinkedList,
  overwrite = TRUE)

#' dLinkedList: Check if Value Exists
#'
#' Does a value exist in \code{private$storage} or not.
#'
#' @param llist is an instance of the doubly linked list class
#'
# delElement<-function(llist, pos=NULL){
# if(is.null(pos)) warning("Nothing to delete")
# listsize<-sizeLinkList(llist)
# if(pos>listsize) stop("Position greater than size of list")
# if (isEmpty(llist)) {
# warning("Empty List")
# } else if(pos==1){
# PreviousNode<-llist$nextnode
# } else
# {
# PreviousNode<-linkListNode(llist$element)
# for(i in 1:(listsize-1)){
# if(pos==(i+1)){
# PreviousNode$nextnode<-setNextNode(llist$nextnode)
# } else
# {
# PreviousNode$nextnode<-llist$nextnode
# llist<-llist$nextnode
# }
# }
# }
# return(PreviousNode)
# }



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
