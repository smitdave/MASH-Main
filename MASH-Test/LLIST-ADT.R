# create_emptyenv <- function() {
#   emptyenv()
# }
# 
# isEmpty <- function(llist) {
#   if(class(llist)!= "linkList") warning("Not linkList class")
#   identical(llist, create_emptyenv())
# }
# 
# linkListNode <- function(val, node=NULL) {
#   llist <- new.env(parent=create_emptyenv())
#   llist$element <- val
#   llist$nextnode <- node
#   class(llist) <- "linkList"
#   llist
# }
# 
# LList <-linkListNode(5,linkListNode(2,create_emptyenv()))
# 
# setNextNode<-function(llist){
#   llist$nextnode
# }
# setNextElement<-function(llist){
#   llist$element
# }
# 
# sizeLinkList<-function(llist, size=0){
#   if (isEmpty(llist))
#   {
#     return(size)
#   } else
#   {
#     size<-size+1L
#     sizeLinkList(llist$nextnode, size)
#   }
# }
# 
# 
# addElement<-function(new, llist)
# {
#   if (isEmpty(llist)) {
#     llist<-linkedlist(new)
#   } else
#   {
#     llist<-linkListNode(llist, new)
#   }
#   llist
# }
