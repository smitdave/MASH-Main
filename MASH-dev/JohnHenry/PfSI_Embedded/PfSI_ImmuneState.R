ImmuneState <- R6Class("ImmuneState",
                       
                       public = list(
                         
                         initialize = function(){
                           GenImm = 0
                         },
                         
                         get_history = function(){
                           private$history
                         },
                         
                         update_immuneState = function(Ptot, Rx){
                           
                           ## Ptot and Rx are binary; if either is 1, immunity is complete
                           private$GenImm = max(Ptot,Rx)
                           
                           ##history update
                           self$update_history()
                         },
                         
                         update_history = function(){
                           private$history$GenImm = c(private$history$GenImm, private$GenImm)
                         }
                         
                       ),
                       
                       ############### private fields #############
                       
                       
                       private = list(
                         GenImm = NULL,
                         history = NULL
                       )
                       
)