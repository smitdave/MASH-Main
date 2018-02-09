HealthState <- R6Class("HealthState",
                       
                       public = list(
                         
                         initialize = function(){
                           private$Fever = FALSE
                           private$PD = 0
                           private$history = list()
                         },
                         
                         
                         ####### Accessors ########
                         
                         
                         get_Fever = function(){
                           private$Fever
                         },
                         set_Fever = function(newFever){
                           private$Fever = newFever
                         },
                         get_RxStart = function(){
                           private$RxStart
                         },
                         get_Drug = function(){
                           private$Drug
                         },
                         get_PD = function(){
                           private$PD
                         },
                         get_history = function(){
                           private$history
                         },
                         
                         
                         ############ Update Methods ##############
                         
                         
                         update_healthState = function(t,dt,Ptot){
                           self$update_Fever(Ptot)
                           self$update_PD(t)
                           self$update_history()
                         },
                         
                         update_Fever = function(Ptot){
                           if(!is.na(Ptot)){
                             private$Fever = ifelse(Ptot >= private$feverThresh, TRUE, FALSE)
                           }
                           if(is.na(Ptot)){
                             private$Fever = FALSE
                           }
                         },
                         
                         update_PD = function(t){
                           private$PD = self$getPD(t,private$RxStart,private$Drug)
                         },
                         
                         update_history = function(){
                           private$history$Fever = c(private$history$Fever,private$Fever)
                           private$history$PD = c(private$history$PD,private$PD)
                         },
                         
                         
                         #################### Diagnostic Tests ####################
                         
                         
                  
                         ####################### Rx methods #######################
                         
                         Treat = function(t,Drug){
                           private$RxStart = c(private$RxStart,t)
                           private$Drug = c(private$Drug,1)
                         }
                         
                         
                       ),
                       
                       
                       private = list(
                         Fever = NULL,
                         RxStart = NULL,
                         Drug = NULL,
                         PD = NULL,
                         history = NULL
                       )
                       
)