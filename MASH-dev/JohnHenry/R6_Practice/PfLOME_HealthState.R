HealthState <- R6Class("HealthState",
                       
                       public = list(
                         
                         initialize = function(){
                           private$Fever = FALSE
                           private$feverThresh = 8
                           private$HRP2 = 0
                           private$RBC = log10(5*10^13)
                           private$pLDH = 0
                           private$history = list()
                         },
                         
                         
                         ####### Accessors ########
                         
                         
                         get_Fever = function(){
                           private$Fever
                         },
                         set_Fever = function(newFever){
                           private$Fever = newFever
                         },
                         get_HRP2 = function(){
                           private$HRP2
                         },
                         set_HRP2 = function(newHRP2){
                           private$HRP2 = newHRP2
                         },
                         get_pLDH = function(){
                           private$pLDH
                         },
                         set_pLDH = function(newpLDH){
                           private$pLDH = newpLDH
                         },
                         get_RBC = function(){
                           private$RBC
                         },
                         set_RBC = function(newRBC){
                           private$RBC = newRBC
                         },
                         get_history = function(){
                           private$history
                         },
                         
                         
                         ############ Update Methods ##############
                         
                         
                         update_healthState = function(){
                           self$update_Fever()
                           self$update_HRP2()
                           self$update_pLDH()
                           self$update_RBC()
                         },
                         
                         update_Fever = function(){
                           #print(pathogen$get_Ptot())
                         },
                         
                         update_HRP2 = function(){
                           
                         },
                         
                         update_pLDH = function(){
                           
                         },
                         
                         update_RBC = function(){
                           
                         },
                         
                         update_history = function(){
                           private$history$Fever = c(private$history$Fever,private$Fever)
                           private$history$HRP2 = c(private$history$HRP2,private$HRP2)
                           private$history$pLDH = c(private$history$pLDH,private$pLDH)
                           private$history$RBC = c(private$history$RBC,private$RBC)
                         }
                         
                       ),
                       
                       
                       private = list(
                         Fever = NULL,
                         feverThresh = NULL,
                         HRP2 = NULL,
                         pLDH = NULL,
                         RBC = NULL,
                         history = NULL
                       )
                       
)

