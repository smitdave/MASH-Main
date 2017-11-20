HealthState <- R6Class("HealthState",
                       
                       public = list(
                         
                         initialize = function(){
                           private$Fever = FALSE
                           private$HRP2 = 0
                           private$RBC = log10(5*10^13)
                           private$pLDH = 0
                         },
                         
                         get_Fever = function(){
                           private$Fever
                         },
                         set_Fever = function(newFever){
                           private$Fever <<- newFever
                         },
                         get_HRP2 = function(){
                           private$HRP2
                         },
                         set_HRP2 = function(newHRP2){
                           private$HRP2 <<- newHRP2
                         },
                         get_pLDH = function(){
                           private$pLDH
                         },
                         set_pLDH = function(newpLDH){
                           private$pLDH <<- newpLDH
                         },
                         get_RBC = function(){
                           private$RBC
                         },
                         set_RBC = function(newRBC){
                           private$RBC <<- newRBC
                         },
                         
                         update_healthState = function(){
                           set_HRP2(update_HRP2(private$HRP2))
                           set_pLDH(update_pLDH(private$pLDH))
                           set_RBC(update_RBC(private$RBC))
                         }
                       ),
                       
                       private = list(
                         Fever = NULL,
                         HRP2 = NULL,
                         pLDH = NULL,
                         RBC = NULL,
                         Ptot = NULL,
                         Gtot = NULL,
                         Stot = NULL
                       )
                       
)

