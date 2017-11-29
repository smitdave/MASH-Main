HealthState <- R6Class("HealthState",
                       
                       public = list(
                         
                         initialize = function(){
                           private$Fever = FALSE
                           private$feverThresh = 8
                           private$HRP2 = 0
                           private$RBC = 2.49
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
                         
                         
                         update_healthState = function(Ptot,RBCHist){
                           self$update_Fever(Ptot)
                           self$update_HRP2(Ptot)
                           self$update_pLDH(Ptot)
                           self$update_RBC(Ptot,RBCHist)
                           self$update_history()
                         },
                         
                         update_Fever = function(Ptot){
                           if(!is.na(Ptot)){
                             private$Fever = ifelse(Ptot >= private$feverThresh, TRUE, FALSE)
                           }
                         },
                         
                         update_HRP2 = function(Ptot){
                           a = .0019
                           b = log(2)/3.67
                           private$HRP2 = ifelse(is.nan(Ptot),log10(10^private$HRP2-b*10^private$HRP2),log10(10^private$HRP2+a*10^Ptot-b*10^private$HRP2))
                         },
                         
                         update_pLDH = function(Ptot){
                           a = .13
                           b = log(2)/2
                           private$pLDH = ifelse(!is.na(Ptot),
                                                 log10((1-b)*10^private$pLDH + a*10^Ptot),
                                                 log10((1-b)*10^private$pLDH))
                         },
                         
                         update_RBC = function(Ptot,RBCHist){
                           a = log(2)/120 #RBC halflife
                           b = 1
                           c = 1.7
                           d = .5
                           e = 5*10^9
                           rhat = ifelse(t<7,2.5,RBCHist[t-6])
                           r = private$RBC
                           private$RBC = ifelse(is.nan(Ptot),
                                                        r - a*r + b*exp(-c*rhat),
                                                        r - a*r + b*exp(-c*rhat) - d*10^Ptot/(e+10^Ptot)*r)
                         },
                         
                         update_history = function(){
                           private$history$Fever = c(private$history$Fever,private$Fever)
                           private$history$HRP2 = c(private$history$HRP2,private$HRP2)
                           private$history$pLDH = c(private$history$pLDH,private$pLDH)
                           private$history$RBC = c(private$history$RBC,private$RBC)
                         },
                         
                         
                         #################### Diagnostic Tests ####################
                         
                         
                         RDT = function(){
                           detect = 1
                           E1 = .1
                           E2 = .1
                           x = private$HRP2
                           p = E1+(1-E1-E2)*self$sigmoidX(x,detect,3,13)
                           return(rbinom(1,1,p))
                         },
                         
                         HSRDT = function(){
                           detect = 1
                           E1 = .1
                           E2 = .1
                           x = private$HRP2
                           p = E1+(1-E1-E2)*self$sigmoidX(x,detect,3,13)
                           return(rbinom(1,1,p))
                         },
                         
                         PCR = function(){
                           detect = 1
                           E1 = .1
                           E2 = .1
                           x = private$HRP2
                           p = E1+(1-E1-E2)*self$sigmoidX(x,detect,3,13)
                           return(rbinom(1,1,p))
                         },
                         
                         LAMP = function(){
                           detect = 1
                           E1 = .1
                           E2 = .1
                           x = private$HRP2
                           p = E1+(1-E1-E2)*self$sigmoidX(x,detect,3,13)
                           return(rbinom(1,1,p))
                         },
                         
                         LightMic = function(){
                           detect = 1
                           E1 = .1
                           E2 = .1
                           x = private$HRP2
                           p = E1+(1-E1-E2)*self$sigmoidX(x,detect,3,13)
                           return(rbinom(1,1,p))
                         },
                         
                         sigmoidX = function(X, X50=6, Xs=3, atMax=13){
                           pmin((1/(1+exp(-Xs*(X-X50))) - 1/(1+exp(Xs*X50)))/(1/(1+exp(-Xs*(atMax-X50))) - 1/(1+exp(Xs*X50))),1)
                         }
                         
                         
                         ####################### Rx methods #######################
                         
                         
                       ),
                       
                       
                       private = list(
                         Fever = NULL,
                         feverThresh = NULL,
                         HRP2 = NULL,
                         pLDH = NULL,
                         RBC = NULL,
                         Rx = NULL,
                         history = NULL
                       )
                       
)