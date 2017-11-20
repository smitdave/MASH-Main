library(R6)

## Convention: Capitalize first letter for class definition, 
## lowercase first letter for particular instantiation within class

Pathogen <- R6Class("Pathogen",
                    
                    public = list(
                      
                      initialize = function(){
                      }
                    ),
                    
                    private = list(
                      
                    )
                    
                    )

Pf <- R6Class("Pf",
              
              inherit = Pathogen,
              
              public = list(
                initialize = function(t,pfid){
                  private$pfid = pfid
                  private$PAR = tentPAR(t,private.pfid)
                  private$gtype = getGtype(private.pfid)
                },
                get_gtype = function(){
                  private$gtype
                },
                set_gtype = function(newgtype){
                  private$gtype = newgtype
                },
                get_ptype = function(){
                  private$ptype
                },
                set_ptype = function(newptype){
                  private$ptype = newptype
                },
                
                ## update function
                update_Pathogen = function(t,Pt=private$Pt,PAR=private$PAR,PD=0,IM=0){
                  private$Pt = dPdt_tent(t,private$Pt,PAR,PD,IM)
                }
              ),
              
              private = list(
                ## tentPars
                PAR = NULL,
                ## Parasite Densities (Pt = Asexual, Gt = Gametocyte, 
                ## St = Sporozoite)
                activeP = NULL,
                activeG = NULL,
                activeS = NULL,
                Pt = NULL,
                Ptt = NULL,
                Ptot = NULL,
                Gt = NULL,
                Gtot = NULL,
                St = NULL,
                Stot = NULL,
                ## biological parameters
                gtype = NULL,
                ptype = NULL
              )
              
              )

ImmuneState <- R6Class("ImmuneState",
                       
                       private = list(
                         Counters = NULL
                       )
                       
)

 HealthState <- R6Class("HealthState",
                       
                       public = list(
                         
                         initialize = function(){
                           private$Fever = FALSE
                           private$HRP2 = 0
                           private$RBCCount = log10(5*10^13)
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
                         get_RBCCount = function(){
                           private$RBCCount
                         },
                         set_RBCCount = function(newRBCCount){
                           private$RBCCount <<- newRBCCount
                         },
                         update_ImmuneState = function(){
                         }
                       ),
                       
                       private = list(
                         Fever = NULL,
                         HRP2 = NULL,
                         pLDH = NULL,
                         RBCCount = NULL
                       )
                      
)

PfPedigree <- R6Class("PfPedigree",
                      
                      public = list(
                        initialize = function(){
                          private$pfid = 1
                        }
                      ),
                      
                      private = list(
                        
                        pfid = NULL,
                        gtype = NULL,
                        ptype = NULL,
                        mic = 1,
                        mac = 1
                        
                      )
)

Human <- R6Class("Human",
                 
                 ## Public Fields, Methods, and Initialization
                 public = list(
                   
                   ## Initialization of Components
                   initialize = function(ixH = NA, age = NA, sex = NA, locH = NA){
                     private$ixH = ixH
                     private$locH = locH
                     private$immuneState = ImmuneState$new()
                     private$healthState = HealthState$new()
                   },
                   
                   ## Human Methods
                   ## Infection Event
                   infectHuman = function(){
                     ## set adds pathogen without overwriting previous
                     private$pathogen = Pathogen$set(1)
                   },
                   ## write method to remove particular infection
                   clearPathogen = function(pfid){
                     private$pathogen
                   },
                   infectMosquito = function(){
                   },
                   ## Daily Update
                   dailyUpdate = function(){
                     private$pathogen$update_Pathogen()
                     private$immuneState$update_ImmuneState()
                     private$healthState$update_HealthState()
                   },
                   
                   ## Accessors
                   get_ixH = function(){
                     private$ixH
                   },
                   set_ixH = function(newixH){
                     private$ixH = newixH
                   },
                   get_age = function(){
                     private$age
                   },
                   get_sex = function(){
                     private$sex
                   },
                   get_locH = function(){
                     private$locH
                   },
                   set_locH = function(newlocH){
                     private$locH = newlocH
                   },
                   get_immuneState = function(){
                     private$immuneState
                   },
                   get_healthState = function(){
                     private$healthState
                   },
                   get_pathogen = function(){
                     private$pathogen
                   },
                   get_HRP2 = function(){
                     private$healthState$get_HRP2()
                   },
                   set_HRP2 = function(newHRP2){
                     private$healthState$set_HRP2(newHRP2)
                   },
                   
                   ## daily update functions
                   update_parasite = function(t,PAR,){
                     private$pathogen = update_Parasite
                   }
                   
                   ),
                 
                 ## Private Fields
                 private = list(
                   
                   ixH = NULL,
                   age = NULL,
                   sex = NULL,
                   locH = NULL,
                   pathogen = NULL,
                   immuneState = NULL,
                   healthState = NULL
                   
                 )
                 
)
