source("PfLOME_HealthState.R")
source("PfLOME_ImmuneState.R")

Human <- R6Class("Human",
                 
                 ## Public Fields, Methods, and Initialization
                 public = list(
                   
                   ## Initialization of Components
                   
                   initialize = function(ixH = NA, age = NA, sex = NA, locH = NA){
                     private$ixH = ixH
                     private$age = age
                     private$sex = sex
                     private$locH = locH
                     private$immuneState = ImmuneState$new()
                     private$healthState = HealthState$new()
                     private$pathogen = Pathogen$new()
                     private$history = list()
                   },
                   
                   
                   ######## Infection Methods #########
                   
                   
                   infectHuman = function(t,pfid){
                     mic = pfped$get_mic(pfid)
                     mac = pfped$get_mac(pfid)
                     gtype = pfped$get_gtype(pfid)
                     private$pathogen$add_Pf(t,pfid,mic,mac,gtype)
                   },
                   ## write method to remove particular infection
                   clearPathogen = function(pfid){
                     private$pathogen$PfPathogen[[pfid]] = NULL
                     private$pathogen$set_PfMOI(private$pathogen$get_PfMOI()-1)
                   },
                   infectMosquito = function(){
                     
                   },
                   
                   
                   ########## Update Function #########
                   
                   
                   updateHuman = function(t){
                     #private$immuneState$update_immuneState(t)
                     private$healthState$update_healthState(self$get_Ptot(),self$get_history()$RBC)
                     private$pathogen$update_pathogen(t)
                   },
                   
                   
                   ########## Accessors ##############
                   
                   
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
                   
                   get_Ptot = function(){
                     private$pathogen$get_Ptot()
                   },
                   
                   get_Gtot = function(){
                     private$pathogen$get_Gtot()
                   },
                   
                   get_history = function(){
                     c(private$pathogen$get_history(), private$healthState$get_history())
                     #private$immuneState$get_history()
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
                   healthState = NULL,
                   history = NULL
                   
                 )
                 
)


