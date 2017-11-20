source("PfLOME_HealthState.R")
source("PfLOME_Pathogen.R")
source("PfLOME_ImmuneState.R")
source("PfLOME_PfPedigree.R")

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
                   },
                   
                   ## Human Methods
                   ## Infection Event
                   infectHuman = function(t,pf){ ## also pass pfid
                     #pfid = pf$get_pfid()
                     private$pathogen$add_Pf(t,pf)
                     private$pfPedigree$add2Pedigree(private$pathogen$get_Pf(pfid)) ### in real sim, this occurs in mosquito
                   },
                   ## write method to remove particular infection
                   clearPathogen = function(pfid){
                     private$pathogen[[pfid]] = NULL
                   },
                   infectMosquito = function(){
                     
                   },
                   ## Daily Update
                   dailyUpdate = function(){
                     private$pathogen$update_pathogen()
                     private$immuneState$update_immuneState()
                     private$healthState$update_healthState()
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
                     private$pathogen$get_Pf()
                   },
                   get_HRP2 = function(){
                     private$healthState$get_HRP2()
                   },
                   set_HRP2 = function(newHRP2){
                     private$healthState$set_HRP2(newHRP2)
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
                   pfPedigree = NULL
                   
                 )
                 
)


