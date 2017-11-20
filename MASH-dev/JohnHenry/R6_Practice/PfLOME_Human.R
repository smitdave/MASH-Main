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
                     private$pfPedigree = PfPedigree$new()
                   },
                   
                   ## Human Methods
                   ## Infection Event
                   infectHuman = function(t){
                     pfid = private$pfPedigree$get_pfid()
                     private$pathogen$PfPathogen[[pfid]] = private$pathogen$add_Pf(t,pfid)
                     private$pfPedigree[[pfid]]$add2Pedigree(pfid)
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


############## artificial pedigree - will exist on tile ################

#PfPedigree = list()
#pfid = 0
#nAntigenLoci = 9
#nptypes = c(3,5,4,3,6,3,2,7,9)

