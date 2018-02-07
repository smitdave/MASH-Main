source("PfSI_HealthState.R")
source("PfSI_ImmuneState.R")

Human <- R6Class("Human",
                 
                 ## Public Fields, Methods, and Initialization
                 public = list(
                   
                   ## Initialization of Components
                   
                   initialize = function(ixH = NA, age = NA, sex = NA, locH = NA){
                     private$ixH = ixH
                     private$pathogen = Pathogen$new()
                     private$healthState = HealthState$new()
                     private$history = list()
                     
                   },
                   
                   
                   ######## Infection Methods #########
                   
                   
                   infectHuman = function(t,pfid){
                     mic = pfped$get_mic(pfid)
                     mac = pfped$get_mac(pfid)
                     gtype = pfped$get_gtype(pfid)
                     BSImm = private$immuneState$get_BSImm()
                     #typeImm = private$immuneState$get_typeImm(t,ptype)
                     private$pathogen$add_Pf(t,pfid,mic,mac,gtype,BSImm)
                     pfped$set_th(pfid,t)
                   },
                   ## write method to remove particular infection
                   clearPathogen = function(t, pfid){
                     private$pathogen$PfPathogen[[pfid]] = NULL
                     private$pathogen$set_PfMOI(private$pathogen$get_PfMOI()-1)
                     pfped$set_thEnd(pfid,t)
                   },
                   infectMosquito = function(t, pfid, ixm){
                     
                   },
                   
                   moveHuman = function(newlocH){
                     self$set_locH(newlocH)
                   },
                   
                   Treat = function(t,Drug){
                     private$healthState$Treat(t,Drug)
                   },
                   
                   ########## Update Function #########
                   
                   ## **edit updateHuman to allow for variable update steps **
                   
                   updateHuman = function(t,dt){
                     private$immuneState$update_immuneState(t,dt,self$get_Ptot())
                     private$healthState$update_healthState(t,dt,self$get_Ptot(),self$get_history()$RBC)
                     private$pathogen$update_pathogen(t,dt,self$get_PD())
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
                   
                   set_age = function(newAge){
                     private$age = newAge
                   },
                   
                   get_sex = function(){
                     private$sex
                   },
                   
                   set_sex = function(newSex){
                     private$sex = newSex
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
                   
                   get_pLDH = function(){
                     private$healthState$get_pLDH()
                   },
                   
                   get_Ptot = function(){
                     private$pathogen$get_Ptot()
                   },
                   
                   get_Gtot = function(){
                     private$pathogen$get_Gtot()
                   },
                   
                   get_Drug = function(){
                     private$healthState$get_Drug()
                   },
                   
                   get_RxStart = function(){
                     private$healthState$get_RxStart()
                   },
                   
                   get_PD = function(){
                     private$healthState$get_PD()
                   },
                   
                   get_Fever = function(){
                     private$healthState$get_Fever()
                   },
                   
                   get_PfMOI = function(){
                     private$pathogen$get_PfMOI()
                   },
                   
                   get_history = function(){
                     c(private$pathogen$get_history(), 
                       private$healthState$get_history(),
                       private$immuneState$get_history())
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
