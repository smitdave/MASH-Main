###############################################################################
#       __  ___      ____  _____________________
#      /  |/  /     / __ )/  _/_  __/ ____/ ___/
#     / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#    / /  / /_____/ /_/ // /  / / / /___ ___/ /
#   /_/  /_/     /_____/___/ /_/ /_____//____/
#
#   MICRO
#   M-BITES: Timing
#   MASH Team
#   January 2018
#
###############################################################################

#################################################################
# M-BITES: Dwell Times
#################################################################

#' M-BITES: Simulates estivation \code{MosquitoFemale}
#'
#' The number estivationDay is a day of the year. This  
#' method checks to see if tNow < estivationDay < tNext. 
#' If so then, the mosquito estivates, which sets
#' tNext to a random number in the future, ttEstivate() 
#' 
#'  * This method is bound to \code{MosquitoFemale$timing()}.
#'
mbites_checkMating <- function(){
  if(private$mature == FALSE){ 
    #tSwarm is a time of day
    tSwarm = private$FemalePopPointer$get_MBITES_PAR("tSwarm")
    T1 = private$tNow - floor(private$tNow) 
    T2 = private$tNext - private$tNow 
    if((T1<tSwarm) & (T1+T2 > tSwarm)){ 
      private$state = 'M' 
      private$tNext = floor(private$tNow) + tSwarm 
    } 
    if((tSwarm>T1) & (T1+T2 > 1+tSwarm)){
      private$state = 'M' 
      private$tNext = floor(private$tNow) + tSwarm 
    }  
  } 
}
 
#' M-BITES: Simulates estivation \code{MosquitoFemale}
#'
#' The number estivationDay is a day of the year. This  
#' method checks to see if tNow < estivationDay < tNext. 
#' If so then, the mosquito estivates, which sets
#' tNext to a random number in the future, ttEstivate() 
#' 
#'  * This method is bound to \code{MosquitoFemale$timing()}.
#'
mbites_checkEstivation <- function(){
  #estivationDay is a day of the year, 0 <= estivationDay  <= 365 
  estivationDay = private$FemalePopPointer$get_MBITES_PAR("estivationDay")
  T1 = private$tNow%%365 
  T2 = private$tNext - private$tNow
  if(T1<estivationDay & T1+T2 > estivationDay){ 
    ttEstivate = private$FemalePopPointer$get_MBITES_PAR("ttEvent_Estivate")
    private$tNext =  private$tNext + ttEstivate()  
  }
}

#' M-BITES: Simulates the post-prandial resting period for \code{MosquitoFemale}
#'
#' Method checks to see if the mosquito has bloodfed and is in a
#' post prandial state; if so, it resets tNow and tNext to 
#' tNow + ttEvent_ppRest() 
#' 
#'  * This method is bound to \code{MosquitoFemale$timing()}.
#'
mbites_checkPostPrandial = function(){
  if (private$bloodfed == TRUE){ 
    ppRest = private$FemalePopPointer$get_MBITES_PAR("ttEvent_ppRest")  
    private$tNow = private$tNow + self$ppRest()
    private$tNext = private$tNow 
    private$bloodfed = FALSE
  }
}

#' M-BITES: Timing for \code{MosquitoFemale}
#'
#' Method for exponentially-distributed bout lengths (model mosquitoes as a Markov process).
#'  * This method is bound to \code{MosquitoFemale$timing()}.
#'
mbites_timing <- function(){

  if(private$state != 'D'){
    # NOTE :: this updates tNow 
    self$checkPostPrandial() 
 
    ttBout = if(private$search==FALSE){
      switch(private$state, #time to the attempt bout 
        B = {private$FemalePopPointer$get_MBITES_PAR("ttEvent_BoutF")},
        O = {private$FemalePopPointer$get_MBITES_PAR("ttEvent_BoutO")},
        S = {private$FemalePopPointer$get_MBITES_PAR("ttEvent_BoutS")}
      )
    }else{
      switch(private$state, #time to the search bout 
        B = {private$FemalePopPointer$get_MBITES_PAR("ttEvent_BoutBs")},
        O = {private$FemalePopPointer$get_MBITES_PAR("ttEvent_BoutOs")},
        S = {private$FemalePopPointer$get_MBITES_PAR("ttEvent_BoutSs")}
      )
    } 
    private$tNext = ttBout(private$tNow) 
    self$checkMating()
    self$checkEstivation() 
  }
}

#################################################################
# M-BITES: Time to Event Function Families  
# 
#   ttEvent_BoutX are "closures" derived from these families 
#   see: http://adv-r.had.co.nz/Functional-programming.html#closures 
# 
#################################################################

mbites_tteGenExp= function(t, rate, tmin=0){
  tmin + rexp(1, rate) 
} 

mbites_tteGamma = function(t, mean, cv, tmin=0){
  # mean  = shape*scale 
  # cv    = shape*scale^2/(shape/scale)^2 = 1/shape
  # shape = 1/cv 
  # scale = mean*cv 

  tmin + rgamma(1,shape=1/cv,scale=mean*cv)  
}  

mbites_tteDiurnal = function(t, peak, tmin=0){
  t=t+tmin
  
}
