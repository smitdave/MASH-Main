
#' MBITES: The probability of refeeding 
#' 
#' Computes the integral of the probability of  
#' feeding conditioned on the size of the bloodmeal, 
#' multiplied by the probability of bloodmeal 
#' of that size.  
#' 
mbites_getPrRefeed = function(){
  rf_a = MBITES:::Parameters$get_rf_a()
  rf_b = MBITES:::Parameters$get_rf_b()
  bm_a = MBITES:::Parameters$get_bm_a()
  bm_b = MBITES:::Parameters$get_bm_b()

  FF = function(X){ 
    dbeta(X,bm_a,bm_b)*(2+rf_b)/(1+rf_b) - exp(rf_a*X)/(rf_b + exp(rf_a*X))
  }
  integrate(FF, 0, 1)  
} 

#' MBITES: The probability of refeeding 
#' 
#' Computes the integral of the probability of  
#' feeding conditioned on the size of the bloodmeal, 
#' multiplied by the probability of bloodmeal 
#' of that size.  
#' 
mbites_getPrOverfeed = function(){
  of_a = MBITES:::Parameters$get_of_a()
  of_b = MBITES:::Parameters$get_of_b()
  bm_a = MBITES:::Parameters$get_bm_a()
  bm_b = MBITES:::Parameters$get_bm_b()

  FF = function(X){ 
    dbeta(X,bm_a,bm_b)*exp(of_a*private$bmSize)/(of_b + exp(of_a*private$bmSize))
  }
  integrate(FF, 0, 1)  
} 

#' MBITES: Prob of Surviving the post-prandial flight  
#' 
#' 
mbites_getSurvLaden =function(){ 
  lrl = self$getRestingParam()
  land = lrl[1]; retry = lrl[2]; leave = lrl[3]    
  PPR_a = MBITES:::Parameters$get_PPR_a()
  PPR_b = MBITES:::Parameters$get_PPR_b()
  bm_a = MBITES:::Parameters$get_bm_a()
  bm_b = MBITES:::Parameters$get_bm_b()
  FF = function(X){ 
     p = dbeta(x,bm_a,bm_b)*exp(PPR_a*private$bmSize)/(PPR_b + exp(PPR_a*private$bmSize))
     p*(land+leave)/(1-retry) 
  }
  integrate(FF, 0, 1)  
} 

#' MBITES: Prob of Leaving, post prandially 
#' 
#' 
mbites_getLeaveLaden=function(){ 
  lrl = self$getRestingParam()
  land = lrl[1]; retry = lrl[2]; leave = lrl[3]    
  PPR_a = MBITES:::Parameters$get_PPR_a()
  PPR_b = MBITES:::Parameters$get_PPR_b()
  bm_a = MBITES:::Parameters$get_bm_a()
  bm_b = MBITES:::Parameters$get_bm_b()
  FF = function(X){ 
     p = dbeta(x,bm_a,bm_b)*exp(PPR_a*private$bmSize)/(PPR_b + exp(PPR_a*private$bmSize))
     p*leave/(1-retry) 
  }
  integrate(FF, 0, 1)  
} 

#' MBITES: Prob of surviving landing 
#' 
#' 
mbites_getSurvUnaden =function(){ 
  lrl = self$getRestingParam()
  land = lrl[1]; retry = lrl[2]; leave = lrl[3]    
  (land+leave)/(1-retry) 
} 

#' MBITES: Prob of Surviving the post-prandial flight  
#' 
#' 
mbites_getLeaveUnaden =function(){ 
  lrl = self$getRestingParam()
  land = lrl[1]; retry = lrl[2]; leave = lrl[3]    
  leave/(1-retry) 
} 


#' MBITES: Resting spot probabilities 
#'  
mbites_getRestingParam = function(){ 
  if(private$site$get_type()==1L){
     probs = MBITES:::Parameters$get_InAndOut_row(private$rspot) * MBITES:::Parameters$get_wts(private$state)
     land = sum(probs[1:3]) 
     retry = probs[4] 
     leave = probs[5] 
  # not homestead
  } else {
     return("v")
  }
  c(land, retry, leave) 
} 

#' MBITES: Blood Feeding Search Bout State Transitions 
#'  
#' 
mbites_FstateTransitions = function(i){
  # F -> F, B, D 
  succeed = MBITES:::Parameters$get_Bs_succeed()
  survive = MBITES:::Parameters$get_Bs_surv()
  
  F2B = succeed*survive
  F2F = (1-succeed)*survive
  F2D = 1-survive 

  return(c(F2F, F2B, 0, 0, 0, F2D)) 
} 

#' MBITES: Blood Feeding Attempt Bout State Transitions 
#'  
#' B -> F,B,R,D  
#'  
mbites_BstateTransitions = function(){
  ##########################################################
  # Does the mosquito even make it to an initial approach? 
  ##########################################################
  approach = MBITES:::Parameters$get_B_succeed()
  survive = MBITES:::Parameters$get_B_surv())

  ##########################################################
  # check the function 
  ##########################################################
  host = private$feeding_resource$RiskQ$typewtsQ() 
 
  ##########################################################
  # probability of taking a human bloodmeal | human chosen 
  ##########################################################
  h1 = MBITES:::Parameters$get_probeH()       
  h2 = MBITES:::Parameters$get_surviveprobeH()
  h3 = MBITES:::Parameters$get_feedH()
  hfeed = h1*h2*h3 
  hfail = (1-h1)+h1*h2*(1-h3) 
 
  ##########################################################
  # probability of taking a bloodmeal | other host chosen 
  ##########################################################
  z1 = MBITES:::Parameters$get_surviveZ()
  z2 = MBITES:::Parameters$get_feedZ()
  zfeed = z1*z2 
  zfail = z1*(1-z2) 
 
  ##########################################################
  # probability of failing | trap chosen 
  ##########################################################
  tfail = 0 
   
  ##########################################################
  # survive an unladen flight, stay
  ##########################################################
  surviveUnladen = self$getSurvUnladen()  
  stayUnladen = 1-self$getLeaveUnladen()  
 
  ##########################################################
  # survive a laden flight, stay 
  ##########################################################
  surviveLaden = self$getSurvLaden()  
 
  ##########################################################
  # fail the approach  
  # note :: check hfeed and zfeed above for more fails  
  ##########################################################
  failApproach = host[1]*hfail+host[2]*zfail+host[3]*tfail+host[4]
  bFeed = host[1]*hfeed+host[2]*zfeed
 
  B2R = approach*bFeed*surviveLaden
  B2B = approach*failApproach*surviveUnladen*stayUnladen*survive
  B2F1 = (1-approach)*surviveUnladen*stayUnladen*survive 
  B2F2 = approach*failApproach*surviveUnladen*(1-stayUnladen)*survive
  B2F = B2F1 + B2F2 
  B2D = 1-B2R-B2B-B2F  
  return(c(B2F, B2B, B2R, 0, 0, B2D)) 
}

#' MBITES: Resting Period State Transitions  
#' 
#'  R-> B, F, L, O, D 
#' 
mbites_RperiodTransitions = function(i){
  ##########################################################
  # The probability of refeeding  
  ##########################################################
  refeed = self$getPrRefeed() 
  ##########################################################
  # stay, after a laden flight
  #########################################################
  stayLaden = self$getLeaveLaden()  
  ##########################################################
  # aquatic habitat present 
  ##########################################################
  aquatic = private$site$has_aqua() 
  ##########################################################
  # blood host present 
  ##########################################################
  blood = private$site$has_feed()
  ##########################################################
  # survive the post-prandial resting period 
  ##########################################################
  surviveRest = 
  
  R2B = blood*refeed*surviveRest  
  R2F = (1-blood)*refeed*surviveRest  
  R2L = (1-aquatic)*(1-refeed)*surviveRest 
  R2O = aquatic*(1-refeed)*surviveRest
  R2D = 1-R2F-R2B-R2L-R2O 
  return(c(R2F, R2B, 0, R2L, R2O, R2D)) 
} 

#' mbites2mbdetes: Egg Laying Search Bout State Transitions 
#'  
#' L -> L, O, D   
#'  
#'  
mbites_LstateTransitions = function(i){
  success = MBITES:::Parameters$get_Os_succeed()
  survive = MBITES:::Parameters$get_Os_surv()

  L2O = succeed*survive
  L2L = (1-succeed)*survive
  L2D = 1-L2O-L2L 

  return(c(0, 0, 0, L2L, L2O, L2D)) 
} 

#' mbites2mbdetes: Egg Laying Attempt Bout State Transitions 
#'  
#' O -> F, B, L, O, D 
#'  
#'  
mbites_OstateTransitions = function(){
  success = MBITES:::Parameters$get_O_succeed()
  survive = MBITES:::Parameters$get_O_surv()
  blood   = private$site$has_feed()
  stay    = 1-self$getLeaveUnladen()  

  O2F = succeed*survive*(1-blood) 
  O2B = succeed*survive*blood 
  O2O = (1-succeed)*survive*stay
  O2L = (1-succeed)*survive*(1-stay) 
  O2D = 1-O2F-O2B-O2O-O2L-O2D  
  return(c(O2F, O2B, 0, O2L, O2O, O2D)) 
} 

#' mbites2mbdetes: The State Transition Matrix 
#'  
#' FBRLO to FBRLOD (the rows sum to one)  
#'  
mbites_StateTransitions = function(i){
  FBRLOD = self$FstateTransitions() 
  FBRLOD = rbind(FBRLOD, self$BstateTransitions()) 
  FBRLOD = rbind(FBRLOD, self$RstateTransitions()) 
  FBRLOD = rbind(FBRLOD, self$LstateTransitions()) 
  FBRLOD = rbind(FBRLOD, self$OstateTransitions()) 
} 

