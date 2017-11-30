x###########################################
#
#  Immune counters track something that
#  either affects something else or that
#  is a marker for infection/immunity
#
#  There are two classes of counters:
#  - Non-specific counters
#  - Strain specific counters
#
#  These counters have one of five effects:
#  - block sporozoites, preventing human infection
#  - block gametocites, preventing mosquito infection
#  - anti-parasite immunity (changes Pf infection time course)
#  - clinical immunity
#    * tolerance / fever threshold / risk of fever
#    * risk of severe malaria given fever
#    * risk of death given severe malaria
#    * risk for anemia
#
#  Non-specific immune counter
#  -  name
#  -  value
#  -  a function that updates the value
#  -  a function that updates the effect
#
#  Type-specific immune counter
#  -  name
#  -  the immune space
#  -  number of types seen
#  -  values of these responses
#  -  a function that updates the values
#  -  a function that updates the effect
#
###########################################


updateSalivaCounter_F1 = function(i,V,ixH,PAR){with(PAR,{
  hbr = HUMANS[[ixH]]$Pf$hbr
  HUMANS[[ixH]]$Pf$ImmC[[i]]$V <<- iterWaxWane(V,hbr,P50,Ps,wax,xw,kw,wane,xn,kn)
})}

salivaCounter_1 = list(
  name = "non-specific mosquito saliva counter",
  V = 0,
  updateF = updateSalivaCounter_F1,
  PAR = list(P50=6,Ps=1,wax=1/7,kw=30,wane=1/14,xn=05,kn=30)
)

#PfImmunity_LOME = list(
#  b = 0.55,        # the baseline probability of
#  spzBlock = 1,    #
#  gsBlock  = 1,     #
#
#)
#
#updateCounter(X,counter){with(counter,{
#  V = updateValue(X)
#})
#
#updateEffect(counter){with(counter,{
#  V = updateEffect(X)
#})
#
#generalCounter = function(F){
#list(V,
#  updateValue = NULL, # a function describing how V changes
#  updateEffect = NULL # a function describing what V does
#)
#
#typeCounter = function(F){
#list(
#  N=0,                  # number of types seen
#  V=NULL,
#  updateValue = NULL,   # a function describing how V changes
#  updateEffect = NULL   # a function describing what V does
#)
#
#antibodySpace = function(){
#
#}
#
##updateBystanderCounter_Pf_LOME = function(ixH,t){with(HUMANS[[ixH]]$Pf,{
##  HUMANS[[ixH]]$Pf$mosqSaliva.counter <<- waxwaneBY.LOME(ixH,tt)
##  HUMANS[[ixH]]$Pf$antispz.im <<- mosqSalivaF(ixH, t)
##})}
##
##waxwaneBY.LOME = function(ixH,tt){with(HUMANS[[ixH]]$Pf,{
##  mosqSaliva.counter + iterWaxWane(mosqSaliva.counter, hbr[t], PfP)
##})}
##
##mosqSalivaF = function(ixH, t){with(HUMANS[[ixH]]$Pf,{
##  1
##})}
##
##updateAntiSPZ.LOME = function(ixH,t){with(HUMANS[[ixH]]$Pf,{
##  HUMANS[[ixH]]$Pf$antispz.counter <<- waxwaneSPZ.LOME(ixH,tt)
##  HUMANS[[ixH]]$Pf$antispz.im <<- antispzF(ixH, t)
##})}
##
##waxwaneSPZ.LOME = function(ixH,tt){with(HUMANS[[ixH]]$Pf,{
##  antispz.counter + iterWaxWane(antispz.counter, eir[t], PfP)
##})}
##
##antispzF = function(ixH, t){with(HUMANS[[ixH]]$Pf,{
##  1
##})}
##
##updateLiver.LOME = function(ixH,t){with(HUMANS[[ixH]]$Pf,{
##  HUMANS[[ixH]]$Pf$liver.counter <<- waxwaneLV.LOME(ixH,tt)
##  HUMANS[[ixH]]$Pf$liver.im <<- liverF(ixH,t)
##})}
##
##waxwaneLV.LOME = function(ixH,tt){with(HUMANS[[ixH]]$Pf,{
##  liver.counter + iterWaxWane(liver.counter, eir[t], PfP)
##})}
##
##liverF = function(ixH, t){with(HUMANS[[ixH]]$Pf,{
##  1
##})}
##
##updatePEb.LOME = function(ixH,t,b.base=0.55){
##  #FIX STUB
##  with(HUMANS[[ixH]]$Pf,{
##    PfP$b.base*mosqSaliva.im*spz.im*liver.im
##})}
#iterWaxWane
#
