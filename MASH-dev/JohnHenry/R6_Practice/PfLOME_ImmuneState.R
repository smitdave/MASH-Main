ImmuneState <- R6Class("ImmuneState",
                       
                       public = list(
                         
                         initialize = function(){
                           
                           private$wx = c(rep(1/30, 3), rep(1/90, 3), rep(1/270, 3), 1/5/365)
                           private$wn = c(rep(c(1/30, 1/90, 1/270), 3), 0)
                           
                           BSImmCounters = list()
                           for(i in 1:10){
                             private$BSImmCounters[[i]] = list(
                               PAR = self$gImPAR(wx=private$wx[i], wn=private$wn[i], P50=6, Ps=5),
                               F = self$dynamicXdt
                             )
                           }
                           
                           history = list()
                           
                         },
                         
                         get_history = function(){
                           private$history
                         },
                         
                         get_BSImmCounters = function(){
                           private$BSImmCounters
                         },
                         
                         get_nBSImmCounters = function(){
                           private$nBSImmCounters
                         },
                         
                         updateImmuneState = function(t){
                           
                         },
                         
                         
                         ############ General immune methods ################
                         
                         
                         ##################################################################
                         # The notion here is that we have some underlying dynamic:
                         # dX/dt = r(K(P)-K(X))*(K(P)-X)
                         ##################################################################
                         
                         sigmoidX = function(X, X50=6, Xs=3, atMax=13){
                           pmin((1/(1+exp(-Xs*(X-X50))) - 1/(1+exp(Xs*X50)))/(1/(1+exp(-Xs*(atMax-X50))) - 1/(1+exp(Xs*X50))),1)
                         },
                         
                         dynamicXdt = function(X, P, PAR){with(PAR,{
                           X = ifelse(is.na(X),0,X)
                           P = ifelse(is.na(P),0,P)
                           K = self$sigmoidX(P, P50, Ps, atMax)
                           a = sign(K-X)
                           b = exp(abs(b))
                           X + (a*((1-exp(-b*abs(K-X)^sigma))*c(wn,0,wx)[a+2])^sigma)
                         })},
                         
                         daysSinceUnder = function(X, P, PAR){with(PAR,{
                           X = ifelse(is.na(X),0,X)
                           P = ifelse(is.na(P),0,P)
                           ifelse(P<Pthresh, X+1, 0)
                         })},
                         
                         daysSinceOver = function(X, P, PAR){with(PAR,{
                           X = ifelse(is.na(X),0,X)
                           P = ifelse(is.na(P),0,P)
                           ifelse(P>Pthresh, X+1, 0)
                         })},
                         
                         antibodyRegister  = function(pfid, t, ixH){
                           HUMANS[[ixH]]$Pf$types <<- rbind(HUMANS[[ixH]]$Pf$types, c(pfid, t))
                         },
                         
                         dynamicCounter = function(P, PAR){
                           X = 0
                           Xt = X
                           for(t in 1:length(P)){
                             X = dynamicXdt(X, P[t], PAR)
                             Xt = c(Xt, X)
                           }
                           Xt
                         },
                         
                         gImPAR = function(wx=1/80, wn=1/180, P50=6, Ps=1, atMax=11, b=2, sigma=1){
                           list(wx=wx,wn=wn,P50=P50,Ps=Ps,atMax=atMax,b=b,sigma=sigma)
                         },
                         
                         
                         ############ type-specific immune methods ###########
                         
                         
                         #turns ptype into binary matrix, can more easily compare for cross immunity
                         ptype2Mat = function(ptype,nAntigenLoci,nptypes){
                           mat = matrix(0,nAntigenLoci,max(nptypes))
                           for(i in 1:nAntigenLoci){
                             mat[i,ptype[i]] = mat[i,ptype[i]]+1
                           }
                           return(mat)
                         },
                         
                         shift = function(v,places,dir="right") {
                           places = places%%length(v)
                           if(places==0) return(v)
                           temp = rep(0,length(v))
                           if(dir=="left"){
                             places = length(v)-places
                             dir = "right"}
                           if(dir=="right"){
                             temp[1:places] = tail(v,places)
                             temp[(places+1):length(v)] = v[1:(length(v)-places)]
                             return(temp)
                           }
                         },
                         
                         crossImmunity = function(ptypes,ptypesTime,nAntigenLoci,t,dxp,dtp){
                           crossImmune = ptypes
                           for(i in 1:nAntigenLoci){
                             if(nptypes[i] > 2){
                               for(dx in 1:(nptypes[i]-2)){
                                 ptypestemp = ptypes[i,1:nptypes[i]]
                                 ptypesTimetemp = ptypesTime[i,1:nptypes[i]]
                                 dt = t-ptypesTimetemp
                                 crossImmunetemp = crossImmune[i,1:nptypes[i]]
                                 crossImmunetemp = crossImmunetemp + weight(dx,dt,dxp,dtp)*(shift(ptypestemp,dx,"right")+shift(ptypestemp,dx,"left"))
                                 ptypes[i,1:nptypes[i]] = ptypestemp[1:nptypes[i]]
                                 ptypesTime[i,1:nptypes[i]] = ptypesTimetemp[1:nptypes[i]]
                                 crossImmune[i,1:nptypes[i]] = crossImmunetemp[1:nptypes[i]]
                               }
                             }
                             if(nptypes[i]==2){
                               dx = 1
                               ptypestemp = ptypes[i,1:nptypes[i]]
                               ptypesTimetemp = ptypesTime[i,1:nptypes[i]]
                               dt = t-ptypesTimetemp
                               crossImmunetemp = crossImmune[i,1:nptypes[i]]
                               crossImmunetemp = crossImmunetemp + weight(dx,dt,dxp,dtp)*shift(ptypestemp,dx)
                               ptypes[i,1:nptypes[i]] = ptypestemp[1:nptypes[i]]
                               ptypesTime[i,1:nptypes[i]] = ptypesTimetemp[1:nptypes[i]]
                               crossImmune[i,1:nptypes[i]] = crossImmunetemp[1:nptypes[i]]
                             }
                           }
                           return(crossImmune)
                         },
                         
                         weight = function(dx,dt,dxp,dtp){
                           exp(-(dx*dxp+dt*dtp))
                         }
                         
                       ),
                       
                       
                       ############### private fields #############
                       
                       
                       private = list(
                         nBSImmCounters = NULL,
                         BSImmCounters = NULL,
                         wx = NULL,
                         wn = NULL,
                         TypeCounters = NULL,
                         ptypes = NULL,
                         ptypesTime = NULL,
                         history = NULL
                       )
                       
)