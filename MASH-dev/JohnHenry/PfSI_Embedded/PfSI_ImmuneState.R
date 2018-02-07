ImmuneState <- R6Class("ImmuneState",
                       
                       public = list(
                         
                         initialize = function(){
                           
                           private$nBSImmCounters = 4
                           private$BSImm = rep(0,private$nBSImmCounters)
                           private$wx = dt*c(1/30, 1/90, 1/270, 1/5/365)
                           private$wn = dt*c(1/30, 1/90, 1/270, 0)
                           
                           BSImmCounters = list()
                           for(i in 1:private$nBSImmCounters){
                             private$BSImmCounters[[i]] = list(
                               PAR = self$gImPAR(wx=private$wx[i], wn=private$wn[i], P50=6, Ps=5),
                               F = self$dynamicXdt
                             )
                           }
                           
                           private$history = list()
                           private$history$BSImm = list()
                           for(i in 1:private$nBSImmCounters){
                             private$history$BSImm[[i]] = 0
                           }
                           private$GenImm = 0
                           
                           private$dxp = 1
                           private$dtp = 1/100
                           ##### this next line assumes pfpedigree is declared and called pfped - be careful here
                           private$nptypes = pfped$get_nptypes()
                           
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
                         
                         get_BSImm = function(){
                           private$BSImm
                         },
                         
                         get_typeImm = function(t,ptype){
                           weight = self$crossImm(ptype,private$nptypes)
                           if(is.null(private$typeImm)){
                             private$typeImm = matrix(rep(0,max(private$nptypes)*length(ptype)),nrow=length(ptype))
                           }
                           print(private$typeImm)
                           rowSums(weight*private$typeImm)
                         },
                         
                         update_immuneState = function(t,dt,Ptot){
                           
                           ##BS immunity update
                           for(i in 1:private$nBSImmCounters){
                             private$BSImm[i] = with(private$BSImmCounters[[i]], F(private$BSImm[i], Ptot, PAR)) # bloodstage immune counters
                           }
                           private$GenImm = 1-prod(1-private$BSImm)
                           
                           ##type specific immunity update
                           
                           self$update_typeImmunity(t,dt,ptype=NaN)
                           
                           ##history update
                           self$update_history()
                         },
                         
                         update_history = function(){
                           for(i in 1:private$nBSImmCounters){
                             private$history$BSImm[[i]] = c(private$history$BSImm[[i]], private$BSImm[i])
                           }
                           private$history$GenImm = c(private$history$GenImm, 1-prod(1-private$BSImm))
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
                           ifelse(P<Pthresh, X+dt, 0)
                         })},
                         
                         daysSinceOver = function(X, P, PAR){with(PAR,{
                           X = ifelse(is.na(X),0,X)
                           P = ifelse(is.na(P),0,P)
                           ifelse(P>Pthresh, X+dt, 0)
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
                         ptype2Mat = function(ptype,nptypes){
                           nAntigenLoci = length(ptype)
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
                         
                         update_typeImmunity = function(t,dt,ptype){
                           nptypes = pfped$get_nptypes()
                           ptype = ifelse(is.na(ptype),rep(0,10),ptype)
                           private$ptypesTime = pmax(self$ptype2Mat(ptype,nptypes)*t*dt,private$ptypesTime,na.rm=T)
                           private$typeImm = pmax(exp(-private$dtp*private$ptypesTime),0,na.rm=T)
                         },
                         
                         crossImm = function(ptype,nptypes){
                           ptypes = self$ptype2Mat(ptype,nptypes)
                           for(i in 1:nAntigenLoci){
                             a = which(ptypes[i,]==1)
                             b = which(ptypes[i,]==0)
                             b = b[1:(nptypes[i]-1)]
                             c = abs(a-b)
                             ptypes[i,b]=c
                             ptypes[i,a]=0
                             if(nptypes[i]<max(nptypes)){
                               ptypes[i,(nptypes[i]+1):max(nptypes)]=NaN
                             }
                           }
                           pmax(exp(-private$dxp*ptypes),0,na.rm=T)
                         }
                         
                         
                         
                       ),
                       
                       
                       ############### private fields #############
                       
                       
                       private = list(
                         nBSImmCounters = NULL,
                         BSImmCounters = NULL,
                         BSImm = NULL,
                         GenImm = NULL,
                         wx = NULL,
                         wn = NULL,
                         ptypesTime = NULL,
                         typeImm = NULL,
                         dxp = NULL,
                         dtp = NULL,
                         nptypes = NULL,
                         history = NULL
                       )
                       
)