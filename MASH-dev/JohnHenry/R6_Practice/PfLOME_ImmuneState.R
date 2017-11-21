ImmuneState <- R6Class("ImmuneState",
                       
                       public = list(
                         initialize = funciton(){
                           
                         },
                         
                         updateImmuneState = function(t){
                           
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
                         }
                         
                       ),
                       
                       
                       ############### private fields #############
                       
                       
                       private = list(
                         General = NULL,
                         TypeSpecific = NULL
                       )
                       
)