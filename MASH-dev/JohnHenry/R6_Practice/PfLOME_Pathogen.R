Pathogen <- R6Class("Pathogen",
                    
                    public = list(
                      
                      ## initialization of components
                      
                      initialize = function(){
                        private$PfPathogen = list()
                        private$PfMOI = 0
                        private$Ptot = NaN
                        private$Gtot = NaN
                        private$history = list()
                      },
                      
                      ## add pf during infection
                      
                      add_Pf = function(t,pfid,mic,mac,gtype){
                        pf = Pf$new(mic,mac,pfid,TRUE)
                        pf$set_gtype(gtype)
                        pf$set_PAR(pf$tentPAR(t,pfid))
                        pf$set_Pt(pf$get_PAR()$MZ0)
                        pf$set_activeP(1)
                        pf$set_Gt(NaN)
                        pf$set_activeG(1)
                        ifelse(is.na(private$Ptot),self$set_Ptot(pf$get_PAR()$MZ0),self$set_Ptot(log10(10^pf$get_PAR()$MZ0+10^private$Ptot)))
                        private$PfPathogen[[pfid]] = pf
                        self$set_PfMOI(self$get_PfMOI()+1)
                      },
                      
                      remove_Pf = function(t,pfid){
                        private$PfPathogen[[pfid]] = NULL ## null out object or just set active P/G to 0?
                        self$set_PfMOI(self$get_PfMOI()-1)
                      },
                      
                      ## update pathogens
                      
                      update_pathogen = function(t){
                        for(i in 1:length(private$PfPathogen)){
                          Pttemp = private$PfPathogen[[i]]$get_Pt()
                          private$PfPathogen[[i]]$update_Pf(t)
                          if(is.na(private$PfPathogen[[i]]$get_Pt()) & !is.na(Pttemp)){
                            self$set_PfMOI(private$PfMOI-1)
                          }
                        }
                        self$update_Ptot()
                        self$update_Gtot()
                        self$update_history()
                      },

                      
                      
                      ######### update methods ##########
                      
                      
                      update_Ptot = function(){
                        private$Ptot = NaN
                        for(i in 1:length(private$PfPathogen)){
                          private$Ptot = self$log10sum(c(private$Ptot, private$PfPathogen[[i]]$get_Pt()))
                        }
                      },
                      
                      update_Gtot = function(){
                        private$Gtot = NaN
                        for(i in 1:length(private$PfPathogen)){
                          private$Gtot = self$log10sum(c(private$Gtot, private$PfPathogen[[i]]$get_Gt()))
                        }
                      },
                      
                      update_history = function(){
                        private$history$Ptot = c(private$history$Ptot,private$Ptot)
                        private$history$Gtot = c(private$history$Gtot,private$Gtot)
                        private$history$PfMOI = c(private$history$PfMOI,private$PfMOI)
                      },
                      
                      
                      ######## accessors #########
                      
                      
                      get_Ptot = function(){
                        private$Ptot
                      },
                      
                      set_Ptot = function(newPtot){
                        private$Ptot = newPtot
                      },
                      
                      get_Gtot = function(){
                        private$Gtot
                      },
                      
                      get_PfMOI = function(){
                        private$PfMOI
                      },
                      
                      set_PfMOI = function(newPfMOI){
                        private$PfMOI = newPfMOI
                      },
                      
                      get_Pf = function(){
                        private$PfPathogen
                      },
                      
                      get_history = function(){
                        private$history
                      },
                      
                      log10sum = function(x){
                        self$log10vals(log10(sum(10^self$log10vals(x), na.rm=TRUE)))
                      },
                      
                      log10vals = function(x){
                        ifelse(!is.na(x) & is.finite(x) & x>=0, x, NaN)
                      }
                      
                    ),
                    
                    
                    ########### private fields ##############
                    
                    
                    private = list(
                      PfPathogen = NULL,
                      Ptot = NULL,
                      Gtot = NULL,
                      Stot = NULL,
                      PfMOI = NULL,
                      history = NULL
                    )
                    
)

Pf <- R6Class("Pf",
              
              public = list(
                
                ## initialization of components
                
                initialize = function(mic,mac,pfid,seed=FALSE){
                  private$pfid = pfid
                  private$mic = mic
                  private$mac = mac
                  private$mu = .01
                  private$Ptt = rep(NaN,10)
                  private$gtype = self$getGtype(mic,mac,private$mu,seed)
                  private$ptype = self$getPtype(private$gtype,pfped$get_nptypes())
                },
                
                
                ######### setting g/p types
                
                
                getGtype = function(mic,mac,mu,seed=FALSE){
                  ifelse(seed==TRUE,{
                    gtype=runif(pfped$get_nptypes())
                  },
                  {
                    micType = pfped$get_gtype(mic)
                    macType =  pfped$get_gtype(mac)
                    micmac = cbind(micType, macType)
                    ix = sample(c(1,2), pfped$get_nAntigenLoci(), replace =TRUE)
                    gtype = NULL
                    for(i in 1:pfped$get_nAntigenLoci()){
                      gtype = c(gtype,micmac[i,ix[i]])
                    }
                    gtype = self$mutate(gtype,mu)
                  })
                  return(gtype)
                },
                
                getPtype = function(gtype,nptypes){
                  ptype = ceiling(gtype*nptypes)
                  ptype[which(ptype==0)]=1
                  return(ptype)
                },
                
                mutate = function(gtype,mu){
                  #mu is parameter describing mutation probability at each locus - between 0 and 1
                  ##assuming julia gog forulation with genotypes lying on unit interval:
                  mutgen = which(rbinom(pfped$get_nAntigenLoci(),1,mu)==1)
                  if(length(mutgen)!=0) {
                    for(i in 1:length(mutgen)) {
                      gtype[mutgen[i]] = gtype[mutgen[i]] + runif(1,min=-gtype[mutgen[i]],max=1-gtype[mutgen[i]]) #uniformly random mutation
                    }
                  }
                  return(gtype)
                },
                
                
                ############ accessors ############
                
                
                get_pfid = function(){
                  private$pfid
                },
                
                get_gtype = function(){
                  private$gtype
                },
                
                set_gtype = function(newgtype){
                  private$gtype = newgtype
                },
                get_ptype = function(){
                  private$ptype
                },
                
                set_ptype = function(newptype){
                  private$ptype = newptype
                },
                
                get_PAR = function(){
                  private$PAR
                },
                
                set_PAR = function(newPAR){
                  private$PAR = newPAR
                },
                
                get_Pt = function(){
                  private$Pt
                },
                
                set_Pt = function(newPt){
                  private$Pt = newPt
                },
                
                get_Ptt = function(){
                  private$Ptt
                },
                
                set_Ptt = function(newPtt){
                  private$Ptt = newPtt
                },
                
                get_Gt = function(){
                  private$Gt
                },
                
                set_Gt = function(newGt){
                  private$Gt = newGt
                },
                
                get_mic = function(){
                  private$mic
                },
                
                get_mac = function(){
                  private$mac
                },
                
                get_activeP = function(){
                  private$activeP
                },
                
                set_activeP = function(newactiveP){
                  private$activeP = newactiveP
                },
                
                get_activeG = function(){
                  private$activeG
                },
                
                set_activeG = function(newactiveG){
                  private$activeG = newactiveG
                },
                
                
                ########## update methods ##########
                
                
                update_Pf = function(t){
                  self$update_Pt(t)
                  self$update_Ptt()
                  self$update_Gt(t)
                },
                
                update_Pt = function(t){
                  self$set_Pt(self$dPdt_tent(t,private$Pt,private$PAR))
                  if(is.na(private$Pt)){
                    private$PAR$tEnd = t - private$PAR$t0
                    self$set_activeP(0)
                  }
                },
                
                update_Ptt = function(){
                  private$Ptt = self$shift(private$Ptt,1)
                  private$Ptt[1] = private$Pt
                },
                
                update_Gt = function(t){
                  if(is.na(private$Gt)){
                    private$Gt = self$GamCyGen(t,private$Ptt[10],private$PAR)
                  }
                  if(!is.na(private$Gt)){
                    private$Gt = self$log10sum(c(private$Gt - private$gdk, self$GamCyGen(t,private$Ptt[10],private$PAR)))
                  }
                },
                
                GamCyGen = function(t, P, PAR){
                  P-2
                },
                
                
                ############### Tent Methods #################

                
                Pf.MaxPD = function(N=1, mn=10.5, vr=0.5){
                  rnorm(N,mn,vr)
                  },
                
                Pf.PeakD = function(min=18){
                  #FIX STUB
                  # Day when parasitemia first peaks
                  ceiling(min+rlnorm(1,log(3),.5))
                },
                
                Pf.MZ0 = function(){
                  #FIX STUB
                  rnorm(1,4.2,.1)
                },
                
                Pf.Duration = function(peakD,N=1,mn=200){
                  #FIX STUB
                  # Time to last parasitemia
                  peakD + rgeom(N,1/mn)
                },
                
                
                tentPAR = function(t,pfid){
                  mxPD          = self$Pf.MaxPD()
                  peakD         = self$Pf.PeakD()
                  MZ0           = self$Pf.MZ0()
                  tEnd          = self$Pf.Duration(peakD)
                  
                  gr 		        = (mxPD-MZ0)/peakD
                  dr            = mxPD/(tEnd-peakD)
                  gtype         = private$gtype
                  
                  list(
                    pfid	        = pfid,
                    t0  	        = t,
                    gr            = gr,
                    dr            = dr,
                    MZ0           = MZ0,
                    peakD         = peakD,
                    mxPD          = mxPD,
                    tEnd          = tEnd
                  )
                },
                
                gr_tent = function(t, PAR){with(PAR,{
                  ifelse(t<peakD, gr, -dr)
                })},
                
                dPdt_tent = function(t, P, PAR, PD=0, IM=0){with(PAR,{
                  age = ifelse(t>=t0, t-t0+1, 0)
                  P = ifelse(age>=1 & age<=tEnd,
                             pmin(mxPD, P + self$gr_tent(age,PAR))-PD-IM,
                             NaN)
                  ifelse(!is.na(P)&P>0, P, NaN)
                })},
                
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
                
                log10sum = function(x){
                  self$log10vals(log10(sum(10^self$log10vals(x), na.rm=TRUE)))
                },
                
                log10vals = function(x){
                  ifelse(!is.na(x) & is.finite(x) & x>=0, x, NaN)
                }
                
              ),
              
              
              ############ private fields #################
              
              
              private = list(
                pfid = NULL,
                ## tentPars
                PAR = NULL,
                gdk = log(2)/6, ## halflife of gametocytes 6 days
                ## Parasite Densities (Pt = Asexual, Gt = Gametocyte, 
                ## St = Sporozoite)
                activeP = NULL,
                activeG = NULL,
                activeS = NULL,
                Pt = NULL,
                Ptt = NULL,
                Gt = NULL,
                St = NULL,
                mic = NULL,
                mac = NULL,
                ## biological parameters
                gtype = NULL,
                ptype = NULL,
                mu = .01
                
              )
              
)
