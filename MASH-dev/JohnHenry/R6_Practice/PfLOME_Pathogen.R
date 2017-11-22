Pathogen <- R6Class("Pathogen",
                    
                    public = list(
                      
                      ## initialization of components
                      
                      initialize = function(){
                        private$PfPathogen = list()
                        private$PfMOI = 0
                      },
                      
                      ## add pf during infection
                      
                      add_Pf = function(t,pfid,mic,mac,gtype){
                        pf = Pf$new(mic,mac,pfid,TRUE)
                        pf$set_gtype(gtype)
                        pf$set_PAR(pf$tentPAR(t,pfid))
                        pf$set_Pt(PAR$MZ0)
                        pf$set_activeP(1)
                        pf$set_activeG(1)
                        self$set_Ptot(PAR$MZ0)
                        private$PfPathogen[[pfid]] = pf
                        self$set_PfMOI(self$get_PfMOI()+1)
                      },
                      
                      remove_Pf = function(t,pfid){
                        private$PfPathogen[[pfid]] = NULL
                        self$set_PfMOI(self$get_PfMOI()-1)
                      },
                      
                      ## update pathogens
                      
                      update_pathogen = function(t){
                        for(i in 1:(pfid-1)){
                          private$PfPathogen[[i]]$update_Pf(t)
                          #if(is.na(private$PfPathogen[[i]]$Pt)){
                          #  private$PfPathogen[[i]]$activeP = 0
                          #}
                        }
                        self$update_Ptot()
                        self$update_Gtot()
                        self$update_history()
                      },

                      
                      
                      ######### update methods ##########
                      
                      
                      update_Ptot = function(){
                        private$Ptot = 0
                        for(i in 1:private$PfMOI){
                          private$Ptot = private$Ptot + private$PfPathogen[[i]]$get_Pt()
                        }
                      },
                      
                      update_Gtot = function(){
                        private$Gtot = 0
                        for(i in 1:private$PfMOI){
                          private$Gtot = private$Gtot + private$PfPathogen[[i]]$get_Gt()
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
                      }
                      
                    ),
                    
                    
                    ########### private fields ##############
                    
                    
                    private = list(
                      PfPathogen = NULL,
                      Ptot = NULL,
                      Gtot = NULL,
                      Stot = NULL,
                      PfMOI = NULL,
                      history = list()
                    )
                    
)

Pf <- R6Class("Pf",
              
              public = list(
                
                ## initialization of components
                
                initialize = function(mic,mac,pfid,seed=FALSE){
                  private$pfid = pfid
                  private$mic = mic
                  private$mac = mac
                  private$gtype = self$getGtype(mic,mac,seed)
                  private$ptype = self$getPtype(private$gtype,pfped$get_nptypes())
                },
                
                
                ######### setting g/p types
                
                
                getGtype = function(mic,mac,seed=FALSE){
                  ifelse(seed==TRUE,{
                    gtype=runif(pfped$get_nptypes())
                  },
                  {
                    micType = pfped$get_gtype[[mic]]
                    macType =  pfped$get_gtype[[mac]]
                    micmac = cbind(micType, macType)
                    ix = sample(c(1,2), nAntigenLoci, replace =TRUE)
                    gtype = NULL
                    for(i in 1:nAntigenLoci){
                      gtype = c(gtype,micmac[i,ix[i]])
                    }
                    gtype = mutate(gtype,mu)
                  })
                  return(gtype)
                },
                
                getPtype = function(gtype,nptypes){
                  ptype = ceiling(gtype*nptypes)
                  ptype[which(ptype==0)]=1
                  return(ptype)
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
                  self$update_Gt(t)
                },
                
                update_Pt = function(t){
                  if(private$activeP > 0){
                    private$Pt = self$dPdt_tent(t,private$Pt,private$PAR)
                    #if(is.na(private$Pt)){
                    #  private$PAR$tEnd = t - private$PAR$t0
                    #  private$PfPedigree[[private$pfid]]$t0 = private$PAR$tEnd
                    #  parasite$PfPathogen[[private$pfid]]$activeP = 0
                    #}
                  }
                },
                
                update_Gt = function(t){
                  tt = (t+1)%%10+1
                  private$Gt = log10sum(c(private$Gt - gdk, self$GamCyGen(t,private$Ptt,private$PAR)))
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
                  if(NOISY == TRUE) browser()
                  age = ifelse(t>=t0, t-t0+1, 0)
                  P = ifelse(age>=1 & age<=tEnd,
                             pmin(mxPD, P + self$gr_tent(age,PAR))-PD-IM,
                             NaN)
                  ifelse(!is.na(P)&P>0, P, NaN)
                })}
              ),
              
              
              ############ private fields #################
              
              
              private = list(
                pfid = NULL,
                ## tentPars
                PAR = NULL,
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
                ptype = NULL
                
              )
              
)
