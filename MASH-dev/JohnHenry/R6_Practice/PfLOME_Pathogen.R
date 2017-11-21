Pathogen <- R6Class("Pathogen",
                    
                    public = list(
                      
                      initialize = function(){
                        PfPathogen = list()
                      },
                      add_Pf = function(t,pfid,mic,mac,gtype){
                        pf = Pf$new(mic,mac,pfid,TRUE)
                        pf$set_gtype(gtype)
                        pf$set_PAR(pf$tentPAR(t,pfid))
                        pf$set_Pt(PAR$MZ0)
                        pf$set_activeP(1)
                        pf$set_activeG(1)
                        private$PfPathogen[[pfid]] = pf
                      },
                      update_pathogen = function(t){
                        for(i in 1:(pfid-1)){
                          private$PfPathogen[[i]]$update_Pf(t)
                        }
                        self$update_Ptot()
                      },
                      get_Pf = function(){
                        private$PfPathogen
                      },
                      update_Ptot = function(){
                        private$Ptot = 0
                        for(i in 1:(pfid-1)){
                          private$Ptot = private$Ptot + private$PfPathogen[[i]]$get_Pt()
                        }
                      },
                      update_Gtot = function(){
                        private$Gtot = 0
                        for(i in 1:(pfid-1)){
                          private$Gtot = private$Gtot + private$PfPathogen[[i]]$get_Gt()
                        }
                      },
                      get_Ptot = function(){
                        private$Ptot
                      },
                      get_Gtot = function(){
                        private$Gtot
                      }
                    ),
                    
                    private = list(
                      PfPathogen = NULL,
                      Ptot = NULL,
                      Gtot = NULL,
                      Stot = NULL
                    )
                    
)

Pf <- R6Class("Pf",
              
              public = list(
                initialize = function(mic,mac,pfid,seed=FALSE){
                  private$pfid = pfid
                  private$mic = mic
                  private$mac = mac
                  private$gtype = self$getGtype(mic,mac,seed)
                  private$ptype = self$getPtype(private$gtype,pfped$get_nptypes())
                },
                
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
                set_activeP = function(activeP){
                  private$activeP = activeP
                },
                get_activeG = function(){
                  private$activeG
                },
                set_activeG = function(activeG){
                  private$activeG = activeG
                },
                
                
                ## update function
                update_Pf = function(t){
                  self$update_Pt(t)
                  self$update_Gt(t)
                },
                
                update_Pt = function(t){
                  if(private$activeP > 0){
                    private$Pt = dPdt_tent(t,private$Pt,private$PAR)
                    if(is.na(private$Pt)){
                      private$PAR$tEnd = t - private$PAR$t0
                      private$PfPedigree[[private$pfid]]$t0 = private$PAR$tEnd
                      parasite$PfPathogen[[private$pfid]]$activeP = 0
                    }
                  }
                },
                
                update_Gt = function(t){
                  tt = (t+1)%%10+1
                  private$Gt = log10sum(c(private$Gt - gdk, GamCyGen(t,private$Ptt,private$PAR)))
                },
                
                tentPAR = function(t,pfid){
                  tEnd          = Pf.Duration()
                  mxPD          = Pf.MaxPD()
                  peakD         = Pf.PeakD()
                  MZ0           = Pf.MZ0()
                  
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
                }
              ),
              
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
