Pathogen <- R6Class("Pathogen",
                    
                    public = list(
                      
                      initialize = function(){
                        PfPathogen = list()
                      },
                      add_Pf = function(t,pfid){
                        private$PfPathogen[[pfid]] = Pf$new(t,pfid)
                      },
                      update_pathogen = function(t){
                        for(i in 1:pfid){
                          private$PfPathogen[[i]]$update_Pf(t)
                        }
                      },
                      get_Pf = function(){
                        private$PfPathogen
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
                initialize = function(t,pfid){
                  private$pfid = pfid
                  private$activeP = 1
                  private$PAR = tentPAR(t,private$pfid)
                  private$Pt = private$PAR$MZ0
                  private$gtype = getGtype(private$pfid)
                  private$ptype = getPtype(private$pfid,private$gtype,nptypes)
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
                get_Pt = function(){
                  private$Pt
                },
                
                ## update function
                update_Pf = function(t){
                  update_Pt(t)
                  update_Gt(t)
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
                ## biological parameters
                gtype = NULL,
                ptype = NULL
              )
              
)
