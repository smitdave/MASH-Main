PfPedigree <- R6Class("PfPedigree",
                      
                      public = list(
                        initialize = function(){
                          private$pfid = 1
                          private$nAntigenLoci = 9
                          private$nptypes = c(3,5,4,3,6,3,2,7,9)
                        },
                        
                        add2Pedigree = function(pf){
                          pfid = pf$get_pfid()
                          private$gtype[[pfid]] = pf$get_gtype()
                          private$ptype[[pfid]] = pf$get_ptype()
                          private$mic[[pfid]] = pf$get_mic()
                          private$mac[[pfid]] = pf$get_mac()
                          private$th[[pfid]] = pf$get_th()
                          private$thEnd[[pfid]] = pf$get_thEnd()
                          private$sib[[pfid]] = pf$get_sib()
                          
                          PfPedigree[[pfid]] <<- newPfPed(pfid)
                          if(pfid > 1){
                            ## need to fix mic/mac pick - should be assigned 
                            ## to Pf object inside mosquito, passed onto next
                            ## human
                            private$mac[[pfid]] <<- getParent()
                            private$mic[[pfid]] <<- getParent()
                            private$th[[pfid]] <<- t
                            private$thEnd[[pfid]] <<- private$pathogen[[pfid]]$tEnd+t
                            if(PfPedigree[[pfid]]$mic != PfPedigree[[pfid]]$mac) {
                              PfPedigree[[pfid]]$sib = sample(1:2,2)
                            }
                          }
                          private$gtype[[pfid]] <<- getGtype(pfid)
                          private$ptype[[pfid]] <<- getPtype(pfid,private$gtype[[pfid]],nptypes)
                          private$pfid = pfid+1
                        },
                          
                        get_gtype = function(pfid){
                          private$gtype[[pfid]]
                        },
                        
                        get_ptype = function(pfid){
                          private$ptype[[pfid]]
                        },
                        
                        get_pfid = function(){
                          private$pfid
                        },
                        
                        get_mic = function(){
                          private$mic
                        },
                        
                        get_mac = function(){
                          private$mac
                        },
                        
                        get_
                      ),
                      
                      private = list(
                        
                        pfid = NULL,
                        gtype = list(),
                        ptype = list(),
                        mic = list(),
                        mac = list(),
                        th = list(),
                        thEnd = list(),
                        sib = list(),
                        nAntigenLoci = integer(0),
                        nptypes = NULL
                        
                        
                      )
)