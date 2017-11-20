PfPedigree <- R6Class("PfPedigree",
                      
                      public = list(
                        initialize = function(){
                          private$pfid = 1
                          private$nAntigenLoci = 9
                          private$nptypes = c(3,5,4,3,6,3,2,7,9)
                        },
                        
                        add2Pedigree = function(pf){
                          ## extract all info from pf through get functions
                          ## record info in pedigree within hash table
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
                        }
                      ),
                      
                      private = list(
                        
                        pfid = NULL,
                        gtype = NULL,
                        ptype = NULL,
                        mic = NULL,
                        mac = NULL,
                        th = NULL,
                        thEnd = NULL,
                        sib = NULL,
                        nAntigenLoci = NULL,
                        nptypes = NULL
                        
                        
                      )
)