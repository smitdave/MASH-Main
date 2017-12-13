Mosquito <- R6Class("Mosquito",
                       
                       public = list(
                         
                         initialize = function(){

                         },
                         
                         
                         ####### Accessors ########

                         
                         
                         ############ Update Methods ##############
                         
                         update_SPZ = function(){
                           
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
                         
                         
                         update_history = function(){

                         }
                         
                         
                         
                       ),
                       
                       
                       private = list(
                         SPZ = NULL
                       )
)
                       