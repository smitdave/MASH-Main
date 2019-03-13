library(R6)

PDGHuman <- R6Class("PDGHuman",

                 ## Public Fields, Methods, and Initialization
                 public = list(

                   ## Initialization of Components

                   initialize = function(ixH = NA, age = NA, sex = NA, locH = NA){

                     private$ixH = ixH ## id number
                     private$age = age
                     private$sex = sex
                     private$locH = locH ## location (human)

                     private$pfAges = 16 ## 16 age categories of infection, for example
                     private$Pf = rep(0,private$pfAges)
                     private$Pt = NaN
                     private$Gt = NaN
                     private$ggr = .01 ## about .01 Gt produced per Pt
                     private$gdk = .7 ## about 70 percent of Gt die over 1-2 weeks (halflife around 10 days)
                     private$pfdr = .005 # .5 percent attrition rate of "old" infections - pf death rate
                     private$A = self$ageMatrix(private$pfAges)
                     private$Ptmu = log(c(0,8,10,12,11.5,11,10.5,10,9.5,9,8.5,8,7.5,7,7,7)) ## mean of lognormal densities for ages 1:pfAges; must be of length pfAges
                     private$Ptvar = rep(.1,16) ## var of " "; must be of length pfAges

                     private$Imm = 0
                     private$immCounter = 0
                     private$immHalf = 4
                     private$immSlope = 3
                     private$immThresh = 7.5

                     private$fever = 0
                     private$feverThresh = 8
                     
                     private$TE = 0
                     private$TEHalf = 1
                     private$TESlope = 1
                     private$TEMax = 1

                     private$history = list()

                   },


                   ######## Infection Methods #########


                   infect_Human = function(nInfections=1){
                      private$Pf[1] = private$Pf[1]+nInfections
                    },

                   clear_All_Pathogens = function(){
                      private$Pf = rep(0,private$pfAges)
                      private$Pt = 0
                      private$Gt = 0
                   },


                   ########## Update Functions #########


                   update_Human = function(){

                     ## these use old value of Pt, so must be computed first
                     self$update_Imm()
                     self$update_Gt()

                     ## these update respectively Pf, Pt, MOI, TE
                     self$age_Infections()
                     self$update_Pt()
                     self$update_MOI()
                     self$update_TE()

                     self$update_History()

                   },

                   age_Infections = function(){

                     ## removes from final category at a particular rate, relatively small
                     private$Pf[private$pfAges] = max(private$Pf[private$pfAges] - sum(rbinom(private$Pf[private$pfAges],1,private$pfdr)),0)

                     ## shifts to next age group
                     private$Pf = private$A %*% private$Pf

                   },

                   update_Pt = function(){

                     private$Pt = 0

                     ## pull from all of the age-specific distributions, sum to get total Pt; limit tails of dist'ns
                     for(i in 1:private$pfAges){
                       if(private$Pf[i] > 0){
                          private$Pt = log10(10^private$Pt + sum(10^(min(rlnorm(private$Pf[i],private$Ptmu[i],private$Ptvar[i]),13))))
                       }
                     }

                     ## don't care about very small numbers of parasites
                     if(private$Pt < 1){
                       private$Pt = NaN
                     }

                     ## include immune effect
                     ## this is a stub; here we just discount Pt by at most 99 percent
                     private$Pt = log10((1-.99*private$Imm)*10^private$Pt)


                   },

                   update_Gt = function(){

                     ## multiply previous Pt by the average Gt created per Pt, log scaling
                     ## sequestration/delay handled by the large (1-2 wk) time steps
                     if(is.na(private$Pt)==F){
                        private$Gt = ifelse(is.na(private$Gt), log10(private$ggr*10^private$Pt), log10((1-private$gdk)*10^private$Gt + private$ggr*10^private$Pt))
                        if(is.na(private$Gt)==F){
                          if(private$Gt < 3){
                            private$Gt = NaN
                          }
                        }
                     }
                     if(is.na(private$Pt)){
                       private$Gt = log10((1-private$gdk)*10^private$Gt)
                       if(is.na(private$Gt)==F){
                         if(private$Gt < 3){
                           private$Gt = NaN
                         }
                       }
                     }

                   },

                   update_MOI = function(){

                     ## add total active infections in each age category
                     private$MOI = sum(private$Pf)

                   },

                   update_Imm = function(){

                     ## count up if above threshhold parasite density, down if below
                     private$immCounter = ifelse(private$Pt < private$immThresh | is.nan(private$Pt), max(private$immCounter-.1,0), min(private$immCounter+1,10))
                     ## ensures nonnegative-definiteness of counters
                     private$immCounter = max(0,private$immCounter)
                     ## sigmoidal conversion of counter to immune effect
                     private$Imm = self$sigmoid(private$immCounter,private$immHalf,private$immSlope)

                   },
                   
                   update_TE = function(){
                     
                     ## scaled sigmoid signal; Gametocytes assumed to encode TE
                     private$TE = private$TEMax*sigmoidexp(private$Gt,private$TEHalf,private$TESlope)
                     
                   },

                   update_History = function(){

                     private$history$MOI = c(private$history$MOI,private$MOI)
                     private$history$Pt = c(private$history$Pt,private$Pt)
                     private$history$Gt = c(private$history$Gt,private$Gt)
                     private$history$Pf = c(private$history$Pf,private$Pf)
                     private$history$Imm = c(private$history$Imm,private$Imm)
                     private$history$immCounter = c(private$history$immCounter,private$immCounter)

                   },


                   ########## Accessors ##############


                   get_ixH = function(){
                     private$ixH
                   },

                   set_ixH = function(newixH){
                     private$ixH = newixH
                   },

                   get_age = function(){
                     private$age
                   },

                   set_age = function(newAge){
                     private$age = newAge
                   },

                   get_sex = function(){
                     private$sex
                   },

                   set_sex = function(newSex){
                     private$sex = newSex
                   },

                   get_locH = function(){
                     private$locH
                   },

                   set_locH = function(newlocH){
                     private$locH = newlocH
                   },

                   get_fever = function(){
                     private$fever
                   },

                   set_fever = function(newFever){
                     private$fever = newFever
                   },

                   get_Pf = function(){
                     private$Pf
                   },

                   get_Pt = function(){
                     private$Pt
                   },

                   get_Gt = function(){
                     private$Gt
                   },

                   get_MOI = function(){
                     private$MOI
                   },

                   get_history = function(){
                     private$history
                   },


                   ################# extra functions #################


                   ## polynomial sigmoid function
                   sigmoid = function(x,xhalf,b){
                     (x/xhalf)^b/((x/xhalf)^b+1)
                   },
                   
                   ## exponential sigmoid function
                   sigmoidexp = function(x,xhalf,b){
                     exp(x*b)/(exp(x*b)+exp(xhalf*b))
                   }

                   ## creates tridiagonal matrix, used to create aging matrix
                   tridiag = function(upper, lower, main) {
                     out = matrix(0,length(main),length(main))
                     diag(out) = main
                     indx = seq.int(length(upper))
                     out[cbind(indx+1,indx)] = lower
                     out[cbind(indx,indx+1)] = upper
                     return(out)
                   },

                   ## aging matrix - transitions parasites to next age group,
                   ## keeps those at end stationary ("oldest" class)
                   ageMatrix = function(size){
                     u = rep(0,size-1)
                     l = rep(1,size-1)
                     m = rep(0,size)
                     m[size] = 1
                     A = self$tridiag(u,l,m)
                   }

                 ),


                 ## Private Fields


                 private = list(

                   ## human base traits
                   ixH = NULL, ## human ID
                   age = NULL,
                   sex = NULL,
                   locH = NULL, ## human Location

                   ## Pf
                   Pf = NULL, ## list with number of infections at different age categories
                   pfAges = NULL, ## number of age classes
                   pfdr = NULL, ## probability of clearing old infection
                   A = NULL, ## aging matrix (shifts pf)
                   Pt = NULL, ## asexual parasite count
                   Gt = NULL, ## gametocyte count
                   ggr = NULL, ## "gametocyte growth rate" (per asexual rate of Gt production)
                   gdk = NULL, ## "gametocyte decay rate" (death rate of Gt)
                   Ptmu = NULL, ## vector of means of dist'ns of Pt for different age categories
                   Ptvar = NULL, ## " " variances " "
                   MOI = NULL, ## multiplicity of infection, sum of vector pf

                   ## Immunity
                   Imm = NULL, ## normalized immune strength
                   immHalf = NULL, ## half maximum immunity, sigmoid param
                   immSlope = NULL, ## slope of immune conversion, sigmoid param
                   immCounter = NULL, ## counts up if Pt > PtThresh, down otherwise
                   immThresh = NULL, ## immunogenic threshhold, based on Pt

                   ## Health
                   fever = NULL, ## fever, can be binary or graded (if graded, need sigmoid params)
                   feverThresh = NULL, ## fever threshhold, for binary fever (related to Pt)
                   
                   ## Infectivity
                   TE = NULL, ## transmission efficiency
                   TEHalf = NULL, ## half maximum transmission efficiency, sigmoid param
                   TESlope = NULL, ## slope of gametocyte to TE conversion, sigmoid param
                   TEMax = NULL, ## maximum transmission efficiency, sigmoid scaling

                   ## history
                   history = NULL ## list containing past densities, immunity, etc

                 )

)
