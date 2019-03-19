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

                     private$pfAges = 26 ## 26 age categories of infection, fortnights for a full year plus one latent stage
                     private$Pf = rep(0,private$pfAges)
                     private$Pt = NaN
                     private$Gt = NaN
                     private$pgm = 1.184 ## slope of log10 asexual-to-gametocyte power law
                     private$pgb = -2.004 ## y-intercept of log10 asexual-to-gametocyte power law
                     private$gm = 1.652 ## slope of log10 mean-variance power law for gametocytes
                     private$gb = 1.662 ## y intercept of log10 mean-variance power law for gametocytes
                     
                     #private$ggr = .01 ## about .01 Gt produced per Pt
                     
                     private$gdk = log10(.7) ## about 70 percent of Gt die over 2 weeks (halflife around 10 days); this determines lingering gametocyte densities if asexuals disappear first
                     private$pfdr = .05 ## small rate at which old infections are removed ***** This is still an unknown parameter *****
                     private$pfpatency = 1-exp(-.1385412) # probability an infection enters subpatent phase within next fortnight
                     private$A = self$ageMatrix(private$pfAges)
                     private$Ptmu = c(0,4.304,3.586,3.373,3.276,3.165,3.043,2.8,2.67,2.517,2.393,2.266,2.05,1.99,2.005,1.818,1.280,.897,1.309,.804,1.22,.553,.671,.561,.1719,1.494,1.199) ## mean of densities for ages 1:pfAges; must be of length pfAges
                     private$Ptvar = (1.63*private$Ptmu+1.688) ## var of " "; must be of length pfAges. Here using power law relationship between mean and variance
                     private$MOI = 0 ## multiplicity of infection, initially set to 0
                     
                     private$Imm = 0
                     private$immCounter = 0
                     private$immHalf = 4
                     private$immSlope = 3
                     private$immThresh = 7.5

                     private$pFever = 0
                     private$feverHalf = 3.5246
                     private$feverSlope = 3.038
                     private$feverMax = .8835
                     
                     private$TE = 0
                     private$TEHalf = 2.3038
                     private$TESlope = 3.5524
                     private$TEMax = .4242

                     private$history = list()

                   },


                   ######## Infection Methods #########


                   infect_Human = function(nInfections=1){
                      private$Pf[1] = private$Pf[1]+nInfections
                      private$MOI = private$MOI+1
                    },

                   clear_All_Pathogens = function(){
                      private$Pf = rep(0,private$pfAges)
                      private$Pt = 0
                      private$Gt = 0
                   },


                   ########## Update Functions #########


                   update_Human = function(){

                     ## this uses old value of Pt, so must be computed first
                     ## self$update_Imm() ## turning off immunity for now

                     ## these update respectively Pf, Pt, MOI, TE
                     self$age_Infections()
                     self$update_Pt()
                     self$update_Gt()
                     self$update_MOI()
                     self$update_TE()
                     self$update_pFever()

                     self$update_History()

                   },

                   age_Infections = function(){

                     ## removes from final category at a particular rate, relatively small
                     if(private$Pf[private$pfAges] > 0){
                        private$Pf[private$pfAges] = max(private$Pf[private$pfAges] - sum(rbinom(private$Pf[private$pfAges],1,private$pfdr)),0)
                     }

                     ## some proportion of patent infections move into subpatent phase; each independent
                     if(((private$MOI - private$Pf[private$pfAges]-private$Pf[1]) > 0)& (sum(private$Pf,na.rm=T) > 0) ){ ## we want to only move them into subpatency AFTER the intrinsic incubation period & first fortnight of infection
                        term = rbinom(private$MOI-private$Pf[private$pfAges],1,private$pfpatency)   ## how many patent cohorts to "terminate"
                        subs = rmultinom(1,term,private$Pf/sum(private$Pf,na.rm=T)) ## which age cohorts are being removed
                        private$Pf = private$Pf - subs ## remove the newly subpatent infections
                        private$Pf[private$pfAges] = private$Pf[private$pfAges]+sum(subs) ## add the subpatent infections to oldest age group
                     }
                     
                     ## shifts to next age group
                     private$Pf = private$A %*% private$Pf

                   },

                   update_Pt = function(){

                     private$Pt = 0

                     ## pull from all of the age-specific distributions, sum to get total Pt; limit tails of dist'ns
                     for(i in 1:private$pfAges){
                       if(private$Pf[i] > 0){
                          private$Pt = log10(10^private$Pt + sum(10^(rnorm(private$Pf[i],private$Ptmu[i],sqrt(private$Ptvar[i])/10)))) ## this shifts and reverses the lognormal
                       }
                     }

                     ## don't care about very small numbers of parasites
                     if(private$Pt < 0){
                        private$Pt = NaN
                     }

                     ## include immune effect
                     ## this is a stub; here we just discount Pt by at most 99 percent
                     private$Pt = log10((1-.99*private$Imm)*10^private$Pt)
                     
                     if(private$MOI == 0){
                       private$Pt = NaN
                     }


                   },

                   update_Gt = function(){
                    
                     ## OLD ALGORITHM
                     
                     ## multiply previous Pt by the average Gt created per Pt, log scaling
                     ## sequestration/delay handled by the large (1-2 wk) time steps
                     #if(is.na(private$Pt)==F){
                     #    private$Gt = ifelse(is.na(private$Gt), log10(private$ggr*10^private$Pt), log10((1-private$gdk)*10^private$Gt + private$ggr*10^private$Pt))
                     #    if(is.na(private$Gt)==F){
                     #      if(private$Gt < 3){
                     #        private$Gt = NaN
                     #      }
                     #    }
                     # }
                     #if(is.na(private$Pt)){
                     # private$Gt = log10((1-private$gdk)*10^private$Gt)
                     # if(is.na(private$Gt)==F){
                     #   if(private$Gt < 3){
                     #     private$Gt = NaN
                     #   }
                     # }
                     # }
                     
                     ## NEW ALGORITHM
                     
                     ## use power law to translate from Pt to Gt; add unbiased noise to account for mean-variance power law in gametocytes
                     if((sum(private$Pt,na.rm=T) > 0) ){
                       private$Gt = private$pgm*private$Pt + private$pgb
                       private$Gt = ifelse((private$Gt*private$gm + private$gb)>0, private$Gt + rnorm(1,0,sqrt(private$Gt*private$gm + private$gb)/14), NaN)
                     }
                     if(sum(private$Pt,na.rm=T) == 0){
                       private$Gt = private$Gt + private$gdk
                       private$Gt = private$Gt + rnorm(1,0,sqrt(private$Gt*private$gm + private$gb)/14)
                     }
                     if(sum(private$Gt,na.rm=T) < .5){
                       private$Gt = NaN
                     }

                   },

                   update_MOI = function(){

                     ## add total active infections in each age category
                     private$MOI = sum(private$Pf,na.rm=T)

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
                     private$TE = private$TEMax*self$sigmoidexp(private$Gt,private$TEHalf,private$TESlope)
                     
                   },
                   
                   update_pFever = function(){
                     
                     private$pFever = private$feverMax*self$sigmoidexp(private$Pt,private$feverHalf,private$feverSlope)
                       
                   },

                   update_History = function(){

                     private$history$MOI = c(private$history$MOI,private$MOI)
                     private$history$Pt = c(private$history$Pt,private$Pt)
                     private$history$Gt = c(private$history$Gt,private$Gt)
                     private$history$Pf = c(private$history$Pf,private$Pf)
                     private$history$TE = c(private$history$TE,private$TE)
                     private$history$pFever = c(private$history$pFever,private$pFever)
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

                   get_pFever = function(){
                     private$pFever
                   },

                   set_pFever = function(newpFever){
                     private$pFever = newpFever
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
                   
                   get_TE = function(){
                     private$TE
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
                   },

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
                   #ggr = NULL, ## "gametocyte growth rate" (per asexual rate of Gt production)
                   gdk = NULL, ## "gametocyte decay rate" (death rate of Gt in absence of Pt)
                   pgm = NULL, ## slope of log10 Asexual to Gametocyte density function (power law)
                   pgb = NULL, ## y-intercept of log10 Asexual to Gametocyte density function (power law)
                   gm = NULL, ## slope of log10 mean-variance power law for gametocytes
                   gb = NULL, ## y intercept of log10 mean-variance power law for gametocytes
                   Ptmu = NULL, ## vector of means of dist'ns of Pt for different age categories
                   Ptvar = NULL, ## " " variances " "
                   MOI = NULL, ## multiplicity of infection, sum of vector pf
                   pfpatency = NULL, ## rate at which infections leave patency, become subpatent infection

                   ## Immunity
                   Imm = NULL, ## normalized immune strength
                   immHalf = NULL, ## half maximum immunity, sigmoid param
                   immSlope = NULL, ## slope of immune conversion, sigmoid param
                   immCounter = NULL, ## counts up if Pt > PtThresh, down otherwise
                   immThresh = NULL, ## immunogenic threshhold, based on Pt

                   ## Health
                   pFever = NULL, ## probability of fever; probability seeking treatment should be related to number of febrile days
                   feverHalf = NULL, ## half maximum prob of fever, sigmoid param
                   feverSlope = NULL, ## slope of prob of fever conversion, sigmoid param
                   feverMax = NULL, ## maximum prob of fever, sigmoid scaling param
                   
                   ## Infectivity
                   TE = NULL, ## transmission efficiency
                   TEHalf = NULL, ## half maximum transmission efficiency, sigmoid param
                   TESlope = NULL, ## slope of gametocyte to TE conversion, sigmoid param
                   TEMax = NULL, ## maximum transmission efficiency, sigmoid scaling

                   ## history
                   history = NULL ## list containing past densities, immunity, etc

                 )

)
