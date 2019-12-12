library(R6)

PDGHuman <- R6Class("PDGHuman",

                 ## Public Fields, Methods, and Initialization
                 public = list(

                   ## Initialization of Components

                   initialize = function(ixH = NA, age = NA, sex = NA, locH = NA, dt = NA){

                     private$ixH = ixH ## id number
                     private$age = age
                     private$sex = sex
                     private$locH = locH ## location (human)

                     private$pfAges = 37 ## 37 age categories of infection, 10 day intervals for a full year plus one latent stage and one subpatent stage
                     private$Pf = rep(0,private$pfAges)
                     private$Pt = NaN
                     private$Gt = NaN
                     private$pgm = 1.184 ## slope of log10 asexual-to-gametocyte power law
                     private$pgb = -2.004 ## y-intercept of log10 asexual-to-gametocyte power law
                     private$pgv = .2704 ## sample variance of residuals of asexual-to-gametocyte power law fit
                     private$gm = 1.652 ## slope of log10 mean-variance power law for gametocytes
                     private$gb = 1.662 ## y intercept of log10 mean-variance power law for gametocytes
                     
                     private$gdk = log10(.5) ## halflife of Gt around 10 days); this determines lingering gametocyte densities if asexuals disappear first
                     private$pfdr = .75 ## small rate at which old infections are removed (this is by wild unmotivated assumption, not fitted)
                     ## ***** ^^^^^ This is still an unknown parameter *****
                     private$pfpatency = 1-exp(-.0984) # probability an infection enters subpatent phase within next 10 day timestep
                     private$A = self$ageMatrix(private$pfAges)
                     private$MOI = 0 ## multiplicity of infection, initially set to 0
                     ## set max densities, and shape-rate parameters for log gamma dist'ns
                     private$Ptmax = c(0,5.210029,5.098377,4.345852,4.479676,4.415912,4.232222,4.120041,4.317347,3.792151,4.066512,3.851283,3.815943,3.786396,3.463236,3.762829,3.542265,3.334655,3.796081,2.965578,3.866064,3.431900,2.764674,2.361728,2.146128,3.035730,1.857332,2.213075,3.138077,1.892095,2.443263,2.681241,1.707570,1.740363,1.367977,3.427594,2.707570)
                     private$Ptshape = c(0,3.978623,2.678468,1.878722,2.493990,3.094425,2.459693,2.666507,3.606924,2.941532,4.287920,3.661314,4.011108,4.135072,2.847501,5.412349,3.585518,6.056590,8.460887,3.793583,8.594032,5.551457,4.206740,4.083759,5.279131,3.625526,1.469722,2.499612,5.076690,2.162326,2.812369,1.998919,1.000000,1.340513,1.000000,2.612531,2.973956)
                     private$Ptrate = c(0,3.585977,1.501009,1.406488,1.598298,2.046123,1.756996,1.917906,2.126131,2.042600,2.388907,2.230532,2.342005,2.310496,1.842563,2.688642,2.018155,3.501845,3.444821,2.286649,3.498115,2.639891,2.986170,3.436599,3.970240,1.983717,1.924545,2.246207,2.554646,2.884792,1.813013,1.202784,4.223981,1.449905,1.874508,1.261668,2.832022)
                     
                     private$Imm = 0
                     private$immCounter = 0
                     private$immHalf = 3.5246
                     private$immSlope = 3.038
                     private$gamma = 1
                     private$immThresh = 0

                     private$pFever = 0
                     private$feverHalf = 3.463
                     private$feverSlope = 3.851
                     private$feverMax = .8835
                     
                     private$TE = 0
                     private$TEHalf = 2.01
                     private$TESlope = 2.18
                     private$TEMax = .6753
                     
                     private$LMHalf = 2
                     private$LMSlope = 3
                     private$LMMax = .9
                     private$LMMin = .05

                     private$history = list()

                   },


                   ######## Infection Methods #########


                   infect_Human = function(nInfections=1){
                      private$Pf[1] = private$Pf[1]+nInfections
                      private$MOI = private$MOI+nInfections
                      private$MOI = sum(private$Pf)
                    },

                   clear_All_Pathogens = function(){
                      private$Pf = rep(0,private$pfAges)
                      private$Pt = 0
                      private$Gt = 0
                   },


                   ########## Update Functions #########


                   update_Human = function(dt){

                     ## this uses old value of Pt, so must be computed first
                     ## self$update_Imm() ## turning off immunity for now

                     ## these update respectively Pf, Pt, MOI, TE
                     self$update_Gt()
                     self$age_Infections()
                     self$update_Pt()
                     self$update_MOI()
                     self$update_TE()
                     self$update_pFever()
                     
                     self$update_Age(dt)

                     self$update_History()

                   },

                   age_Infections = function(){

                     ## removes from final category at a particular rate, relatively small
                     if(!is.na(tail(private$Pf)[1])){
                        if(tail(private$Pf)[1] > 0){
                          private$Pf[private$pfAges] = max(private$Pf[private$pfAges] - sum(rbinom(n = 1,size = private$Pf[private$pfAges],prob = private$pfdr)),0)
                        }
                     }

                     ## some proportion of patent infections move into subpatent phase; each independent
                     if(((private$MOI - private$Pf[private$pfAges]-private$Pf[1]) > 0)& (sum(private$Pf,na.rm=T) > 0) ){ ## we want to only move them into subpatency AFTER the intrinsic incubation period & first fortnight of infection
                        term = rbinom(n = 1,size = private$MOI-private$Pf[private$pfAges],prob = private$pfpatency)   ## how many patent cohorts to "terminate"
                        # subs = rmultinom(1,term,private$Pf/sum(private$Pf,na.rm=T)) ## which age cohorts are being removed
                        # only draw the random number if there's infections to move and more than one total
                        if(term > 0){
                          if(sum(private$Pf) > 1){
                            subs = as.vector(extraDistr::rmvhyper(nn = 1,n = as.vector(private$Pf),k = term))
                            private$Pf = private$Pf - subs ## remove the newly subpatent infections
                            private$Pf[private$pfAges] = private$Pf[private$pfAges]+sum(subs) ## add the subpatent infections to oldest age group  
                          } else {
                          # in this case there's only 1 infection, move it directly to the end
                            private$Pf <- private$Pf*0
                            private$Pf[private$pfAges] <- 1
                          }
                        }
                     }
                     
                     ## shifts to next age group
                     private$Pf = private$A %*% private$Pf

                   },
                   
                   update_Pt = function(){

                     private$Pt = 0

                     ## pull from all of the age-specific distributions, sum to get total Pt; limit tails of dist'ns
                     for(i in 1:private$pfAges){
                       if(!is.na(private$Pf[i])){
                          if(private$Pf[i] > 0){
                            private$Pt = log10(10^private$Pt + sum(10^(private$Ptmax[i]-rgamma(private$Pf[i],shape=private$Ptshape[i],rate=private$Ptrate[i])),na.rm=T))
                          }
                       }
                     }

                     ## include immune effect
                     ## this is a stub; here we just discount Pt by at most 50 percent
                     private$Pt = log10((1-.5*private$Imm)*10^private$Pt)
                     
                     if(!is.na(private$MOI)){
                       if(private$MOI == 0){
                         private$Pt = NaN
                       } 
                     }


                   },

                   update_Gt = function(){
                     
                     ## use power law to translate from Pt to Gt; add unbiased noise due to uncertainty in P2G fit
                     if((sum(private$Pf,na.rm=T) > 0) ){
                       if(is.na(private$Pt) | is.nan(private$Pt)){
                         private$Gt <- NaN
                       } else {
                         private$Gt = private$pgm*private$Pt + private$pgb
                         private$Gt = ifelse((private$Gt*private$gm + private$gb)>0, private$Gt+rnorm(1,0,sqrt(private$pgv)), NaN)
                       }
                       # private$Gt = private$pgm*private$Pt + private$pgb
                       # private$Gt = ifelse((private$Gt*private$gm + private$gb)>0, private$Gt+rnorm(1,0,sqrt(private$pgv)), NaN)
  
                       
                     } else {
                       private$Gt = private$Gt + private$gdk
                     }
                     # if(sum(private$Pf,na.rm=T) == 0){
                     #   # private$Gt = max(private$Gt + private$gdk,0)
                     #   private$Gt = private$Gt + private$gdk
                     # }

                   },

                   update_MOI = function(){

                     ## add total active infections in each age category
                     private$MOI = sum(private$Pf,na.rm=T)

                   },

                   update_Imm = function(){

                     ## count up at rate proportional to Pt if above threshold, down by fixed proportion if below
                     private$immCounter = ifelse(private$Pt < private$immThresh | is.nan(private$Pt), private$immCounter*exp(-private$gamma*dt), private$immCounter+dt*private$Pt)
                     ## sigmoidal conversion of counter to immune effect
                     private$Imm = self$sigmoid(private$immCounter,private$immHalf,private$immSlope)

                   },
                   
                   update_TE = function(){
                     
                     ## scaled sigmoid signal; Gametocytes assumed to encode TE
                     private$TE = private$TEMax*self$sigmoidexp(private$Gt,private$TEHalf,private$TESlope)
                     
                   },
                   
                   update_pFever = function(){
                     
                     private$pFever = private$feverMax*self$sigmoidexp(private$Pt,private$feverHalf,private$feverSlope)
                     private$pFever = ifelse(is.na(private$pFever), 0, private$pFever)
                     
                   },
                   
                   update_Age = function(dt){
                     private$age = private$age+dt
                   },
                   
                   ## light microscopy - a function that can be called from the human object, will calculate a probability of testing positive using
                   ## asexual densities, then pull a random number with that probability of success. Returns 0/1 for negative/positive
                   LM = function(){
                     p = 0
                     if(private$Pt>0){
                        p = (private$LMMAx-private$LMMin)*self$sigmoidexp(private$Pt,private$LMHalf,private$LMSlope)+private$LMMin 
                     }
                     test = rbinom(1,1,p)
                     return(test)
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
                     private$history$age = c(private$history$age,private$age)

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


                   ## polynomial sigmoid function, defined over nonnegative real line
                   sigmoid = function(x,xhalf,b){
                     (x/xhalf)^b/((x/xhalf)^b+1)
                   },
                   
                   ## exponential sigmoid function, defined over whole real line
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
                   Ptmax = NULL, ## vector of max of dist'ns of Pt for different infection age categories
                   Ptshape = NULL, ## " " shape "
                   Ptrate = NULL, ## " " rate "
                   MOI = NULL, ## multiplicity of infection, sum of vector pf
                   pfpatency = NULL, ## rate at which infections leave patency, become subpatent infection
                   pgv = NULL, ## variance associated with asexual-to-gametocyte translation (power law)

                   ## Immunity
                   Imm = NULL, ## normalized immune strength
                   immHalf = NULL, ## half maximum immunity, sigmoid param
                   immSlope = NULL, ## slope of immune conversion, sigmoid param
                   immCounter = NULL, ## counts up if Pt > PtThresh, down otherwise
                   immThresh = NULL, ## immunogenic threshhold, based on Pt
                   gamma = NULL, ## rate of immune waning

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
                   
                   ## Diagnostics - Light Microscopy
                   LMHalf = NULL, ## Asexual density that gives 50 percent change of testing positive by Light Microscopy
                   LMSlope = NULL, ## slope of sigmoid converting Asexual density to probability of testing positive
                   LMMax = NULL, ## maximum probability of testing positive; 1 - (type 2 error + type 1 error)
                   LMMin = NULL, ## minimum probability of testing postiive; type 1 error

                   ## history
                   history = NULL ## list containing past densities, immunity, etc

                 )

)
