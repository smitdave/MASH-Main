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

                     private$pfAges = 27 ## 28 age categories of infection, fortnights for a full year plus one latent stage and one subpatent stage
                     private$Pf = rep(0,private$pfAges)
                     private$Pt = NaN
                     private$Gt = NaN
                     private$pgm = 1.184 ## slope of log10 asexual-to-gametocyte power law
                     private$pgb = -2.004 ## y-intercept of log10 asexual-to-gametocyte power law
                     private$pgv = .2704 ## sample variance of residuals of asexual-to-gametocyte power law fit
                     private$gm = 1.652 ## slope of log10 mean-variance power law for gametocytes
                     private$gb = 1.662 ## y intercept of log10 mean-variance power law for gametocytes
                     
                     private$gdk = log10(.7) ## about 70 percent of Gt die over 2 weeks (halflife around 10 days); this determines lingering gametocyte densities if asexuals disappear first
                     private$pfdr = .75 ## small rate at which old infections are removed (this sets a 10 percent chance per fortnight of clearing subpatent infection; this is by wild unmotivated assumption, not fitted)
                     ## ***** ^^^^^ This is still an unknown parameter *****
                     private$pfpatency = 1-exp(-.1385) # probability an infection enters subpatent phase within next fortnight
                     private$A = self$ageMatrix(private$pfAges)
                     private$MOI = 0 ## multiplicity of infection, initially set to 0
                     ## set max densities, and shape-rate parameters for log gamma dist'ns
                     private$Ptmax = c(0,5.210246,4.573131,4.373306,4.218014,4.079337,4.157638,3.805582,3.821811,3.827757,3.467524,3.740757,3.349316,3.732802,3.652580,3.231724,2.567026,2.283804,2.929233,1.962211,2.944931,1.851258,2.193125,2.309303,1.564271,3.205746,2.719204,1.915100)
                     #smoothed: c(0,5.210246,4.492700,4.385300,4.277900,4.170500,4.063100,3.955700,3.848300,3.740900,3.633500,3.526100,3.418700,3.311300,3.203900,3.096500,2.989100,2.881700,2.774300,2.666900,2.559500,2.452100,2.344700,2.237300,2.129900,2.022500,1.915100,1.915100)
                     private$Ptshape = c(0,4.639315,2.275418,2.258965,2.311616,2.474827,3.176543,2.943449,3.419812,4.387235,2.755179,5.014499,4.114957,8.849801,6.918817,4.470316,3.726024,4.332549,4.547252,1.829113,5.378781,1.581417,1.094105,1.000000,1.030259,2.641712,2.991721,5.007464)
                     #smoothed: c(0,4.639315,2.714693,2.796321,2.879157,2.963203,3.048457,3.134921,3.222594,3.311476,3.401567,3.492868,3.585377,3.679095,3.774023,3.870160,3.967505,4.066060,4.165824,4.266797,4.368979,4.472371,4.576971,4.682781,4.789799,4.898027,5.007464,5.007464)
                     private$Ptrate = c(0,4.008275,1.539217,1.612759,1.756409,1.907544,2.034369,1.949314,2.043045,2.571400,1.800733,2.482635,2.385546,3.573325,3.135033,2.244577,2.825106,3.004125,2.390421,2.198324,2.916877,1.992531,1.051514,1.572928,1.460975,1.294750,2.077740,2.618999)
                     #smoothed: c(0,4.008275,1.928354,1.957131,1.985908,2.014685,2.043462,2.072239,2.101016,2.129793,2.158569,2.187346,2.216123,2.244900,2.273677,2.302454,2.331231,2.360008,2.388784,2.417561,2.446338,2.475115,2.503892,2.532669,2.561446,2.590223,2.618999,2.618999)
                     
                     private$Imm = 0
                     private$immCounter = 0
                     private$immHalf = 3.5246
                     private$immSlope = 3.038
                     private$immThresh = 2

                     private$pFever = 0
                     private$feverHalf = 3.5246
                     private$feverSlope = 3.038
                     private$feverMax = .8835
                     
                     private$TE = 0
                     private$TEHalf = 2.3038
                     private$TESlope = 3.5524
                     private$TEMax = .4242
                     
                     private$LMHalf = 2
                     private$LMSlope = 3
                     private$LMMax = .9
                     private$LMMin = .05

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
                          private$Pt = log10(10^private$Pt + sum(10^(private$Ptmax[i]-rgamma(private$Pf[i],shape=private$Ptshape[i],rate=private$Ptrate[i])),na.rm=T))
                       }
                     }

                     ## include immune effect
                     ## this is a stub; here we just discount Pt by at most 99 percent
                     private$Pt = log10((1-.99*private$Imm)*10^private$Pt)
                     
                     if(private$MOI == 0){
                       private$Pt = NaN
                     }


                   },

                   update_Gt = function(){
                     
                     ## use power law to translate from Pt to Gt; add unbiased noise due to uncertainty in P2G fit
                     if((sum(private$Pf,na.rm=T) > 0) ){
                       private$Gt = private$pgm*private$Pt + private$pgb
                       private$Gt = ifelse((private$Gt*private$gm + private$gb)>0, private$Gt+rnorm(1,0,sqrt(private$pgv)), NaN)
                     }
                     if(sum(private$Pf,na.rm=T) == 0){
                       private$Gt = private$Gt + private$gdk
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
