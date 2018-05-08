Human <- R6Class("PDGHuman",
                 
                 ## Public Fields, Methods, and Initialization
                 public = list(
                   
                   ## Initialization of Components
                   
                   initialize = function(ixH = NA, age = NA, sex = NA, locH = NA, IncImm=T, IncPfPed = T){
                     
                     private$ixH = ixH ## id number
                     private$age = age
                     private$sex = sex
                     private$locH = locH ## location (human)
                     
                     private$pfAges = 5 ## 5 age categories of infection, for example
                     private$pf = rep(0,private$pfAges)
                     private$Pt = 0
                     private$Gt = 0
                     private$ggr = .01 ## about .01 Gt produced per Pt
                     private$gdk = .5 ## about 50 percent of Gt die over 1-2 weeks (halflife around 10 days)
                     private$pfdr = .05 # 5 percent attrition rate of "old" infections - pf death rate
                     private$A = self$ageMatrix(5)
                     private$Ptmu = log(c(6,10,8,6,4)) ## mean of lognormal densities for ages 1:5; must be of length pfAges
                     private$Ptvar = c(.1,.1,.1,.1,.1) ## var of " "; must be of length pfAges
                     
                     private$Imm = 0
                     private$ImmCounter = 0
                     
                     private$fever = 0
                     private$feverThresh = 8
                     
                     private$history = list()
                     
                   },
                   
                   
                   ######## Infection Methods #########
                   
                   
                   infectHuman = function(){
                      private$pf[1] = private$pf[1]+1
                    },
                   
                   clearAllPathogens = function(){
                      private$pf = rep(0,5)
                   },
                   
                   moveHuman = function(newlocH){
                     self$set_locH(newlocH)
                   },
                   
                   Treat = function(t,Drug){
                     private$healthState$Treat(t,Drug)
                   },
                   
                   ########## Update Function #########
                   
                   
                   updateHuman = function(){
                     
                     ageInfections()
                     updatePt()
                     updateMOI()
                     updateImm()
                     updateHist()
                     
                   },
                   
                   ageInfections = function(){
                     
                     ## removes from final category at a particular rate, relatively small
                     private$pf[5] = private$pf[5] - sum(rbinom(5,1,private$pfdr))
                     
                     ## shifts to next age group
                     private$pf = private$A %*% private$pf
                     
                   },
                   
                   updatePt = function(){
                     
                     private$Pt = 0
                     
                     for(i in 1:5){
                       private$Pt = private$Pt + sum(rlnorm(private$pf[i],private$Ptmu[i],private$Ptvar[i]))
                     }
                     
                   },
                   
                   updateGt = function(){
                     
                     ## multiply previous Pt by the average Gt created per Pt, log scaling
                     ## sequestration/delay handled by the large (1-2 wk) time steps
                     private$Gt = log10((1-gdk)*10^private$Gt + private$ggr*10^private$Pt)
                     
                   }
                   
                   updateMOI = function(){
                     
                     ## add total active infections in each age category
                     private$MOI = sum(private$pf)
                     
                   },
                   
                   updateImm = function(){
                     
                     ## count up if above threshhold parasite density, down if below
                     private$ImmCounter = ifelse(Pt > private$PtThresh, private$ImmCounter+1, private$ImmCounter-1)
                     ## sigmoidal conversion of counter to immune effect
                     private$Imm = self$sigmoid(private$Pt,private$immHalf,private$immSlope)
                     
                   },
                   
                   updateHist = function(){
                     private$history$MOI = c(private$history$MOI,private$MOI)
                     private$history$Pt = c(private$history$Pt,private$Pt)
                     private$history$Gt = c(private$history$Gt,private$Gt)
                   }
                   
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
                   
                   get_pathogen = function(){
                     private$pf
                   },
                   
                   get_Pt = function(){
                     sum(private$Pt)
                   },
                   
                   get_Gt = function(){
                     private$pathogen$get_Gt()
                   },
                   
                   get_Drug = function(){
                     private$healthState$get_Drug()
                   },
                   
                   get_RxStart = function(){
                     private$healthState$get_RxStart()
                   },
                   
                   get_PD = function(){
                     private$healthState$get_PD()
                   },
                   
                   get_Fever = function(){
                     private$healthState$get_Fever()
                   },
                   
                   get_PfMOI = function(){
                     private$pathogen$get_PfMOI()
                   },
                   
                   get_history = function(){
                     
                   },
                   
                   
                   ################# extra functions #################
                   
                   
                   sigmoid = function(x,xhalf,b){
                     (x/xhalf)^b/((x/xhalf)^b+1)
                   },
                   
                   tridiag = function(upper, lower, main) {
                     out = matrix(0,length(main),length(main))
                     diag(out) = main
                     indx = seq.int(length(upper))
                     out[cbind(indx+1,indx)] = lower
                     out[cbind(indx,indx+1)] = upper
                     return(out)
                   },
                   
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
                   pf = NULL, ## list with number of infections at different age categories
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
                   
                   ## Health
                   fever = NULL, ## fever, can be binary or graded (if graded, need sigmoid params)
                   feverThresh = NULL, ## fever threshhold, for binary fever (related to Pt)
                   
                   ## history
                   history = NULL ## list containing past densities, immunity, etc
                   
                 )
                 
)

