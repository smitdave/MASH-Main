micro.input.data.gen = function (num.patches=10, num.houses=200*num.patches, avg.num.humans.per.house=5, num.neighbors=4, 								
								num.days=100, num.runs=1, num.days.per.output=1, file.name.postfix = "", patches.file.name="micro.patches",
        	              		houses.file.name="micro.houses", humans.file.name="micro.humans", lambda.file.name="micro.lambda",
                                pTreat=0.5, lambda.scenario=3, const.lambda=1, itn=-1, itn.use = -1, itn.age = -1,
            	          		ponds.file.name="micro.ponds", infections.file.name="micro.infections", config.file.name="micro.in", mozzy.life.span = 10, 
                	      		mozzy.sporogony.days=10, mozzy.rest.days=1, mozzy.prob.rest.indoor=0.9, mozzy.prob.alive.rest=0.9, mozzy.oviposit.days=1,
	  	                  	  	mozzy.prob.alive.oviposit=0.9, mozzy.search.algo=2, mozzy.num.search.houses=50, mozzy.num.search.ponds=4,
                      			patch.wise.initial.infection=0, initial.pop.infected=0.5, incubation.days=10, latency.days=14, 
                      			mean.infection.days=200, prob.travel.abroad=-1, clinic.test.tp=0.8, clinic.test.tn=0.8, 
                      			clinic.drug.accuracy=1, acd.resolution=2, acd.trigger.prob=0.5, acd.radius=10, acd.intervention=1,
                      			acd.capacity=50, drug.prophylaxis=30, drug.days.to.clear.inf=12, itn.repelling.effect=0.3, irs.repelling.effect=0.5, itn.decay.rate=0.005, 
                      			irs.decay.rate=0.001, mass.vaccination.days=c(100,2000), mass.vaccination.coverage=0.9, 
                      			pe.uptake.coverage=0.9, tb.uptake.coverage=0.9, mass.drug.admin.days=c(150,230), mass.drug.admin.coverage=0.9,   
                      			mass.spraying.days=c(230,400,6000), mass.spraying.coverage=0.9, mass.itn.dist.days=c(132,255), 
                      			mass.itn.dist.coverage=0.9) {
                                
    houses.file.name<-paste(houses.file.name,file.name.postfix,sep="")
    patches.file.name<-paste(patches.file.name,file.name.postfix,sep="")
    ponds.file.name<-paste(ponds.file.name,file.name.postfix,sep="")
    humans.file.name<-paste(humans.file.name,file.name.postfix,sep="")
    lambda.file.name<-paste(lambda.file.name,file.name.postfix,sep="")
    config.file.name<-paste(config.file.name,file.name.postfix,sep="")
    infections.file.name<-paste(infections.file.name,file.name.postfix,sep="")

	# Patch centroids							
	patch.ids = 1:num.patches
	xy = 1:(ceiling(sqrt(num.patches)))*1000
	patch.X = rep(xy, each=length(xy), length=num.patches)
	patch.Y = rep(xy, length=num.patches)
  	
  	
  	
  	# Houses
  	house.X = round(runif(num.houses-1, 0, xy))
  	house.Y = round(runif(num.houses-1, 0, xy))	#with replacement?
  	house.humans = 3 + rpois((num.houses-1), avg.num.humans.per.house-3)
  	num.humans = sum(house.humans)
	w = rgamma.alpha(num.houses-1, 3) 
	Q = 0.05 
	w = w/(Q + sum(w)) 
	
	#COW = rgamma.alpha(num.houses-1, 3)
	#COW = COW / sum(COW)*.05*num.humans/(num.houses-1)
	COW = rep(0, num.houses-1)
	findPatch = function(i, x,y ) {
  		#which.min( sqrt((x[i]-patch.X)^2 + (y[i]-Patch.Y)^2))
  		which.min((x[i]-patch.X)^2 + (y[i]-patch.Y)^2)
	}
	house.pids = sapply(1:(num.houses-1), findPatch, x = house.X, y=house.Y)  
	HOUSE = data.frame(ID = 0:(num.houses-1), X=c(0,house.X), Y=c(0,house.Y),
	w=c(Q,w), COW = c(1,COW), P=c(0,house.pids), nH=c(0,house.humans)) 
	write.table (HOUSE, houses.file.name, row.names = F)
	
	
	
	# Patches
	find.patch.neighbors = function(i, x, y, n) {
		dist.order = ((order((x[i]-patch.X)^2 + (y[i]-patch.Y)^2)))[1:n+1]
		dist.order[dist.order!=i]
	}
	patch.houses = as.vector(table(house.pids))
	neighbors = sapply(1:num.patches, find.patch.neighbors, x = patch.X, y=patch.Y, n=num.neighbors)
	#neighbors = do.call(rbind,lapply(patch.ids, function(x) sample(patch.ids[-i], num.neighbors)))	
	patches=data.frame(ID=patch.ids, X=patch.X, Y=patch.Y, nHouses=patch.houses, nPonds=1, N=t(neighbors))  
  	write.table(patches, patches.file.name, row.names = FALSE,na="", quote=FALSE)
	
	
	# Ponds
	num.ponds = num.patches
	pond.X = patch.X
	pond.Y = patch.Y
	w = rgamma.alpha(num.ponds, 3) 
	lambda = rgamma.alpha(num.ponds, 3) 
	Pp = sapply(1:num.ponds, findPatch, x = pond.X, y=pond.Y)  
	POND = data.frame(X=pond.X, Y=pond.Y, w, Pp, lambda) 
	write.table (POND, ponds.file.name, row.names = F)



	# Humans 
	bday = - pmax(round(rexp(num.humans, rate = 1/50/365)), 85*365)
	dday = pmax(round(rexp(num.humans, rate = 1/50/365)), 80*365) 
	H = rep(c(1:(num.houses-1)), house.humans) 
	Ph = rep(house.pids,house.humans) 
	w = rgamma.alpha(num.humans, 3)
	w = w/(Q+sum(w))
	#pTreat = 0.5 + runif(num.humans)/2
	if (itn == -1) {itn = round(runif(num.humans, 0, 1))}
	if (itn.use == -1) {itn.use = 1/3 + 2*runif(num.humans)/3}
	if (itn.age == -1) {itn.age = rexp(num.humans, rate = 1/5)*365}
	HUMAN = data.frame(ID=1:num.humans, bday, dday, H, P=Ph, w, c=0.1, b=0.65,  atRisk = 0.9, pTreat, itn, itn.use, itn.age)
	write.table(HUMAN, humans.file.name, row.names = F) 


	# Lambda
	if (lambda.scenario == 1) {
    	a= 0.35  # scale max = a*b+a+c
    	b= 10  # seasonality
    	c= 0.1 # minimum baseline value
    	lambda.a = pmax(0,a*(1 + b*sin(2*pi*c(1:365)/365))) + c 
    }
  	if (lambda.scenario == 2) {lambda.a = rep(const.lambda,365)}
  	if (lambda.scenario == 3) {lambda.a = read.table("default.lambda", header=TRUE)}
  	lambda.a = data.frame(lambda.a=lambda.a)
  	write.table(lambda.a, lambda.file.name, row.names = FALSE, na="", quote=FALSE)
	
	
	
	# Configuration File
	micro.config.file(config.file.name, num.days, num.runs, num.days.per.output, patches.file.name,
                      houses.file.name, humans.file.name, lambda.file.name,
                      ponds.file.name, infections.file.name, mozzy.life.span, mozzy.sporogony.days, 
                      mozzy.rest.days, mozzy.prob.rest.indoor, mozzy.prob.alive.rest, mozzy.oviposit.days,
                      mozzy.prob.alive.oviposit, mozzy.search.algo, mozzy.num.search.houses, mozzy.num.search.ponds,
                      patch.wise.initial.infection, initial.pop.infected, incubation.days, latency.days, 
                      mean.infection.days, prob.travel.abroad, clinic.test.tp, clinic.test.tn, 
                      clinic.drug.accuracy, acd.resolution, acd.trigger.prob, acd.radius, acd.intervention,
                      acd.capacity, drug.prophylaxis, drug.days.to.clear.inf, itn.repelling.effect, irs.repelling.effect, itn.decay.rate, 
                      irs.decay.rate, mass.vaccination.days, mass.vaccination.coverage, 
                      pe.uptake.coverage, tb.uptake.coverage, mass.drug.admin.days, mass.drug.admin.coverage,   
                      mass.spraying.days, mass.spraying.coverage, mass.itn.dist.days, 
                      mass.itn.dist.coverage)
	
	
	
	# Lambda
	
	    							
	print(paste('number of patches:', num.patches))
	print(paste('number of houses:', num.houses))
	print(paste('number of humans:', num.humans))
	print(paste('number of ponds:', num.ponds))
}

rgamma.alpha = function(n, alpha) {

  rgamma(n, shape = 1/alpha, scale = alpha) 
}



micro.config.file = function(config.file.name="micro.in", num.days=100, num.runs=1, num.days.per.output=1, patches.file.name="default.patches",
                      houses.file.name="default.houses", humans.file.name="default.humans", lambda.file.name="default.lambda",
                      ponds.file.name="default.ponds", infections.file.name="default.infections", mozzy.life.span = 10, mozzy.sporogony.days=10, 
                      mozzy.rest.days=1, mozzy.prob.rest.indoor=0.9, mozzy.prob.alive.rest=0.9, mozzy.oviposit.days=1,
                      mozzy.prob.alive.oviposit=0.9, mozzy.search.algo=2, mozzy.num.search.houses=50, mozzy.num.search.ponds=4,
                      patch.wise.initial.infection=0, initial.pop.infected=0.5, incubation.days=10, latency.days=14, 
                      mean.infection.days=200, prob.travel.abroad=-1, clinic.test.tp=0.8, clinic.test.tn=0.8, 
                      clinic.drug.accuracy=1, acd.resolution=2, acd.trigger.prob=0.5, acd.radius=10, acd.intervention=1,
                      acd.capacity=50, drug.prophylaxis=30, drug.days.to.clear.inf=12, itn.repelling.effect=0.3, irs.repelling.effect=0.5, itn.decay.rate=0.005, 
                      irs.decay.rate=0.001, mass.vaccination.days=c(100,2000), mass.vaccination.coverage=0.9, 
                      pe.uptake.coverage=0.9, tb.uptake.coverage=0.9, mass.drug.admin.days=c(150,230), mass.drug.admin.coverage=0.9,   
                      mass.spraying.days=c(230,400,6000), mass.spraying.coverage=0.9, mass.itn.dist.days=c(132,255), 
                      mass.itn.dist.coverage=0.9) {

	fileConn<-file(config.file.name)

	writeLines( c(
		"################################################################################",
		"#               Configuration file for MicroSim                                #",
		"################################################################################",
		"",
		"",
		"##### Simulation time related parameters",
		"",
		"# Number of runs of simulation",
		paste("NUM_RUNS = ",num.runs),
		"",
		"# Total number of simulation days",
		paste("NUM_DAYS_OF_SIMULATION = ",num.days),
		"",
		"# Time period for the output frequency",
		paste("NUM_DAYS_PER_OUTPUT = ",num.days.per.output),
		"",
		"",
		"##### Simulation input files",
		"",
		"# File containing information about the patches",
		paste("PATCHES_FILE = ",patches.file.name),
		"",
		"# File containing information about the houses",
		paste("HOUSES_FILE = ",houses.file.name),
		"",
		"# File containing information about the humans",
		paste("HUMANS_FILE = ",humans.file.name),
		"",
		"# File containing information about the seasonal lambda",
		paste("LAMBDA_FILE = ",lambda.file.name),
		"",
		"# File containing information about the ponds",
		paste("PONDS_FILE = ",ponds.file.name),
		"",
		"# File containing information about initial population infected. Considered only when PATCH_WISE_INITIAL_POPULATION_INFECTED = 1. ",
		"# Non listed patches in this file are considered to have zero percent initial infected population. ",
		paste("INITIAL_HUMAN_INFECTION_FILE = ",infections.file.name),
		"",
		"",
		"##### Mosquito behavior",
		"",
		"# Mosquito life span",
		paste("MOZZY_LIFE_SPAN_DAYS = ",mozzy.life.span),
		"",
		"# Mosquito sporogony",
		paste("MOZZY_SPOROGONY_DAYS = ",mozzy.sporogony.days),
		"",
		"## Resting",
		"# Number of resting days",
		paste("MOZZY_NUM_REST_DAYS = ",mozzy.rest.days),
		"",
		"# Probability of resting indoors",
		paste("MOZZY_PROB_REST_INDOORS = ",mozzy.prob.rest.indoor),
		"",
		"# Probability of staying alive during resting",
		paste("MOZZY_PROB_STAYING_ALIVE_RESTING = ",mozzy.prob.alive.rest),
		"",
		"## Ovipositing",
		"# Number of ovipositing days",
		paste("MOZZY_NUM_OVIPOSIT_DAYS = ",mozzy.oviposit.days),
		"",
		"# Probability of staying alive during ovipositing",
		paste("MOZZY_PROB_STAYING_ALIVE_OVIPOSITING = ",mozzy.prob.alive.rest),	
		"",
		"# Mosquito location search algorithm: Simple nearest neighbor based search = 2, Wind direction based search = 1",
		paste("MOZZY_SEARCH_ALGO = ",mozzy.search.algo),
		"",
		"## Nearest neighbor based search. Considered only when MOZZY_SEARCH_ALGO = 2.",
		"# Number of nearest houses to look",
		paste("MOZZY_NUM_SEARCH_HOUSES = ",mozzy.num.search.houses),
		"",
		"# Number of nearest ponds to look",
		paste("MOZZY_NUM_SEARCH_PONDS = ",mozzy.num.search.ponds),
		"",
		"",
		"##### Human infection related parameters",
		"",
		"# Set to 1 if proportion of infected population is provided in separate file (INITIAL_HUMAN_INFECTION_FILE)",
		paste("PATCH_WISE_INITIAL_POPULATION_INFECTED = ",patch.wise.initial.infection),
		"",
		"# Proportion of human population initially infected. Considered only when PATCH_WISE_INITIAL_POPULATION_INFECTED = 0",
		paste("PROPORTION_INITIAL_POPULATION_INFECTED = ",initial.pop.infected),
		"",
		"# Exact number of days it takes for human to develop fever after getting infected",
		paste("NUM_INCUBATION_DAYS = ",incubation.days),
		"",
		"# Exact number of days it takes for human to become infectious after getting infected",
		paste("NUM_LATENCY_DAYS = ",latency.days),
		"",
		"# Mean number of days for infection if stayed untreated (actual number randomly derived from exponential distribution around this mean)",
		paste("MEAN_UNTREATED_INFECTION_DAYS = ",mean.infection.days),
		"",
		"# Mean number of days for of fever before the human seeks treatment (actual number randomly derived from exponential distribution around this mean)",
		"#MEAN_FEVER_DAYS_BEFORE_SEEKING_TREATMENT",
		"",
		"# Probability of travel abroad. (Set to a negative value to turn off the feature)",
		paste("HUMAN_PROB_TRAVEL_ABROAD = ",prob.travel.abroad),
		"",
		"",
 		"##### Clinic and Active case detection (ACD)",
 		"",
 		"# Clinic test sensibility (true-positives)",
 		paste("CLINIC_TEST_TP = ",clinic.test.tp),
 		"",
		"# Clinic test sensibility (true-negatives)",
		paste("CLINIC_TEST_TN = ",clinic.test.tn),
		"",
		"# Clinic drug assignment accuracy",
		paste("CLINIC_DRUG_ACCURACY = ",clinic.drug.accuracy),
		"",
		"# Gives the resolution of case detections: Patch wise = 1, In given radius around the house of the case being investigated = 2 ",
		paste("ACD_RESOLUTION = ",acd.resolution),
		"",
		"# Probability of a malaria case triggering investigation",
		paste("ACD_PROB_TRIGGER = ",acd.trigger.prob),
		"",
		"# Radius of ACD (Applicable only with ACD_RESOLUTION = 2)",
		paste("ACD_RADIUS = ",acd.radius),
		"",
		"# Intervention triggered due to investigation: MSAT (Mass-screen and test) = 1, MDA (mass drug administration) =2",
		paste("ACD_INTERVENTION = ",acd.intervention),
		"",
		"# Capacity of the clinics to perform ACD (No. of houses visited everyday)",
		paste("ACD_CAPACITY = ",acd.capacity),
		"",
		"",
		"##### Drug effects related parameters",
		"",
		"# Gives the number of days the drug stops incidence of new infections",
		paste("NUM_DAYS_OF_PROPHYLAXIS = ",drug.prophylaxis),
		"",
		"# Gives the number of days the drug takes to clear the existing infection",
		paste("NUM_DAYS_TO_CLEAR_INFECTION = ",drug.days.to.clear.inf),
		"",
		"",
		"##### Vaccinations",
		"",
		"# Probability of success for pre-erythrocytic vaccine",
		"# ask number of days before getting into effect",
		"#PROB_SUCCESS_PREERYTHROCYTIC = 0.9",
		"",
		"# Probability of success for transblocking vaccine",
		"#PROB_SUCCESS_TRANSBLOCKING = 0.9",
		"",
		"##### ITN effects related parameters",
		"",
		"# Number of days ITN stays effective",
		"#ITN_EFFICIENCY_DURATION",
		"",
		"# Fraction of mosquitoes dying due to ITN (when it is new)",
		"#ITN_MAX_MORTALITY",
		"",
		"# Fraction of mosquitoes dying due to ITN (when it is past its duration)",
		"#ITN_MIN_MORTALITY",
		"",
		"# Fraction of mosquitoes stopped from feeding due to ITN (when it is new)",
		"#ITN_MAX_REPELLING",
		"",
		"# Fraction of mosquitoes stopped from feeding due to ITN (when it is past its duration)",
		"#ITN_MIN_REPELLING",
		"",
		"",
		"##### IRS effects related parameters",
		"",
		"# Number of days IRS stays effective",
		"#IRS_EFFICIENCY_DURATION",
		"",
		"# Fraction of mosquitoes dying due to IRS (when it is new)",
		"#IRS_MAX_MORTALITY",
		"",
		"# Fraction of mosquitoes dying due to IRS (when it is past its duration)",
		"#IRS_MIN_MORTALITY",
		"",
		"",
		"##### Interventions",
		"## Intervention format",
		"## INTERVENTION_NAME DAY NUM_PATCHES <PATCHID, COVERAGE>",
		"##",
		"## NUM_PATCHES: Number of patches to conduct the intervention",
		"## PATCHES: homepatches--------------",
		"## Type of interventions possible",
		"## MASS_DRUG_ADMINISTRATION",
		"## MASS_VACCINATION_PE",
		"## MASS_VACCINATION_TB",
		"## MASS_DISTRIBUTE_NETS",
		"## MASS_SPRAYING",
		"## ",
		"## E.g. MASS_SPRAYING 108 3 2 0.9 10 0.6 47 0.72",
		"## ",
		"##",
		"",
		"",
		"# These parameters define the defaults ",
		"# check nSearch* shouldn't be more than the nHouses and nPonds ///////fix inside code",
		"",
		"",
		"",
		"# House",
		paste("itnRepellingEffect =",itn.repelling.effect),
		paste("irsRepellingEffect =",irs.repelling.effect),
		paste("irsDecayRate =",irs.decay.rate),
		paste("itnDecayRate =",itn.decay.rate),
		"",
		"# Intervene",
		paste("nMassVaccinations =",length(mass.vaccination.days)),
		paste("daysMassVaccination =",paste(mass.vaccination.days, collapse=", ")),
		paste("vaccinationCoverage =",mass.vaccination.coverage), 
		paste("peUptake =",pe.uptake.coverage),
		paste("tbUptake =",tb.uptake.coverage),
		"",
		paste("nMassDrugAdministration =",length(mass.drug.admin.days)),
		paste("daysMassDrugAdministration =",paste(mass.drug.admin.days, collapse=", ")),
		paste("mdaCoverage =",mass.drug.admin.coverage),
		"",
		paste("nSprayHouses =",length(mass.spraying.days)),
		paste("daysSprayHouses =",paste(mass.spraying.days, collapse=", ")),
		paste("irsCoverage =",mass.spraying.coverage),
		"",
		paste("nDistributeNets =",length(mass.itn.dist.days)),
		paste("daysDistributeNets =",paste(mass.itn.dist.days, collapse=", ")),
		paste("itnCoverage =",mass.itn.dist.coverage),
 		""
	), fileConn)
			
	close(fileConn)
          
}      