########################################
# Bioko Island Data, used for setting up a MACRO simulation
#
# The simulation will treat all clusters as isolated from one another, with travel to and from
#  different reservoir regions.  We make this choice in order to respect the lack of specific
#  destination information in the BI survey data.
#
# Need to output:
#   1. Vector of human populations in each cluster - cluster.human.pops
#   2. Vector of PfPR in each cluster and reservoir region
#   2. Travel matrix Pij, between each cluster and each reservoir region - P.ij = proportion of time spent in patch j if one's home is in i
#   3. Frequency of travel - travel.freq
#   4. Probability of fever and receiving treatment - fever.pf, treat.pf
#   4. EIR for all areas and regions - area.EIR
#   5. Lambda for all areas - area.lambda
########################################

library(data.table)
library(pscl, lib.loc = "/homes/georgoff/Rlibs/")
library(boot)
library(MASS)

########################################
# Step 1: Read in all data
########################################
# This is the geographic data listing locations/lat-lon, population at each of the areas
geo <- data.table(read.csv("/homes/georgoff/MASH-Main/MASH-dev/AlecGeorgoff/Bioko/Bioko_Island_Cluster_Simulations/BI_sim_setup_data/bioko_areas.csv"))
# PfPR estimates, from Su's model
pfprsu <- data.table(read.csv("/homes/georgoff/MASH-Main/MASH-dev/AlecGeorgoff/Bioko/Bioko_Island_Cluster_Simulations/BI_sim_setup_data/data_area.csv"))
# All survey data, includes aggregated travel information, from Carlos
travel <- data.table(read.csv("/homes/georgoff/MASH-Main/MASH-dev/AlecGeorgoff/Bioko/Bioko_Island_Cluster_Simulations/BI_sim_setup_data/summaries.csv"))
# Travel times between all areas, from Carlos
times <- data.table(read.csv("/homes/georgoff/MASH-Main/MASH-dev/AlecGeorgoff/Bioko/Bioko_Island_Cluster_Simulations/BI_sim_setup_data/travel_times.csv"))
# rename some of the columns, and reorder
times <- times[, 5:199]
colnames(times)[2:195] <- sapply(colnames(times)[2:195], FUN = function(s){
  return(substring(s, 2, 5))}
)
setcolorder(times, c(1,order(as.integer(colnames(times)[2:195]))+1))
times <- times[order(areaId)]

########################################
# Step 2: Merge and combine data
########################################
# Combine travel with pfprsu based on areaId
travel <- merge(travel, pfprsu[,.(areaId, pfpr)], by = "areaId")
# Combine geographic data with travel based on areaId
travel<- merge(travel, geo[,c("areaId", "pop", "lon", "lat")], by = "areaId")

# Here's the subset that I'll be dealing with for the travel model
travel.model.data <- travel[, .(areaId, lon, lat, X, Y, # areaID and location
                                ad2, malabo, # A2 region and Malabo
                                pop, pfpr, # Population, PfPR
                                clusId, # Cluster ID
                                n, pf, pffv, pftto, # Number surveyed, number surveyed with Pf+, number surveyed who reported fever, number surveyed who got treatment
                                to, ti_ban, ti_mal, ti_lub, ti_ria, ti_mok, ti_ure # amount traveled
)]


# Calculate the average travel times from each area to each region: Baney, Luba, Malabo, Moka, Riaba, Ureka
all = travel.model.data[, .(areaId, ad2, pop)]
all.times = merge(all, times, by = "areaId", all = FALSE)
admin2.names = levels(unique(travel.model.data$ad2))
tt.weighted.names = c("tt.weighted.ban", "tt.weighted.lub", "tt.weighted.mal",
                      "tt.weighted.mok", "tt.weighted.ria", "tt.weighted.ure")
tt.sum.names = c("tt.sum.ban", "tt.sum.lub", "tt.sum.mal",
                 "tt.sum.mok", "tt.sum.ria", "tt.sum.ure")
# Loop over target regions
for(i in c(1:length(admin2.names))){
  reg.times <- all.times[ad2==admin2.names[[i]]][, !c("areaId", "ad2", "pop" )]
  # The weighted average of travel times to the malabo area from each patch
  reg.times1 <- data.table(all.times[ad2 == admin2.names[[i]] ]$pop %*% as.matrix(reg.times, ncol = ncol(reg.times), nrow = nrow(all.times[ad2 == admin2.names[[i]] ]))/sum(all.times[ad2 == admin2.names[[i]] ]$pop))
  reg.times1 <- melt(reg.times1, measure.vars = colnames(reg.times1), variable.name = "areaId", value.name = tt.weighted.names[[i]])
  reg.times1$areaId <- as.integer(levels(reg.times1$areaId))
  # The unweighted average of travel times to the malabo area from each patch
  reg.times2 <- data.table(matrix(colSums(as.matrix(reg.times, ncol = ncol(mal.times), nrow = nrow(all.times[ad2 == admin2.names[[i]] ])))/nrow(all.times[ad2 == admin2.names[[i]] ]), nrow = 1))
  reg.times2 <- melt(reg.times2, measure.vars = colnames(reg.times2), variable.name = "areaId", value.name = tt.sum.names[[i]] )
  reg.times2$areaId = reg.times1$areaId
  # Add to old data set:
  travel.model.data <- merge(travel.model.data, reg.times1, by = "areaId", all = FALSE)
  travel.model.data <- merge(travel.model.data, reg.times2, by = "areaId", all = FALSE)
}
# Traveling to the mainland
# Use: time to travel to mainland = time to travel to malabo + 45*3 minutes, where 45 minutes is the time of the flight
travel.model.data$tt.weighted.o = travel.model.data$tt.weighted.mal + 135
travel.model.data$tt.sum.o = travel.model.data$tt.sum.mal + 135

# Melt data into a more travel-model friendly set of columns, where each row represents a pair interaction
reg.names = c("to", "ti_ban", "ti_lub", "ti_mal", "ti_mok", "ti_ria", "ti_ure")
tt.s.names = c("tt.sum.o", tt.sum.names)
tt.w.names = c("tt.weighted.o",tt.weighted.names)
travel.model.data <- melt(travel.model.data,
                          id.vars = setdiff(colnames(travel.model.data), union(union(tt.w.names, tt.s.names), reg.names)),
                          measure.vars = list(reg.names, tt.w.names, tt.s.names),
                          variable.name = "Region", value.name = c("Freq", "TravelTime.Weighted", "TravelTime.Sum"),
                          value.factor = TRUE)
# Set NA travel frequencies to 0:
travel.model.data[is.na(Freq)]$Freq <- 0
# Write in the population of each target region
reg.pops = c(552408, # off-island population, from CIA world factbook for the whole of EG minus BI total population
             sum(travel.model.data[ad2 == "Baney"]$pop)/7,
             sum(travel.model.data[ad2 == "Luba"]$pop)/7,
             sum(travel.model.data[ad2 == "Malabo"]$pop)/7,
             sum(travel.model.data[ad2 == "Moka"]$pop)/7,
             sum(travel.model.data[ad2 == "Riaba"]$pop)/7,
             sum(travel.model.data[ad2 == "Ureka"]$pop)/7)
travel.model.data$pop.Region = 0
travel.model.data$pop.Region <- sapply(travel.model.data$Region, FUN = function(s){return(reg.pops[[s]])})
# Rename the "Regions," mapping from numbers to location names
reg.names = c("off", "baney", "luba", "malabo", "moka", "riaba", "ureka")
travel.model.data$TravelRegion <- sapply(travel.model.data$Region, FUN = function(s){return(reg.names[[s]])})

########################################
# Step 3: Fit travel model
# Model: gravity model, with log-transformed populations and travel times
# Method: zero-inflated negative binomial regression
########################################
travel.model.data$pop.log = log(travel.model.data$pop)
travel.model.data$pop.Region.log = log(travel.model.data$pop.Region)
gravity.zinb.log.s <- zeroinfl(Freq ~ pop.log + pop.Region.log + TravelTime.Sum, data = travel.model.data, dist = "negbin")
gravity.zinb.log.w <- zeroinfl(Freq ~ pop.log + pop.Region.log + TravelTime.Weighted, data = travel.model.data, dist = "negbin")
travel.model.data$predict.zinb.log.s <- predict(gravity.zinb.log.s, data = travel.model.data, type = "response")
travel.model.data$predict.zinb.log.w <- predict(gravity.zinb.log.w, data = travel.model.data, type = "response")
#c(mean(travel.model.data$Freq), var(travel.model.data$Freq))
#c(mean(travel.model.data$predict.zinb.log.s), var(travel.model.data$predict.zinb.log.s))
#c(mean(travel.model.data$predict.zinb.log.s), var(travel.model.data$predict.zinb.log.s))


########################################
# Step 4: Calculate the probability of fever and seeking treatment
########################################
# only if there are pfpr positive people do we count the fevers
h <- travel.model.data[pf > 0][, c("pf", "pffv", "pftto")]
fever.pf = sum(h$pffv)/sum(h$pf) # doing this the naive way is .1116
# binom.test(x = c(sum(h$pffv), sum(h$pf - h$pffv)))
treat.pf = sum(h[pffv >0]$pftto)/sum(h[pffv >0]$pffv) # doing this the naive way is 0.6025
# binom.test(x = c(sum(h[pffv >0]$pftto),sum(h[pffv >0]$pffv) - sum(h[pffv >0]$pftto) )) # 0.6025641

########################################
# Step 5: Construct inputs for simulation
########################################
# Frequency of travel - average number of trips per day
# Calculated using Carlos's survey data, fit to a poisson distribution
travel.freq = 0.00217742

# Construct the vector of human populations for each cluster
cluster.human.pops = rep(0, 41+7)
# Construct the vector of PfPR for each cluster, and for each region
x.pfpr.input = rep(0, 41 + 7) # 41 clusters, 7 regions
# And off-island? From the Ncogo et al. paper about Bata district, let's just estimate that PfPR = .5
x.pfpr.input[42] <- 0.5
# Baney
x.pfpr.input[43] <- (travel.model.data[ad2 == "Baney"]$pop %*% travel.model.data[ad2 == "Baney"]$pfpr / sum(travel.model.data[ad2 == "Baney"]$pop))[[1]]
# Luba
x.pfpr.input[44] <- (travel.model.data[ad2 == "Luba"]$pop %*% travel.model.data[ad2 == "Luba"]$pfpr / sum(travel.model.data[ad2 == "Luba"]$pop))[[1]]
# Malabo
x.pfpr.input[45] <- (travel.model.data[ad2 == "Malabo"]$pop %*% travel.model.data[ad2 == "Malabo"]$pfpr / sum(travel.model.data[ad2 == "Malabo"]$pop))[[1]]
# Moka
x.pfpr.input[46] <- (travel.model.data[ad2 == "Moka"]$pop %*% travel.model.data[ad2 == "Moka"]$pfpr / sum(travel.model.data[ad2 == "Moka"]$pop))[[1]]
# Riaba
x.pfpr.input[47] <- (travel.model.data[ad2 == "Riaba"]$pop %*% travel.model.data[ad2 == "Riaba"]$pfpr / sum(travel.model.data[ad2 == "Riaba"]$pop))[[1]]
# Ureka
x.pfpr.input[48] <- (travel.model.data[ad2 == "Ureka"]$pop %*% travel.model.data[ad2 == "Ureka"]$pfpr / sum(travel.model.data[ad2 == "Ureka"]$pop))[[1]]
# Construct the matrix for Pij (most entries are 0)
P.ij <- diag(1, nrow = 41+7, ncol = 41+7)
# Loop over all *clusters*
for (i in 1:41){
  # This is the weighted mean of PR for the cluster
  pop.aggregated <- sum(travel.model.data[Region==1 & clusId==i]$pop)[[1]]
  cluster.human.pops[i] <- pop.aggregated
  pfpr.aggregated <- ((travel.model.data[Region==1 & clusId==i]$pop %*% travel.model.data[Region==1 & clusId==i]$pfpr)/pop.aggregated)[[1]]
  x.pfpr.input[i] <- pfpr.aggregated
  # This is the aggregated travel for the cluster, but only to and from the
  trav.aggregated <- travel.model.data[clusId==i][, sum(predict.zinb.log.s), by=Region][order(Region)]$V1
  # Constructing the row of the travel matrix for area 2132
  P.ij[i, 42:48] <- travel.freq*trav.aggregated/sum(trav.aggregated)
  P.ij[i, i] <- 1-travel.freq
}


########################################
# Step 6: Calibrate simulation model
########################################
# Assemble the parameters!
a = 0.3*0.9
b = 0.55
c = 0.15
rho = fever.pf*treat.pf
r = 1./200 # rate at which people become cured
eta = 1./30 # rate at which prophylaxis wears off
p = 0.9 # fraction of surviving mosquitoes
g = 1 - p # fraction of dying mosquitoes
peip = p^11 # fraction of mosquitoes who survive incubation period

d.kappa <- diag(b*c*x.pfpr.input/(1 + a*c*x.pfpr.input/g))
A <- P.ij %*% d.kappa
g.x.pfpr <- r/(1-rho)*x.pfpr.input/(1-(1+rho*r/eta/(1-rho))*x.pfpr.input)
VC <- ginv(A) %*% g.x.pfpr
m <- VC*g/a/a/peip
area.EIR <- (m*a*a*b*c*peip - (1-p)*r/(1-rho))/(p*a*b*c + (1 + rho/eta*r/(1-rho))*b*(1-p))
area.lambda <- (1-p)/p*m[1:41]*travel.model.data[Region==1 & !is.na(clusId)][, sum(pop), by=clusId][order(clusId)]$V1
