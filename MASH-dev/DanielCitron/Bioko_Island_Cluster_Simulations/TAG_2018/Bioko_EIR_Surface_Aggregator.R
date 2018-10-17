####
# Bioko Island EIR Surface calculator
# Requires aggregating a lot of scripts that I've already created
#
# October 12, 2018 - in preparation for ASTMH 2018
#
####

## Load libraries
#setwd("/Users/dtcitron/Documents/MASH/Bioko_Macro/TAG_2018/Bioko_EIR_Surfaces")
library(data.table, quietly = TRUE) # The workhorse of this exercise
library(pscl, quietly = TRUE) # Used for fitting the travel model
library(boot, quietly = TRUE)
library(MASS, quietly = TRUE)
library(nnet, quietly = TRUE)


### Source a lot of our data

### Read in and format the travel data
## Read in population denominator
denom <- data.table(read.csv("/Users/dtcitron/Documents/MASH/MASH-Main/MASH-dev/DanielCitron/Bioko_Island_Cluster_Simulations/BI_sim_setup_data/bioko_areas.csv"))
## PfPR Estimates - from Carlos's paper using Su's MAP model
BI <- as.data.table(read.csv("/Users/dtcitron/Documents/MASH/Bioko_Macro/Travel_Model_Fitting/Importations - Carlos Paper/eta.csv"))
# The old version:
#pfpr <- data.table(read.csv("/Users/dtcitron/Documents/MASH/MASH-Main/MASH-dev/DanielCitron/Bioko_Island_Cluster_Simulations/BI_sim_setup_data/data_area.csv"))
## Travel Survey Data
travel <- data.table(read.csv("/Users/dtcitron/Documents/MASH/MASH-Main/MASH-dev/DanielCitron/Bioko_Island_Cluster_Simulations/BI_sim_setup_data/summaries.csv"))
## Travel Times - proxy for distance between things
times <- data.table(read.csv("/Users/dtcitron/Documents/MASH/MASH-Main/MASH-dev/DanielCitron/Bioko_Island_Cluster_Simulations/BI_sim_setup_data/travel_times.csv"))
# rename some of the columns, and reorder
times <- times[, 5:199]
colnames(times)[2:195] <- sapply(colnames(times)[2:195], FUN = function(s){
  return(substring(s, 2, 5))}
)
setcolorder(times, c(1,order(as.integer(colnames(times)[2:195]))+1))
times <- times[order(areaId)]


## Merge and combine relevant data
# Combine travel with pfprsu based on areaId
travel.model.data <- merge(travel, BI[, c("areaId", "to_map", "prt_map", "prall_map", "te_map", "pre_map")], by = "areaId")
# old version
#travel <- merge(travel, pfpr[,.(areaId, pfpr)], by = "areaId")

# Combine geographic data with travel based on areaId
travel.model.data<- merge(travel.model.data, denom[,c("areaId", "pop", "lon", "lat")], by = "areaId")

# Here's the subset that I'll be dealing with for the travel model
travel.model.data <- travel.model.data[, .(areaId, lon, lat, X, Y, # areaID and location
                                # A2 region and whether or not the patch is near Malabo
                                ad2, malabo,
                                # Population, PfPR
                                pop, prall_map, pre_map, prt_map,
                                # Travel Frequency
                                te_map, to_map,
                                # Cluster ID
                                clusId,
                                # Number surveyed, number surveyed with Pf+
                                n, pf,
                                # Number surveyed who reported fever, got treatment
                                pffv, pftto,
                                # Number of times traveled to each destination, 2015-2017
                                to, ti_ban, ti_mal, ti_lub, ti_ria, ti_mok, ti_ure
)]
# Replace a LOT of NAs with other values:
travel.model.data[is.na(travel.model.data$n),]$n <- 0
travel.model.data[is.na(to),]$to <- 0
travel.model.data[is.na(ti_ban),]$ti_ban <- 0
travel.model.data[is.na(ti_lub),]$ti_lub <- 0
travel.model.data[is.na(ti_mal),]$ti_mal <- 0
travel.model.data[is.na(ti_mok),]$ti_mok <- 0
travel.model.data[is.na(ti_ria),]$ti_ria <- 0
travel.model.data[is.na(ti_ure),]$ti_ure <- 0

# Relabel all peri-urban (Punto Europa) areas in Malabo as "Peri"
travel.model.data[ad2 == "Malabo" & malabo == "non-Malabo"]$ad2 <- "Peri"
travel.model.data$ad2 <- as.factor(travel.model.data$ad2)

# Define a vector of AreaIDs
areaIds <- sort(unique(travel.model.data$areaId))


### Calculate Travel Times
all = travel.model.data[, .(areaId, ad2, pop)]
all.times = merge(all, times, by = "areaId", all = FALSE)
# Names of the target regions - a little awkward, because we don't have Peri as a resolved destination
admin2.names = levels(travel.model.data$ad2)
admin2.names = admin2.names[which(admin2.names != "Peri")]
# Names of target regions, "tt" = "time to"
tt.weighted.names = c("tt.weighted.ban", "tt.weighted.lub", "tt.weighted.mal",
                      "tt.weighted.mok", "tt.weighted.ria", "tt.weighted.ure")
tt.sum.names = c("tt.sum.ban", "tt.sum.lub", "tt.sum.mal",
                 "tt.sum.mok", "tt.sum.ria", "tt.sum.ure")
# Loop over target regions
for(i in c(1:length(admin2.names))){
  # Time to all patches in the target region
  reg.times <- all.times[ad2==admin2.names[[i]]][, !c("areaId", "ad2", "pop" )]
  # The weighted average of travel times to the malabo area from each patch
  reg.times1 <- data.table(
    all.times[ad2 == admin2.names[[i]] ]$pop %*% as.matrix(
      reg.times,
      ncol = ncol(reg.times),
      nrow = nrow(all.times[ad2 == admin2.names[[i]] ])
    )/sum(all.times[ad2 == admin2.names[[i]] ]$pop)
  )
  reg.times1 <- melt(reg.times1,
                     measure.vars = colnames(reg.times1),
                     variable.name = "areaId",
                     value.name = tt.weighted.names[[i]])
  reg.times1$areaId <- as.integer(levels(reg.times1$areaId))
  # The unweighted average of travel times to the malabo area from each patch
  reg.times2 <- data.table(
    matrix(
      colSums(as.matrix(
        reg.times,
        ncol = ncol(mal.times),
        nrow = nrow(all.times[ad2 == admin2.names[[i]] ]))
      )/nrow(all.times[ad2 == admin2.names[[i]] ]), nrow = 1)
  )
  reg.times2 <- melt(reg.times2,
                     measure.vars = colnames(reg.times2),
                     variable.name = "areaId",
                     value.name = tt.sum.names[[i]] )
  reg.times2$areaId = reg.times1$areaId
  # Add to data set
  # Average time to travel to each of the regions
  travel.model.data <- merge(travel.model.data, reg.times1, by = "areaId", all = FALSE)
  travel.model.data <- merge(travel.model.data, reg.times2, by = "areaId", all = FALSE)
}

## Add Off-Island Travel Times
# Use: time to travel to mainland = time to travel to malabo + 45*3 minutes
# where 45 minutes is the time of the flight
# Use: time to travel to mainland = time to travel to malabo + 8 hours
# where 8 hours is the time on the boat
# Using boat travel time does appear to give a better AIC for our modeling fits
travel.model.data$tt.weighted.o = travel.model.data$tt.weighted.mal + 8*60 #135
travel.model.data$tt.sum.o = travel.model.data$tt.sum.mal + 8*60 #135


## Travel Probabilities
# Add two columns to sum over the numbers of times traveled off island or on+off island:
travel.model.data$t.onoff.denom <- travel.model.data$to + travel.model.data$ti_ban + travel.model.data$ti_lub +
  travel.model.data$ti_mal + travel.model.data$ti_mok + travel.model.data$ti_ria + travel.model.data$ti_ure
travel.model.data$t.on.denom <- travel.model.data$ti_ban + travel.model.data$ti_lub + travel.model.data$ti_mal +
  travel.model.data$ti_mok + travel.model.data$ti_ria + travel.model.data$ti_ure
# Add two columns per destination to represent relative fractions of times traveled to each location:
travel.model.data$t.o.onoff <- travel.model.data$to/travel.model.data$t.onoff.denom
travel.model.data$t.ban.onoff <- travel.model.data$ti_ban/travel.model.data$t.onoff.denom
travel.model.data$t.lub.onoff <- travel.model.data$ti_lub/travel.model.data$t.onoff.denom
travel.model.data$t.mal.onoff <- travel.model.data$ti_mal/travel.model.data$t.onoff.denom
travel.model.data$t.mok.onoff <- travel.model.data$ti_mok/travel.model.data$t.onoff.denom
travel.model.data$t.ria.onoff <- travel.model.data$ti_ria/travel.model.data$t.onoff.denom
travel.model.data$t.ure.onoff <- travel.model.data$ti_ure/travel.model.data$t.onoff.denom
travel.model.data$t.ban.on <- travel.model.data$ti_ban/travel.model.data$t.on.denom
travel.model.data$t.lub.on <- travel.model.data$ti_lub/travel.model.data$t.on.denom
travel.model.data$t.mal.on <- travel.model.data$ti_mal/travel.model.data$t.on.denom
travel.model.data$t.mok.on <- travel.model.data$ti_mok/travel.model.data$t.on.denom
travel.model.data$t.ria.on <- travel.model.data$ti_ria/travel.model.data$t.on.denom
travel.model.data$t.ure.on <- travel.model.data$ti_ure/travel.model.data$t.on.denom


## Travel Frequency
# The simplest possible model - probability that each person travels over the 8 week period
# Don't divide by the time scale quite yet
travel.model.data$travel.prob = travel.model.data$t.onoff.denom/travel.model.data$n
travel.model.data[is.na(travel.prob)]$travel.prob = 0
travel.model.data$travel.freq = travel.model.data$travel.prob/56

## Add A2 unit as a variable
travel.model.data$ad2 <- as.factor(travel.model.data$ad2)
travel.model.data <- merge(travel.model.data,
                           # This is a little like a "cast" matrix of indicator variables
                           as.data.table(model.matrix( ~ areaId + ad2 - 1, data = travel.model.data)),
                           by = "areaId")


## Construct PfPR vector
pfpr.input = rep(0, 194 + 7)
# Island patches
for (i in 1:194){
  pfpr.input[i] <- travel.model.data[areaId == areaIds[i]]$prall_map
}
# "Off-island"
pfpr.input[194 + 1] <- .60 # citing ncogo et al for this
# Baney
pfpr.input[194 + 2] <- (travel.model.data[ad2 == "Baney"]$pop %*%
                          travel.model.data[ad2 == "Baney"]$prall_map /
                          sum(travel.model.data[ad2 == "Baney"]$pop))[[1]]
# Luba
pfpr.input[194 + 3] <- (travel.model.data[ad2 == "Luba"]$pop %*%
                          travel.model.data[ad2 == "Luba"]$prall_map /
                          sum(travel.model.data[ad2 == "Luba"]$pop))[[1]]
# Malabo
pfpr.input[194 + 4] <- (travel.model.data[ad2 == "Malabo"]$pop %*%
                          travel.model.data[ad2 == "Malabo"]$prall_map /
                          sum(travel.model.data[ad2 == "Malabo"]$pop))[[1]]
# Moka
pfpr.input[194 + 5] <- (travel.model.data[ad2 == "Moka"]$pop %*%
                          travel.model.data[ad2 == "Moka"]$prall_map /
                          sum(travel.model.data[ad2 == "Moka"]$pop))[[1]]
# Riaba
pfpr.input[194 + 6] <- (travel.model.data[ad2 == "Riaba"]$pop %*%
                          travel.model.data[ad2 == "Riaba"]$prall_map /
                          sum(travel.model.data[ad2 == "Riaba"]$pop))[[1]]
# Ureka
pfpr.input[194 + 7] <- (travel.model.data[ad2 == "Ureka"]$pop %*%
                          travel.model.data[ad2 == "Ureka"]$prall_map /
                          sum(travel.model.data[ad2 == "Ureka"]$pop))[[1]]


## Define Ross-MacDonald Model Parameters
a = 0.3*0.9
b = 0.55
c = 0.15
r = 1./200 # rate at which people become cured
eta = 1./30 # rate at which prophylaxis wears off
p = 0.9 # fraction of surviving mosquitoes
g = 1 - p # fraction of dying mosquitoes
peip = p^11 # fraction of mosquitoes who survive incubation period
## Treatment-seeking behavior parameters
h <- travel.model.data[pf > 0][, c("pf", "pffv", "pftto")]
fever.pf = sum(h$pffv)/sum(h$pf)
# binom.test(x = c(sum(h$pffv), sum(h$pf - h$pffv)))
treat.pf = sum(h[pffv >0]$pftto)/sum(h[pffv >0]$pffv)
# binom.test(x = c(sum(h[pffv >0]$pftto),
#            sum(h[pffv >0]$pffv) - sum(h[pffv >0]$pftto) )) # 0.6025641
rho = fever.pf*treat.pf


###
# SURFACE 1:
# TaR - BASED
# BINOMIAL MODEL OF TRAVEL, MULTINOMIAL MODEL OF DESTINATION SELECTION
# source: Bioko_TaR_Adjustment
###
# Fitting the probability of leaving the island
h <- glm( cbind(t.onoff.denom, n - t.onoff.denom) ~ pop + ad2 + tt.weighted.mal,
          data = travel.model.data,
          family = binomial(link = logit))
travel.model.data$prob.model.fit <- h$fitted.values # fit to travel.prob
travel.model.data$freq.model.fit <- h$fitted.values/56 # fit to travel.freq


# Fitting the choice of where to go
multinom.dat <- travel.model.data
multinom.dat$tt.mal <- multinom.dat$tt.weighted.mal
multinom.dat.v1 <- melt(multinom.dat,
                        id.vars = c("areaId", "ad2", "malabo", "pop", "tt.mal"),
                        measure.vars = list(c("to", "ti_ban", "ti_mal", "ti_lub", "ti_ria", "ti_mok", "ti_ure"),
                                            c("tt.weighted.ban", "tt.weighted.lub", "tt.weighted.mal", "tt.weighted.mok", "tt.weighted.ria", "tt.weighted.ure", "tt.weighted.o"),
                                            c("tt.sum.ban", "tt.sum.lub", "tt.sum.mal", "tt.sum.mok", "tt.sum.ria", "tt.sum.ure", "tt.sum.o")),
                        value.name = c("counts", "tt.weighted", "tt.sum"),
                        variable.name = "ix")
# and reset the levels values for the destinations
dummy <- data.table(ix = c(1:7), destination = c("to", "ti_ban", "ti_mal", "ti_lub", "ti_ria", "ti_mok", "ti_ure"))
multinom.dat.v1 <- merge(multinom.dat.v1, dummy, by = "ix")

# More on melting by groups of variables, rather than a single variable:
# https://stackoverflow.com/questions/44291855/melt-dataframe-multiple-columns-enhanced-new-functionality-from-data-tabl
multinom.dat.v2 <- multinom.dat.v1[counts > 0]
multinom.dat.v2 <- multinom.dat.v2[rep(seq(1, nrow(multinom.dat.v2)), multinom.dat.v2$counts)]

## This is the actual regression
my.multinom.model <- multinom(destination ~ ad2 + pop + tt.mal, data = multinom.dat.v2)

## Predictions for all areas, according to covariates
holder.f <- as.data.table(predict(my.multinom.model,
                                  newdata = multinom.dat.v1[order(areaId) & destination =="to"],
                                  type = "probs"))
colnames(holder.f)  <- c("p.ban", "p.lub", "p.mal", "p.mok", "p.ria", "p.ure", "p.off")
holder.f$areaId <- areaIds

travel.model.data <- merge(multinom.dat, holder.f, by = "areaId")

### Calculate TaR matrix
TaR.1 <- diag(1, nrow = 194+7, ncol = 194+7)
holder <- travel.model.data[, c("p.off", "p.ban", "p.lub", "p.mal", "p.mok", "p.ria", "p.ure")]*c(10,3,3,3,3,3,3)
for (i in 1:194){
  TaR.1[i, (194 + 1):(194 + 7)] <- as.numeric(holder[i,])/(sum(as.numeric(holder[i,])) + travel.model.data$freq.model.fit[i]^(-1))
  TaR.1[i,i] <- 1 - sum(as.numeric(holder[i,])/(sum(as.numeric(holder[i,])) + travel.model.data$freq.model.fit[i]^(-1)))
}

# and now calculate FOI
odds.vector <- r/(1-rho)*pfpr.input/(1-(1+rho*r/eta/(1-rho))*pfpr.input)
h.1 <- ginv(TaR.1) %*% odds.vector
travel.model.data$h.1 <- h.1[1:194]
#hist(h.1[1:194]/b*365, main = "Histogram of Annual EIR")

# What do I need to save here?
# h.1 the FOI vector - this will be used to calculate
# freq.model.fit gives the rate of leaving
# p.off etc, which gives the probability of choosing different destinations
# the vector of travel durations vectorized across destinations

# Plots, for reference
# hist(h.1[1:194]*365/.55)
# area.data = merge(areasf, travel.model.data, by.x = "id", by.y = "areaId", all=TRUE)
# plot.data<-area.data[order(area.data$order), ]
# p1 = ggplot(data = plot.data, aes(x=long, y=lat.x, group = group))
# p2 = p1 + geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill="grey", size = 0.25)
# map <- p2 + geom_polygon(data = plot.data, aes(x = long, y = lat.x, group = group, fill = h.1*365/.55), color = NA, size = 0.25) +
#  scale_fill_gradient(name="FOI (Annual EIR)", low="yellow", high="red", limits=c(0,2.4)) +
#  geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill=NA, size = 0.25) +
#  theme(axis.line=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
#        axis.title.x=element_blank(),
#        axis.title.y=element_blank(), panel.background=element_blank(), legend.position=c(0.2, 0.8))
# map


###
# SURFACE 2:
# TaR - BASED, WITH VARYING TIMES SPENT OFF-ISLAND
# BINOMIAL MODEL OF TRAVEL, MULTINOMIAL MODEL OF DESTINATION SELECTION
# source: Bioko_FOI_calculations
###

# First disambiguate travel times:
# Set up weighted travel
mat.weighted <- matrix(0, nrow = 7, ncol = 195)
# off-island travel
mat.weighted[1,195] <- 1
# baney
mat.weighted[2, which(areaIds %in% travel.model.data[ad2Baney==1]$areaId)] <- travel.model.data[ad2Baney == 1]$pop/sum(travel.model.data[ad2Baney == 1]$pop)
# luba
mat.weighted[3, which(areaIds %in% travel.model.data[ad2Luba==1]$areaId)] <- travel.model.data[ad2Luba == 1]$pop/sum(travel.model.data[ad2Luba == 1]$pop)
# malabo
mat.weighted[4, which(areaIds %in% travel.model.data[ad2Malabo==1]$areaId)] <- travel.model.data[ad2Malabo == 1]$pop/sum(travel.model.data[ad2Malabo == 1]$pop)
# moka
mat.weighted[5, which(areaIds %in% travel.model.data[ad2Moka==1]$areaId)] <- travel.model.data[ad2Moka == 1]$pop/sum(travel.model.data[ad2Moka == 1]$pop)
# riaba
mat.weighted[6, which(areaIds %in% travel.model.data[ad2Riaba==1]$areaId)] <- travel.model.data[ad2Riaba == 1]$pop/sum(travel.model.data[ad2Riaba == 1]$pop)
# ureka
mat.weighted[7, which(areaIds %in% travel.model.data[ad2Ureka==1]$areaId)] <- travel.model.data[ad2Ureka == 1]$pop/sum(travel.model.data[ad2Ureka == 1]$pop)

## Define functions for solving for trip time:
pr.pos = function(Travel.time, PR, h, r){
  return(PR*exp(-(h + r)*Travel.time) + h/(h+r)*(1 - exp(-(h + r)*Travel.time)))
}
# Probability of testing positive after returning, equivalent to PR_t from the geospatial model
po.pos = function(Travel.time, hloc, PR, h, r){
  pr.return = pr.pos(Travel.time, PR, h, r)
  return(hloc/(r + hloc)*(1-(1 - exp(-(hloc + r)*56))/(hloc + r)/56) +
           (1 - exp(-(hloc + r)*56))/(hloc + r)/56*pr.pos(Travel.time, PR, h, r))
}
# Functions that we optimize
x0.th <- c(5, .0001)
fn.th <- function(x){
  # x[1] = Travel time, unknown
  # x[2] = hloc, unknown
  (po.pos(x[1], x[2], pr, .02, 1/200.) - prt)^2 + (r*pr/(1-pr) - 0.02*fr.o.t*x[1] - x[2]*(1 - fr.o.t*x[1]))^2
}
hn.th <- function(x){
  # want the elements of this vector to be positive
  h <- numeric(2)
  h[1] <- x[1]
  h[2] <- x[2]
}

# solve, comparing prall and pre
to.time.e <- rep(0,(194+1))
hloc.e <- rep(0,(194+1))
for (area.ix in (1:194)){
  pr <-  travel.model.data$prall_map[[area.ix]]
  prt <- travel.model.data$pre_map[[area.ix]]
  fr.o.t <- (travel.model.data$freq.model.fit*travel.model.data$p.off)[area.ix] ### not sure about this...
  sol <- nloptr::slsqp(x0.th, fn.th, hin = hn.th)
  to.time.e[area.ix] <- sol$par[1]
  hloc.e[area.ix] <- sol$par[2]
}
#hist(to.time.e)
#hist(hloc.e[1:194]*365/.55)
travel.model.data$hloc.e <- hloc.e[1:194]
travel.model.data$to.time.e <- to.time.e[1:194]

# area.data = merge(areasf, travel.model.data, by.x = "id", by.y = "areaId", all=TRUE)
# plot.data<-area.data[order(area.data$order), ]
# p1 = ggplot(data = plot.data, aes(x=long, y=lat.x, group = group))
# p2 = p1 + geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill="grey", size = 0.25)
# ta.map <- p2 + geom_polygon(data = plot.data, aes(x = long, y = lat.x, group = group, fill = to.time.e), color = NA, size = 0.25) +
#  scale_fill_gradient(name="Time Off Island", low="yellow", high="red", limits=c(0, 50)) +
#  geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill=NA, size = 0.25) +
#  theme(axis.line=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
#        axis.title.x=element_blank(),
#        axis.title.y=element_blank(), panel.background=element_blank(), legend.position=c(0.2, 0.8))
# ta.map
# ta.map <- p2 + geom_polygon(data = plot.data, aes(x = long, y = lat.x, group = group, fill = hloc.e*365/.55), color = NA, size = 0.25) +
#  scale_fill_gradient(name="Local Residual Transmission", low="yellow", high="red", limits=c(-.01, 2.4)) +
#  geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill=NA, size = 0.25) +
#  theme(axis.line=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
#        axis.title.x=element_blank(),
#        axis.title.y=element_blank(), panel.background=element_blank(), legend.position=c(0.2, 0.8))
# ta.map

### And now for the tricky part:
# we want to leverage the times off island by plugging into the TaR matrix
# repeat the analysis from above, this time with the travel times off island in the first column of the travel time vector
to.time.e[which(to.time.e <= 1 )] <- 1
#hist(to.time.e[1:194])
TaR.2 <- diag(1, nrow = 194+7, ncol = 194+7)
for (i in 1:194){
  holder <- travel.model.data[i, c("p.off", "p.ban", "p.lub", "p.mal", "p.mok", "p.ria", "p.ure")]*c(to.time.e[i],3,3,3,3,3,3)
  TaR.2[i, (194 + 1):(194 + 7)] <- as.numeric(holder)/(sum(as.numeric(holder)) + freq.model.fit[i]^(-1))
  TaR.2[i,i] <- 1 - sum(as.numeric(holder)/(sum(as.numeric(holder)) + freq.model.fit[i]^(-1)))
}


# now that we have a TaR matrix we can try solving for local h a second time:
odds.vector <- r/(1-rho)*pfpr.input/(1-(1+rho*r/eta/(1-rho))*pfpr.input)
h.2 <- ginv(TaR.2) %*% odds.vector
travel.model.data$h.2 <- h.2[1:194]


# Plots, for reference
#hist(h.2[1:194]/b*365, main = "Histogram of Annual EIR")
#hist(h.2 - h.1) # looking at the difference
# area.data = merge(areasf, travel.model.data, by.x = "id", by.y = "areaId", all=TRUE)
# plot.data<-area.data[order(area.data$order), ]
# p1 = ggplot(data = plot.data, aes(x=long, y=lat.x, group = group))
# p2 = p1 + geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill="grey", size = 0.25)
# map <- p2 + geom_polygon(data = plot.data, aes(x = long, y = lat.x, group = group, fill = h.2*365/.55), color = NA, size = 0.25) +
#  scale_fill_gradient(name="FOI (Annual EIR)", low="yellow", high="red", limits=c(0,2.4)) +
#  geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill=NA, size = 0.25) +
#  theme(axis.line=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
#        axis.title.x=element_blank(),
#        axis.title.y=element_blank(), panel.background=element_blank(), legend.position=c(0.2, 0.8))
# map




###
# SURFACE 3:
# LOCAL ESTIMATE BASED, FROM THE FIRST BIOKO ISLAND PAPER MODELING
# source: Supplement_BI.Rmd
# UPDATE: not included!
###
## Define a whole lot of functions, from Dave
# Solve for PR, based on delta, eta, h
# Travel2PR = function(Xt, Xp, h=0, T=56, r=1/200, eta=NULL){
#   if(is.null(eta)) eta = Travel2eta(Xt, Xp, h, T, r)
#   delta = -log(1-Xt)/T
#   return((eta*delta + h)/(eta*delta + h + r))
# }
# # solve for h by optimizing
# Travel2h=function(Xt, Xp, PR, T=56, r=1/200, hmx = 3/365, eta=NULL){
#   geth = function(h){
#     abs(PR - Travel2PR(Xt, Xp, h, T, r, eta))^2#^(1/2)
#   }
#   optimize(geth, c(0,hmx))$min
# }
# # Once h is known, need to back solve for eta, this time using PR_L0 = h/(h + r)
# Travel2eta = function(Xt, Xp, h=0, T=56, r=1/200){
#   # Xt :: period prevalence for travel in the last 56 days
#   # Xp :: reported prevalence in travelers
#   LR = h/(h+r) # Local residual PR - equilibrium value when there are no importations
#   prt = function(t){exp(-r*t)}
#   return(T*pmax(Xp-LR,0)/integrate(prt, 0, T)$val) #eta
# }
# # Travel Fraction - need eta
# Travel2TF = function(Xt, Xp, PR, h=0, T=56, r=1/200, eta=NULL){
#   tpr = Travel2PR(Xt, Xp, 0, T, r, eta)
#   pmin(tpr/PR, 1)
# }
#
# ## Delta
# BI$delta.e <- 0
# BI$delta.e <- -log(1 - BI$te_map)/56
# # coestimate
# BI$h.co.e <- 0
# for (i in 1:194){
#   BI$h.co.e[[i]] <- Travel2h(BI$te_map[[i]], BI$pre_map[[i]], BI$prall_map[[i]])
# }
#
# travel.model.data <- merge(travel.model.data, BI[, c("areaId", "h.co.e")], by = "areaId")
# names(travel.model.data)[names(travel.model.data) == "h.co.e"] <- "h.3"
#summary(BI$h.co.e*365/.55)
#summary(travel.model.data$h.1*365/.55)
#summary(travel.model.data$h.2*365/.55)
#hist(travel.model.data$h.3*365/.55)

#area.data = merge(areasf, travel.model.data, by.x = "id", by.y = "areaId", all=TRUE)
#plot.data<-area.data[order(area.data$order), ]
#p1 = ggplot(data = plot.data, aes(x=long, y=lat.x, group = group))
#p2 = p1 + geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill="grey", size = 0.25)
#map <- p2 + geom_polygon(data = plot.data, aes(x = long, y = lat.x, group = group, fill = h.3*365/.55), color = NA, size = 0.25) +
#  scale_fill_gradient(name="FOI (Annual EIR)", low="yellow", high="red", limits=c(0,2.4)) +
#  geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill=NA, size = 0.25) +
#  theme(axis.line=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
#        axis.title.x=element_blank(),
#        axis.title.y=element_blank(), panel.background=element_blank(), legend.position=c(0.2, 0.8))
#map

### LAMBDA Calculation
# We do need the TaR matrices to do this
# Or at least, we should check to see how much adding the tar Matrices matters numerically

pop.inputs <- rep(0, 194 + 1)
pop.inputs[1:194] <- travel.model.data$pop # assume Bata's population is the same as Malabo's
pop.inputs[194+1] <- sum(travel.model.data[ad2=="Malabo" | ad2 == "Peri" ]$pop)
pop.inputs[194+2] <- sum(travel.model.data[ad2 == "Baney"]$pop)
pop.inputs[194+3] <- sum(travel.model.data[ad2=="Luba"]$pop)
pop.inputs[194+4] <- sum(travel.model.data[ad2=="Malabo" | ad2 == "Peri" ]$pop)
pop.inputs[194+5] <- sum(travel.model.data[ad2=="Moka"]$pop)
pop.inputs[194+6] <- sum(travel.model.data[ad2=="Riaba"]$pop)
pop.inputs[194+7] <- sum(travel.model.data[ad2=="Ureka"]$pop)


kappa.1 <- as.vector(pop.inputs*pfpr.input)/(as.vector(pop.inputs)) # sporozoite rate
#kappa.1 <- (TaR.1 %*% as.vector(pop.inputs*pfpr.input))/(TaR.1 %*% as.vector(pop.inputs)) # only works if there are visitors, but for this model there are not
z.1 <- a*c*kappa.1[1:194]/(a*c*kappa.1[1:194] + (1-p))*peip
#hist(a*c*pfpr.input[1:194]/(a*c*pfpr.input[1:194] + (1-p))*peip)
#hist(z.1)
h.1[which(h.1 < 0)] <- 0
travel.model.data$pop.home <- travel.model.data$pop*diag(TaR.1)[1:194]
lambda.1 <- (1-p)/p*h.1[1:194]*travel.model.data$pop.home/a/b/z.1[1:194] ### still haven't gotten this yet...
travel.model.data$lambda.1 <- lambda.1[1:194]
travel.model.data$m.1 <- with(travel.model.data, lambda.1*9/pop.home)
travel.model.data$z.1 <- z.1
# Map Mean Mosquito population density:
# area.data = merge(areasf, travel.model.data, by.x = "id", by.y = "areaId", all=TRUE)
# plot.data<-area.data[order(area.data$order), ]
# p1 = ggplot(data = plot.data, aes(x=long, y=lat.x, group = group))
# p2 = p1 + geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill="grey", size = 0.25)
# map <- p2 + geom_polygon(data = plot.data, aes(x = long, y = lat.x, group = group, fill = m.1), color = NA, size = 0.25) +
#  scale_fill_gradient(name="M/H", low="yellow", high="red", limits=c(0, .6)) +
#  geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill=NA, size = 0.25) +
#  theme(axis.line=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
#        axis.title.x=element_blank(),
#        axis.title.y=element_blank(), panel.background=element_blank(), legend.position=c(0.2, 0.8))
# map
# map <- p2 + geom_polygon(data = plot.data, aes(x = long, y = lat.x, group = group, fill = log10(lambda.1+1)), color = NA, size = 0.25) +
#   scale_fill_gradient(name="Log(Lambda)", low="yellow", high="red", limits=c(0, 3)) +
#   geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill=NA, size = 0.25) +
#   theme(axis.line=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(), panel.background=element_blank(), legend.position=c(0.2, 0.8))
# map

kappa.2 <- as.vector(pop.inputs*pfpr.input)/(as.vector(pop.inputs)) # sporozoite rate
#kappa.2 <- (TaR.2 %*% as.vector(pop.inputs*pfpr.input))/(TaR.2 %*% as.vector(pop.inputs)) # sporozoite rate, if there are visitors
z.2 <- a*c*kappa.2[1:194]/(a*c*kappa.2[1:194] + (1-p))*peip
#hist(a*c*pfpr.input[1:194]/(a*c*pfpr.input[1:194] + (1-p))*peip)
#hist(z.2)
h.2[which(h.2 < 0)] <- 0
travel.model.data$pop.home <- travel.model.data$pop*diag(TaR.2)[1:194] # fraction of people who stay at home
lambda.2 <- (1-p)/p*h.2[1:194]*travel.model.data$pop.home/a/b/z.2[1:194]
#hist(lambda.2)
travel.model.data$lambda.2 <- lambda.2[1:194]
travel.model.data$m.2 <- with(travel.model.data, lambda.2*9/pop.home)
travel.model.data$z.2 <- z.2
# Map Mean Mosquito population density:
# area.data = merge(areasf, travel.model.data, by.x = "id", by.y = "areaId", all=TRUE)
# plot.data<-area.data[order(area.data$order), ]
# p1 = ggplot(data = plot.data, aes(x=long, y=lat.x, group = group))
# p2 = p1 + geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill="grey", size = 0.25)
# map <- p2 + geom_polygon(data = plot.data, aes(x = long, y = lat.x, group = group, fill = m.2), color = NA, size = 0.25) +
#  scale_fill_gradient(name="M/H", low="yellow", high="red", limits=c(0, .6)) +
#  geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill=NA, size = 0.25) +
#  theme(axis.line=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
#        axis.title.x=element_blank(),
#        axis.title.y=element_blank(), panel.background=element_blank(), legend.position=c(0.2, 0.8))
# map
# map <- p2 + geom_polygon(data = plot.data, aes(x = long, y = lat.x, group = group, fill = log10(lambda.2+1)), color = NA, size = 0.25) +
#   scale_fill_gradient(name="Log(Lambda)", low="yellow", high="red", limits=c(0, 3)) +
#   geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill=NA, size = 0.25) +
#   theme(axis.line=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(), panel.background=element_blank(), legend.position=c(0.2, 0.8))
# map

# Sporozoite rate maps
# map <- p2 + geom_polygon(data = plot.data, aes(x = long, y = lat.x, group = group, fill = z.1), color = NA, size = 0.25) +
#   scale_fill_gradient(name="SPZ z*", low="yellow", high="red", limits=c(0, .05)) +
#   geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill=NA, size = 0.25) +
#   theme(axis.line=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(), panel.background=element_blank(), legend.position=c(0.2, 0.8))
# map
# map <- p2 + geom_polygon(data = plot.data, aes(x = long, y = lat.x, group = group, fill = z.2), color = NA, size = 0.25) +
#   scale_fill_gradient(name="SPZ z*", low="yellow", high="red", limits=c(0, .05)) +
#   geom_polygon(data = bioko, aes(x = long, y = lat, group = group), color = "black", fill=NA, size = 0.25) +
#   theme(axis.line=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(), panel.background=element_blank(), legend.position=c(0.2, 0.8))
# map



### Now we save everything
t <- travel.model.data[, c("areaId", "ad2", "malabo", "pop",
                      "prall_map", "pre_map", "prt_map", "te_map", "to_map", # MAPs maps
                      "prob.model.fit",  "freq.model.fit", # probability of leaving, frequency of leaving = prob/56
                      "p.off", "p.ban" ,"p.lub", "p.mal", "p.mok", "p.ria", "p.ure", # destination probability distribution
                      "h.1", "h.2", "hloc.e", "to.time.e", # the latter 2 go with h.2
                      "m.1", "lambda.1", "z.1", # using TaR with 10-day trips, these are emergence rates, mosquito pops, and SPZ rate
                      "m.2", "lambda.2", "z.2" # using TaR with variable trips, these are emergence rates, mosquito pops, and SPZ rate
                  )]
fwrite(t, file = "/Users/dtcitron/Documents/MASH/Bioko_Macro/TAG_2018/Bioko_EIR_Surfaces/EIR_surfaces.csv")
#t.check <- fread(file = "/Users/dtcitron/Documents/MASH/Bioko_Macro/TAG_2018/Bioko_EIR_Surfaces/EIR_surfaces.csv")

# Also need to save our TaR matrices:
write.csv(TaR.1, file = "/Users/dtcitron/Documents/MASH/Bioko_Macro/TAG_2018/Bioko_EIR_Surfaces/TAR1.csv")
write.csv(TaR.2, file = "/Users/dtcitron/Documents/MASH/Bioko_Macro/TAG_2018/Bioko_EIR_Surfaces/TAR2.csv")
#holder <- read.csv(file = "/Users/dtcitron/Documents/MASH/Bioko_Macro/TAG_2018/Bioko_EIR_Surfaces/TAR1.csv")
#holder <- as.matrix(holder[, 2:202])

# Save pfpr.input, h.1, h.2
fwrite(data.table(pfpr.input = pfpr.input,
                  pop.input = pop.inputs,
                  h.1 = h.1,
                  h.2 = h.2), file = "/Users/dtcitron/Documents/MASH/Bioko_Macro/TAG_2018/Bioko_EIR_Surfaces/pfpr_input.csv")
#holder <- fread(file = "/Users/dtcitron/Documents/MASH/Bioko_Macro/TAG_2018/Bioko_EIR_Surfaces/pfpr_input.csv")
