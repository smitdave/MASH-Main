library(sp)
library(rgdal)
library(data.table)
library(rootSolve)
library(xlsx)

# Read in sector-level data from Basupu
# This is just a trial data set, and can be extended to other similar sector-level data with lat/lon coordinates
basupuSectors = fread("datBasupu_sectors.csv")
# Extract latitude, longitude, population data by sector
cntr.lat = basupuSectors$Y
cntr.long = basupuSectors$X
pop = basupuSectors$pop
# Convert lat-long coordinates to UTM projection
# https://tools.wmflabs.org/geohack/geohack.php?pagename=Bioko&params=3_30_N_8_42_E_type:isle_region:GQ_dim:200000    
basupu_latlon_coord <- data.table(lat = cntr.lat, lon = cntr.long)
coordinates(basupu_latlon_coord) <- c("lon", "lat")
proj4string(basupu_latlon_coord) <- CRS("+proj=longlat")
reproj_UTM <- spTransform(basupu_latlon_coord, CRS("+proj=utm +zone=32N"))
cntr.Y <- reproj_UTM$lat
cntr.X <- reproj_UTM$lon

#####
# We will use a kernel function to count the approximate number of people included in a cluster
# centered at a particular location
kern = function(x, k=0.02, x50=250){
  # k controls the steepness of the slope
  # x50 controls the radial distance away from the center where the kernel strength hits 50%
  1-1/(1+exp(-k*(x-x50)))
}
# To count the number of people included in the kernel, we choose a sector to be the center of
# the kernel, and then perform a weighted sum over all of the people in nearby clusters

##### 
# Calculate distance between two sectors (using UTM projection)
# Enter: the x-y coordinates of the center location
#        the x-y coordinates of the other sector's location
krDistance <- function(x.center, y.center, x.other, y.other){
  dst = sqrt((x.center - x.other)^2 + (y.center - y.other)^2)
  return(dst)
}

##### 
# Calculate the number of people included in a kernel of a certain shape
# Enter: i - the index of the center location
#        qty - list of populations
#        y.coords - list of Y coordiantes, converted from latitudes
#        x.coords - list of X coordinates, converted from longitudes
#        k, x50 are parameters that define the shape of the kernel envelope
krSmooth = function(i, qty, y.coords, x.coords, k=0.02, x50=250, PAUSE = FALSE){
  if (PAUSE == TRUE) browser()
  dst = sqrt((y.coords[i] - y.coords)^2 + (x.coords[i] - x.coords)^2)
  #q = weighted.mean(qty, kern(dst,k,x50))
  # The weighted mean doesn't really make sense to me, since turning up the mean distance turns down the total number of people included
  q = sum(qty * kern(dst,k,x50)) 
  return(q)
}

#####
# METHODOLOGY
#
# Step 1: Pick the geographical center of a hypothetical cluster
# Step 2: Use kernel to determine population in the hypothetical cluster
# Step 3: Define proposed coverage level and efficacy of vaccine
# Step 4: Calculate estimated effect size
# Step 5: Power calculations - how large a cluster, and how many
# 
# This procedure can be iterated many times for different cluster locations,
# different cluster sizes, and different vaccine coverage criteria
#

##### 
# Calibrate baseline
# Essentially, finds mosquito population density to reproduce the baseline
# This function is intended to be used with a root solver to solve the Ross-Macdonald equations
#
# Inputs: PR
# Outputs: m = mosquitoes/human
m.calibrator <- function(m, PR){
  a <- 0.3*0.9;
  b <- 0.55;
  c <- 0.15;
  peip <- 0.9^11;
  g <- 1 - 0.9;
  r <- 1/200.;
  (a*a*b*c*m*peip - g*r)/(a*c*(a*b*m*peip + r)) - PR
}
# calibrated to PR = 0.1, answer: step1.calibrator(0.306282)
m.calibrated.ex1 <- uniroot(m.calibrator, c(0,10), PR = 0.1)

#####
# Calculate effect size
# This function, like the previous one, is intended to be used with a root solver to solve the Ross-Macdonald equations
# 
# Inputs: parms = c(m, calibrated using m.calibrator()
#                   v, fraction infected)
#                   bv, vaccine efficacy, recommend writing as a fraction of 0.55 eg 0.5*.55
#
effect.size <- function(x, parms){
  # m is the density of mosquitoes, calibrated in Step 1
  # v is the fraction of people who are vaccinated
  # bv is the rate at which vaccinated people take up new infections
  m <- parms[1];
  v <- parms[2];
  bv <- parms[3];
  a <- 0.3*0.9;
  b <- 0.55;
  c <- 0.15;
  peip <- 0.9^11;
  g <- 1 - 0.9;
  r <- 1/200.;
  PRv <- x[1];
  PRc <- x[2];
  EIR <- x[3];
  F1 <- v*bv*EIR/(bv*EIR + r) - PRv;
  F2 <- (1-v)*(b*EIR)/(b*EIR + r) - PRc;
  F3 <- m*a*a*c*peip*(PRv + PRc)/(a*c*(PRv + PRc) + g) - EIR;
  c(F1, F2, F3)
}
# A very boring case, where nobody is vaccinated in a patch with baseline PR = 0.1
# PRv = 0, PRc = 0.1, EIR = 0.00101010101, m = 0.3062832, v = 0, bv = 0.55
# Returns a vector (PRv, PRc, EIR)
effect.size.ex1 <- multiroot(effect.size, parms = c(m.calibrated.ex1$root, 0,   0.55), start = c(0.05, 0.05, 1), maxiter = 100)
effect.size.ex1$root
# (Should return PRv = 0, PRc = 0.1, EIR = 0.0010101)

#####
# Power calculations
#
# Calculatethe cluster size required for measuring the effect size
# Inputs: p1 = PR with intervention
#         p0 = baseline PR (before intervention)
n.clustersize <- function(p1,p0){
  # For a 90% probability of detecting with p-value < 5%
  (1.96 + 1.28)^2 * (p1*(1 - p1) + p0*(1 - p0))/((p1 - p0)^2)  
}
# Calculatethe number of clusters required for the trial
# Inputs: p1 = PR with intervention
#         p0 = baseline PR (before intervention)
#         k = coefficient of variation, which is the variability (standard deviation/mean) in PR 
#             across the different sectors in the trial
#             (unfortunately our answers are sensitive to this but k is difficult to determine ahead of time)
#         n = cluster size - if not specified then n.clustersize() is used
c.clusternumber <- function(p1, p0, k =  0.5, n = NA){
  if (is.na(n)){
    n <- n.clustersize(p1, p0)
  }
  # For a 90% probability of detecting with p-value < 5%
  c <- 1 + (1.96 + 1.28)^2 * (p1*(1 - p1)/n + p0*(1 - p0)/n + k^2*(p1^2 + p0^2))/((p1 - p0)^2) 
  c
}

#####
# Example set of calculations
# 
# Use as the center sector #10 in Basupu, with about 400 people included in the cluster
# PfPR = 0.205
# Assume the vaccine has a 50% efficacy, so bv = 0.5*.55
cluster.pop <- ceiling(krSmooth(10, pop, cntr.Y, cntr.X, k=0.05, x50 = 100))
PR.baseline <- 0.2055; # for basupu
m.basupu <- uniroot(m.calibrator, c(0,1000), PR = PR.baseline) # should be ~ 0.361

# First, try fixing the coverage to include only 300 doses:
v <- min(1., 300/cluster.pop)
bv <- 0.55*0.5
# Calculate effect size:
prv.prc.eir <- multiroot(effect.size, parms = c(m.basupu$root, v, bv), 
                         start = c(PR.baseline*v, PR.baseline*(1-v), 0.1), maxiter = 100)$root;
prv.prc.eir
# Should find that (PRv, PRc, EIR) are all reduced to 0
#
# Power calculations for the direct effect, based on effect size
#
# Difference between before and after
PR.baseline - prv.prc.eir[1]/v # The second number here is the fraction of those infected with Malaria
# Size of each cluster
n.clustersize(prv.prc.eir[1]/v, PR.baseline)
# Required numbers of clusters
c.clusternumber(prv.prc.eir[1]/v, PR.baseline, k =  0.5)
c.clusternumber(prv.prc.eir[1]/v, PR.baseline, k =  0.5, n = cluster.pop)
### Power calculations for the indirect effect, based on effect size
# (The same in this case, because all PR is reduced to 0)
#
# Difference between before and after
PR.baseline - prv.prc.eir[2]/(1-v) # returns NaNs if we have no people in the uninfected group
# Size of each cluster
n.clustersize(prv.prc.eir[2]/(1-v), PR.baseline)
# Required number of clusters
c.clusternumber(prv.prc.eir[2]/(1-v), PR.baseline, k =  0.5)
c.clusternumber(prv.prc.eir[2]/(1-v), PR.baseline, k =  0.5, n = cluster.pop)

# Second, try fixing the coverage to include exactly 90% of the people:
v <- 0.75
bv <- 0.55*0.5

prv.prc.eir <- multiroot(step2.calibrator, parms = c(m.basupu$root, v, bv), 
                         start = c(PR.baseline*v, PR.baseline*(1-v), 0.1), maxiter = 100)$root;
prv.prc.eir
# Should find that (PRv, PRc, EIR) are all reduced to 0
#
# Power calculations for the direct effect, based on effect size
#
# Difference between before and after
PR.baseline - prv.prc.eir[1]/v # The second number here is the fraction of those infected with Malaria
# Size of each cluster
n.clustersize(prv.prc.eir[1]/v, PR.baseline)
# Required numbers of clusters
c.clusternumber(prv.prc.eir[1]/v, PR.baseline, k =  0.5)
c.clusternumber(prv.prc.eir[1]/v, PR.baseline, k =  0.5, n = cluster.pop)
### Power calculations for the indirect effect, based on effect size
# (The same in this case, because all PR is reduced to 0)
#
# Difference between before and after
PR.baseline - prv.prc.eir[2]/(1-v) # returns NaNs if we have no people in the uninfected group
# Size of each cluster
n.clustersize(prv.prc.eir[2]/(1-v), PR.baseline)
# Required number of clusters
c.clusternumber(prv.prc.eir[2]/(1-v), PR.baseline, k =  0.5)
c.clusternumber(prv.prc.eir[2]/(1-v), PR.baseline, k =  0.5, n = cluster.pop)


#####
# METHODOLOGY LIMITATIONS
# * Clusters are always circular
# * No health-seeking behavior taken into account
# * No human travel, inter-cluster or regional, taken into account
# * No mosquito travel
# * No interactions/spillovers between cluster sites
# * Not geographically explicit
#
