require(jsonlite)
require(data.table)
## Change this to the right path
source("/Users/dtcitron/Documents/MASH/MASH-Main/MASH-dev/DanielCitron/MBITES_vector_bionomics/Vector_Bionomics_Calculator.R")

## Change this to the right path
## This is just an example of MBITES output - you'll want to apply these scripts to other outputs later
filename = "/Users/dtcitron/Documents/MASH/MICRO-testing/prettified/run1/mosquito_F_1.json"

## Read in the json file and convert it to a data.table
## NB: the EIP I've specified here is made up nonsense, and should be replaced with a correct value later (ask Dave)
bionomics.data <- combine.bionomics.data(filename, EIP=11)



## These are the four sets of vector binomics related to transmission

## Histogram of lifetimes
lifetimes.distn <- lifetimes.distribution.data(bionomics.data)
hist(lifetimes.distn, breaks = c(-1:ceiling(max(lifetimes.distn)))+.5)

## Histogram of feeding intervals
feedtimes.distn <- feed.intervals.distribution.data(bionomics.data)
hist(feedtimes.distn, breaks = c(0:ceiling(max(feedtimes.distn)))-.5)

## Histogram of number of blood feeding per mosquito lifetime
numbites.distn <- biting.distribution.data(bionomics.data)
hist(numbites.distn, breaks = c(-1:ceiling(max(numbites.distn)))+.5)

## Histogram of vectorial capacity
# This is the one that's a little tricky -
VC.distn <- vc.distribution.data(bionomics.data)
hist(VC.distn, breaks = c(0:(ceiling(max(VC.distn))+1))-.5)
