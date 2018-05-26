require(jsonlite)
source("/Users/dtcitron/Documents/MASH/MICRO-testing/Vector_Bionomics_Calculator.R")

# Example calls, using Vector_Bionomics_Calculator.R
mosquito.biting.data <- fromJSON(file("/Users/dtcitron/Documents/MASH/MICRO-testing/prettified/run1/mosquito_F_1.json"))
rows <- nrow(mosquito.biting.data)-1
EIP = 2
bionomics.data <- rbindlist(lapply(X=1:rows, vector.bionomics.processor,
                                   mosquito.biting.data=mosquito.biting.data, EIP=EIP))

## Histogram of lifetimes
lifetimes.distn <- bionomics.data[, .SD[1], by = "mosID"]$lifetime
hist(lifetimes.distn, breaks = c(0:ceiling(max(lifetimes.distn)))-.5)

## Histogram of feeding intervals
# the last bite in the mosquito's life has feeding.interval=NA, so we cut those out
feedtimes.distn <- bionomics.data[!is.na(feeding.intervals)]$feeding.intervals
hist(feedtimes.distn, breaks = c(0:ceiling(max(feedtimes.distn)))-.5)

## Histogram of VC
# If a mosquito has a primary bite with 0 bites, we count 0 secondary VC.bites
# If a mosquito has no primary bite, we count NA secondary VC.bites
## If we want to distinguish all hosts on all days:
VC.distn <- bionomics.data[!is.na(VC.bites)][, sum(VC.bites), by=.(bite.times, bite.hosts)]$V1
a <- hist(VC.distn, breaks = c(0:(ceiling(max(VC.distn))+1))-.5)
## If we want to average over all sites on all days (summing over hosts by site)
VC.distn <- bionomics.data[!is.na(VC.bites)][, sum(VC.bites), by=.(bite.times, bite.sites)]$V1
b <- hist(VC.distn, breaks = c(0:(ceiling(max(VC.distn))+1))-.5)

# Trying still to work with minified data
#mosquito.biting.data <- stream_in(file("/Users/dtcitron/Documents/MASH/MICRO-testing/minified/run1/mosquito_F_1.json"))