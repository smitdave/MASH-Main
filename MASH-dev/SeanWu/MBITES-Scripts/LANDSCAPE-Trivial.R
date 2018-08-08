###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Trivial Landscape
#     MBITES Team
#     June 2018
#
###############################################################################

rm(list = ls());gc()
library(MBITES)


###############################################################################
# Make landscape initialization object
###############################################################################

# landscapes
landscape <- vector(mode = "list",length = 3)

# site characteristics
for(i in 1:3){
  landscape[[i]]$id = i
  landscape[[i]]$xy = c(1,1)
  landscape[[i]]$type = 1L
  landscape[[i]]$tileID = 1L
  landscape[[i]]$move = rep(0.5,2)
  landscape[[i]]$move_id = (1:3)[-i]
  landscape[[i]]$haz = 0
  # null resources
  landscape[[i]]$feed = NULL
  landscape[[i]]$aqua = NULL
}

# site 1 has both resources
landscape[[1]]$feed[[1]] = list(w=1,enterP=1)
landscape[[1]]$aqua[[1]] = list(w=1,lambda=1)

# site 2 has only blood feeding resource
landscape[[2]]$feed[[1]] = list(w=1,enterP=1)

# site 3 has only aquatic habitat resource
landscape[[3]]$aqua[[1]] = list(w=1,lambda=1)


###############################################################################
# Make human initialization object
###############################################################################

nHumans = 2

humans = data.frame(
  tileID = rep(1,nHumans),
  # make sure the humans are at the sites with blood feeding resources
  siteID = 1:2,
  feedingID = rep(1,nHumans),
  w = rep(1,nHumans)
)


###############################################################################
# Make mosquito initialization object
###############################################################################

nMosquitos = 50

mosquitos = data.frame(
  tileID = rep(1,nMosquitos),
  # make sure the mosquitos emerge from aquatic habitats
  siteID =rep(c(1,3),length.out=nMosquitos),
  female = rep(T,nMosquitos)
)


###############################################################################
# Run MBITES
###############################################################################


directory <- "/Users/slwu89/Desktop/mbites/trivial/"

# initialize methods
MBITES_Setup_MBDETES()

PATHOGEN_Setup(pathogen_model = "null")

# we want detailed output of blood hosts from the mosquito
trackBloodHost()
trackOviposition()

# set parameters
MBITES:::Parameters$set_parameters(disperse = 0.25,Bs_surv = 0.95,Os_surv = 0.95,B_surv = 0.98,O_surv = 0.98,
                                   Bs_succeed = 0.99,Os_succeed = 0.99,B_succeed = 0.95,O_succeed = 0.99,
                                   S_u = 0)


# initialize a tile
Tile_Initialize(landscape)

Human_NULL_Initialize(humans)

# transitions <- MBDETES_Approx(1L)

MBITES_Initialize(mosquitos)

# run simulation
set_output(directory = directory,runID = 1)

simulation(tMax = 365*3,pretty = TRUE)
hardreset()


###############################################################################
# Analysis
###############################################################################

library(jsonlite)
library(ggplot2)

# where the files can be found
output_dir <- paste0(directory,"run1")

mosquitos_df <- fromJSON(paste0(output_dir,"/mosquito_F_1.json"), flatten = TRUE)
mosquitos_df <- mosquitos_df[-which(sapply(mosquitos_df$id,is.null)),]
humans_df <- fromJSON(paste0(output_dir,"/human_1.json"), flatten = TRUE)
humans_df <- humans_df[-which(sapply(humans_df$id,is.null)),]


###############################################################################
# basic bionomics
###############################################################################

# lifespan
lf <- Bionomics_lifespan(mosquitos_df)
mean_lf <- mean(lf$lifespan)

ggplot() + geom_histogram(data = lf, aes(lifespan), fill = "steelblue", bins = 20) +
  geom_vline(xintercept = mean_lf,col="firebrick3",size=1.15) +
  ggtitle(paste0("Mosquito Lifespans (mean: ",round(mean_lf,3),")")) + xlab("Days") + ylab("Frequency") + theme_bw()

# human blood hosts
bh <- Bionomics_humanBloodHost(mosquitos_df,who = "human")
mean_bh <- mean(bh$humanHost)

ggplot() + geom_histogram(data = bh, aes(humanHost), fill = "steelblue", bins = 20) +
  geom_vline(xintercept = mean_bh,col="firebrick3",size=1.15) +
  ggtitle(paste0("Number of Human Blood Hosts per mosquito (mean: ",round(mean_bh,3),")")) + xlab("Number of Hosts") + ylab("Frequency") + theme_bw()

# blood meal intervals
bmi <- Bionomics_bloodIntervals(mosquitos_df,who = "human")
mean_bmi <- mean(bmi$bmIntervals)

ggplot() + geom_histogram(data = bmi, aes(bmIntervals), fill = "steelblue", bins = 20) +
  geom_vline(xintercept = mean_bmi,col="firebrick3",size=1.15) +
  ggtitle(paste0("Human Blood Meal Interval (mean: ",round(mean_bmi,3),")")) + xlab("Time") + ylab("Frequency") + theme_bw()

# vectorial capacity (might not make a whole lot of sense to look at the histogram, just mean value)
vc <- Bionomics_vectorialCapacity(mosquitos = mosquitos_df,humans = humans_df,EIP = 10,spatial = T)
vc_df <- data.frame(vc=sapply(vc,function(x){x$VC}))
mean_vc <- mean(vc_df$vc)

ggplot() + geom_histogram(data = vc_df, aes(vc), fill = "steelblue", bins = 20) +
  geom_vline(xintercept = mean_vc,col="firebrick3",size=1.15) +
  ggtitle(paste0("Vectorial Capacity (mean: ",round(mean_vc,3),")")) + xlab("Secondary Bites") + ylab("Frequency") + theme_bw()

# lifetime egg production
egg <- Bionomics_lifetimeOviposition(mosquitos_df,TRUE)
egg_df <- data.frame(egg=egg$lifetime)
mean_egg <- mean(egg_df$egg)

ggplot() + geom_histogram(data = egg_df, aes(egg), fill = "steelblue", bins = 20) +
  geom_vline(xintercept = mean_egg,col="firebrick3",size=1.15) +
  ggtitle(paste0("Lifetime Egg Production (mean: ",round(mean_egg,3),")")) + xlab("Eggs") + ylab("Frequency") + theme_bw()

# oviposition intervals and successful events
oviposit <- Bionomics_ovipositionInterval(mosquitos_df)

oviposit_interval <- data.frame(interval=oviposit$interval)
mean_oviposit_interval <- mean(oviposit_interval$interval)

oviposit_num <- data.frame(number=oviposit$numOviposit)
mean_oviposit_num <- mean(oviposit_num$number)

ggplot() + geom_histogram(data = oviposit_interval, aes(interval), fill = "steelblue", bins = 20) +
  geom_vline(xintercept = mean_oviposit_interval,col="firebrick3",size=1.15) +
  ggtitle(paste0("Interval between Successful Oviposition (mean: ",round(mean_oviposit_interval,3),")")) + xlab("Time") + ylab("Frequency") + theme_bw()

ggplot() + geom_histogram(data = oviposit_num, aes(number), fill = "steelblue", bins = 20) +
  geom_vline(xintercept = mean_oviposit_num,col="firebrick3",size=1.15) +
  ggtitle(paste0("Number of Successful Oviposition Events (mean: ",round(mean_oviposit_num,3),")")) + xlab("Number of Events") + ylab("Frequency") + theme_bw()



rate <- Bionomics_ovipositionRate(mosquitos_df)

eggRate <- density(rate$ages)

egg_mean <- weighted.mean(eggRate$x,eggRate$y)
plot(eggRate$x, eggRate$y,type = "l", xlab = "Age (days)", ylab = "Density", main = paste0("MBITES Egg Laying Rate (mean: ",round(egg_mean,3),")"),lwd=2,col="steelblue")
polygon(c(0, eggRate$x), c(0, eggRate$y), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = egg_mean,lwd=2.5,lty=2,col="firebrick3")


blood <- Bionomics_bloodfeedingRate(mosquitos_df)

bloodRate <- density(blood)

blood_mean <- weighted.mean(bloodRate$x,bloodRate$y)
plot(bloodRate$x, bloodRate$y,type = "l", xlab = "Age (days)", ylab = "Density", main = paste0("MBITES Blood Feeding Rate (mean: ",round(blood_mean,3),")"),lwd=2,col="steelblue")
polygon(c(0, bloodRate$x), c(0, bloodRate$y), border=NA, col=adjustcolor("steelblue",alpha.f = 0.5))
abline(v = blood_mean,lwd=2.5,lty=2,col="firebrick3")

# ages <- as.vector(rate$ages)
# eggs <- as.vector(rate$batches)
#
# ages <- c(-0.1,rep(0,1e2),ages)
# eggs <- c(0,rep(0,1e2),eggs)
#
# eggs_c <- cumsum(eggs)
#
# eggs_c_s <- smooth.spline(x = ages,y = eggs_c,all.knots = TRUE,cv = NA,keep.data = FALSE)
# eggs_rate <- predict(object = eggs_c_s,x = ages,deriv = 1)
#
# plot(eggs_rate$x,eggs_rate$y/length(ages),type="l")
