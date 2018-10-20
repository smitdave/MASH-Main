# For data.table basics: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
# For reshape: http://seananderson.ca/2013/10/19/reshape.html

library(data.table)
library(ggplot2)
library(zoo)

# Work with the output data from Multipatch-test.R

log.2.status <- function(x){
  return(switch(x,
                "S" = "S",
                "I" = "I",
                "P" = "P",
                "P2S" = "S",
                "F" = "I",
                "0" = NA,
                "PEvaxx" = NA,
                "PEwane" = NA,
                {stop("unrecognized transition")}))
}

SIP.Conversion.Curves <- function(human.pathogen.path, human.move.path, patch_humans, tMax){
  # human.pathogen.path = "/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/HumanPathogen_Run0.csv"
  # human.move.path = "/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/HumanMove_Run0.csv"
  # patch_humans = vector of starting human populations in each patch, indexed by patch #
  # tMax = maximum time in the simulation

  # This is a data set with humanID, time, event, vectorID
  human.pathogen <- fread(human.pathogen.path)
  # This is a data set with humanID, time, event, location
  human.move <- fread(human.move.path)
  # Rename "events" as "travel events"
  setnames(human.move, "event", "travelEvent")

  # Bind these two together
  tmp1 = rbind(human.move, human.pathogen, fill=T)
  # Sort by time
  tmp1 = tmp1[order(humanID, time)]

  # Now, we want to be able to keep track of different events as they occur in discrete time:
  #gridtime = data.table(humanID=unique(tmp1$humanID),time=rep(seq(0, tMax, by=1), length(unique(tmp1$humanID))), gridtime=1)
  gridtime = data.table(humanID = sort(rep(unique(tmp1$humanID), tMax + 1)), time=rep(seq(0, tMax, by=1), length(unique(tmp1$humanID))), gridtime = 1)
  # Merge together
  tmp2 <- merge(tmp1, gridtime, by=c('humanID','time'), all=TRUE)
  # Fill in the starting location for each person
  patch_id = rep(x = 1:length(patch_humans), patch_humans)
  initLoc = data.table(humanID=unique(tmp1$humanID), time = 0.0, location = patch_id)
  tmp2[, location:= as.integer(location)]
  tmp2[time==0]$location <- initLoc$location[tmp2[time==0]$humanID] #initLoc$location

  # Fill in a "currentPatch" column for the previous location
  tmp3 <- transform(tmp2, location=na.locf(location))

  # Rename all NAs in the event column as 0, meaning "no event"
  tmp3$event[is.na(tmp3$event)] = 0
  # Create a new column, where we translate the events into current status
  tmp3[, status := sapply(tmp3$event, log.2.status)]
  # Fill in the NA values as the previous value
  tmp3 <- transform(tmp3, status=na.locf(status))
  # now, pull out just the grid time steps
  tmp4 <- tmp3[tmp3$gridtime==1]
  # Look at only the final event that occurs for each human (Thanks, Kelly!)
  tmp4 <- tmp4[order(time, humanID)]
  setkey(tmp4, "humanID")
  tmp4 <- tmp4[, .SD[.N], by = c("time", "humanID")] # Subset of x's Data for each group, taking the final element

  # We are now going to operate on each time step, and on each location
  # This gives a table where I can look up counts by location, time, and status
  tmp5 = tmp4[gridtime==1, .N, by=.(location, time, status)]

  # Lastly, we can use dcast to create status+location vs. time - this is the final product we can put out in a csv!
  #tmp6 <- dcast(tmp5, time ~  status + location, value.var="N")

  return(tmp5)
}

SIP.FULL <- function(t, n, tMax, status.list = c("I", "S")){
  # Create a filled-out table that includes all possible
  full <- data.table(expand.grid(location = c(1:n), status = status.list, time = c(1:tMax)))
  # Merge the two data sets: now I have square data, where ALL combinations of locations, statuses, and times are filled out
  h <- merge(full, t, by = c("time", "location", "status"), all.x = TRUE)
  # replace NAs with 0s
  h[is.na(N)]$N <- 0
  # Return an ordered data set, for consistency
  return(h[order(time, location, status)])
}


# make a plot:
#ggplot(data = tmp6) +
#  geom_line(mapping = aes(x = time, y = I_1), color = "Red") +
#  geom_line(mapping = aes(x = time, y = S_1), color = "Blue") +
#  ggtitle("Location 1") + theme(plot.title = element_text(hjust = .5)) +
#  xlab("Time") + ylab("") +
#  xlim(0,1000) + ylim(0,120)

# A more elegant way of plotting multiple patches:
# http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/


# Check on the conservation of humans:
# https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
#tmp5 = tmp4[gridtime==1, .N, by=.(location, time, status)]
# The ugly way:
#tmp6$I_1 + tmp6$I_2 + tmp6$S_1 + tmp6$S_2
# The data.table way:
#t <- aggregate(tmp5$N, by=list(time=tmp5$time), FUN=sum)
#ggplot(data = t) +
#  geom_point(mapping = aes(x = time, y = x))



# Another test to make: does this still work with single patches?
# This is a data set with humanID, time, event, vectorID
#human.pathogen <- fread("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/single_rho0/HumanPathogen_Run0.csv")
# This is a data set with humanID, time, event, location
#human.move <- fread("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/single_rho0/HumanMove_Run0.csv")


# Another test to make: does this still work with single patches, nonzero rho value?
# This is a data set with humanID, time, event, vectorID
#human.pathogen <- fread("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/single_rho03/HumanPathogen_Run0.csv")
# This is a data set with humanID, time, event, location
#human.move <- fread("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/single_rho03/HumanMove_Run0.csv")



###################
# 3 Patch
###################

# This is a data set with humanID, time, event, vectorID
#human.pathogen <- fread("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/3patch/HumanPathogen_Run0.csv")
# This is a data set with humanID, time, event, location
#human.move <- fread("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/3patch/HumanMove_Run0.csv")

# make a plot:
#library(ggplot2)
#ggplot(data = tmp5) +
#  geom_line(mapping = aes(x = time, y = N, color = status)) +
#  facet_wrap(~location, ncol = 3, labeller = label_parsed) +
#  scale_color_manual(name = "Status",
#                     values = c("#ff0000", "#000cff", "#00ff1d"),
#                     breaks = c("I", "S", "P"),
#                     labels = c("Infected (PR)", "Susceptible", "Protected")) +
#  xlim(0,500) + ylim(0,120) +
#  xlab("Time") + ylab("N")


######
# And here's what happens when we try to plot the number vaccinated on the same plots...
######
SIP.Conversion.Curves.PEvaxx <- function(human.pathogen.path, human.move.path, patch_humans, tMax){
  #human.pathogen.path <- "/Users/dtcitron/Documents/MASH/Bioko_Macro/Bioko_Island_Simulation_Setup/BI_n_patch_vaccine_test/No_MDA_PEvaxx_05/HumanPathogen_Run1.csv"
  #human.move.path <- "/Users/dtcitron/Documents/MASH/Bioko_Macro/Bioko_Island_Simulation_Setup/BI_n_patch_vaccine_test/No_MDA_PEvaxx_05/HumanMove_Run1.csv"
  #tMax = 750 # Maximum time
  #patch_humans <- c(245, 250, 247, 246, 0, 0, 0, 0, 0, 0, 0) # Vector of patch populations vs. time
  # patch_humans = vector of starting human populations in each patch, indexed by patch #
  # tMax = maximum time in the simulation

  # This is a data set with humanID, time, event, vectorID
  human.pathogen <- fread(human.pathogen.path)
  # This is a data set with humanID, time, event, location
  human.move <- fread(human.move.path)
  # Rename "events" as "travel events"
  setnames(human.move, "event", "travelEvent")

  # Bind these two together
  tmp1 = rbind(human.move, human.pathogen, fill=T)
  # Sort by time
  tmp1 = tmp1[order(humanID, time)]

  # Now, we want to be able to keep track of different events as they occur in discrete time:
  #gridtime = data.table(humanID=unique(tmp1$humanID),time=rep(seq(0, tMax, by=1), length(unique(tmp1$humanID))), gridtime=1)
  gridtime = data.table(humanID = sort(rep(unique(tmp1$humanID), tMax + 1)), time=rep(seq(0, tMax, by=1), length(unique(tmp1$humanID))), gridtime = 1)
  # Merge together
  tmp2 <- merge(tmp1, gridtime, by=c('humanID','time'), all=TRUE)
  # Fill in the starting location for each person
  patch_id = rep(x = 1:length(patch_humans), patch_humans)
  initLoc = data.table(humanID=unique(tmp1$humanID), time = 0.0, location = patch_id)
  tmp2[, location:= as.integer(location)]
  tmp2[time==0]$location <- initLoc$location[tmp2[time==0]$humanID] #initLoc$location

  # Log all of the vaccination events in a separate column
  # Do this the same way that the current locations are filled in, by checking the previous status with the zoo package
  tmp2$PEvaxx <- NA_integer_
  # Assume zero vaccinated at time=0
  tmp2[time == 0]$PEvaxx <- 0
  # People are either vaccinated or not vaccinated
  tmp2[event == "PEvaxx"]$PEvaxx <- 1
  tmp2[event == "PEwane"]$PEvaxx <- 0
  # now fill in the current status based on the previous transition to vaccinated or not vaccinated:
  tmp2 <- transform(tmp2, PEvaxx = na.locf(PEvaxx))
  # (Now remains to be seen how the new column affects the rest of the collation of individuals)

  # Fill in a "currentPatch" column for the previous location
  tmp3 <- transform(tmp2, location=na.locf(location))

  # Rename all NAs in the event column as 0, meaning "no event"
  tmp3$event[is.na(tmp3$event)] = 0
  # Create a new column, where we translate the events into current status
  tmp3[, status := sapply(tmp3$event, log.2.status)]
  # Fill in the NA values as the previous value
  tmp3 <- transform(tmp3, status=na.locf(status))
  # now, pull out just the grid time steps
  tmp4 <- tmp3[tmp3$gridtime==1]
  # Look at only the final event that occurs for each human (Thanks, Kelly!)
  tmp4 <- tmp4[order(time, humanID)]
  setkey(tmp4, "humanID")
  tmp4 <- tmp4[, .SD[.N], by = c("time", "humanID")] # Subset of x's Data for each group, taking the final element

  # We are now going to operate on each time step, and on each location
  # This gives a table where I can look up counts by location, time, and status
  tmp5 = tmp4[gridtime==1, .N, by=.(location, time, status)]
  # sum up the total number of vaccinated people in each lcoation at each time
  #tmp6 = tmp4[gridtime==1, sum(PEvaxx), by=.(location, time)]
  # merge these two data frames accordingly
  #tmp5 <- merge(tmp5, tmp6, by=c("time", "location"))
  # Rename the columns to stuff we can use
  #setnames(tmp5, "V1", "N.PEvaxx")

  # Fill in
  h <- SIP.FULL(tmp5, length(patch_humans), tMax, status.list = c("S", "I", "P"))

  # merge with number vaccinated
  h <- merge(h, tmp4[gridtime==1, sum(PEvaxx), by=.(location, time)], by = c("location", "time"))
  setnames(h, "V1", "N.PEvaxx")

  # And fill in the vaccine statuses based on location and time
  # This is kind of kludge-y, there might be a more elegant way of doing this?
  h[status == "P"]$N.PEvaxx <- h[status == "S"]$N.PEvaxx

  return(h)
}

## Make a nice plot:
# For plotting aesthetics
#n <-  length(patch_humans)
#cluster.ids <- c(1, 5, 14, 18)
#pfpr <- c(0.34725000, 0.14958000, 0.15925000, 0.24950000, 0.50000000, 0.10624307, 0.10887395, 0.15729471, 0.01386838, 0.12828326, 0.17975000)
#location.names.table <- data.table(location = c(1:n),
#                                   loc.name = c(as.character(cluster.ids), "Off", "Baney", "Luba", "Malabo", "Moka", "Riaba", "Ureka"),
#                                   loc.pfpr = pfpr*patch.human.populations # expected number of infected humans
#                                  )
#h <- merge(h, location.names.table, by = "location")
#h$dummy <- as.factor("Vaccinated")
#
## Make a plot
#ggplot(data = h) +
#  geom_line(mapping = aes(x = time, y = N, color = status)) +
#  geom_line(mapping = aes(x = time, y = N.PEvaxx, linetype = dummy), color = "purple") +
#  facet_wrap(~loc.name, ncol = 3) +
#  geom_hline(data=location.names.table[location %in% c(1:length(cluster.ids), 11,14)],
#             aes(yintercept = loc.pfpr), color = "black") +
#  scale_color_manual(name = "Status",
#                     values = c("red", "blue", "green"),
#                     #values = c("#ff0000", "#000cff", "#00ff1d"),
#                     breaks = c("I", "S", "P"),
#                     labels = c("Infected (PR)", "Susceptible", "Protected")) +
#  scale_linetype_manual(name = " ", values = c(1)) +
#  xlim(0,tMax) + ylim(0,250) +
#  xlab("Time") + ylab("N")
