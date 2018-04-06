# For data.table basics: https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
# For reshape: http://seananderson.ca/2013/10/19/reshape.html

library(data.table)
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
                {stop("unrecognized transition")}))
}

# This is a data set with humanID, time, event, vectorID
human.pathogen <- fread("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/HumanPathogen_Run0.csv")
# This is a data set with humanID, time, event, location
human.move <- fread("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/HumanMove_Run0.csv")
# Rename "events" as "travel events"
setnames(human.move, "event", "travelEvent")

# Bind these two together
tmp1 = rbind(human.move, human.pathogen, fill=T)
# Sort by time
tmp1 = tmp1[order(humanID, time)]

# Now, we want to be able to keep track of different events as they occur in discrete time:
gridtime = data.table(humanID=unique(tmp1$humanID),time=rep(seq(0, 1000, by=1), length(unique(tmp1$humanID))), gridtime=1)
# Merge together
tmp2 <- merge(tmp1, gridtime, by=c('humanID','time'), all=TRUE)

# Fill in the starting location for each person
patch_id = rep(x = 1:n,patch_humans)
initLoc = data.table(humanID=unique(tmp1$humanID), time = 0.0, location = patch_id)
tmp2[, location:= as.integer(location)]
tmp2[time==0]$location <- initLoc$location

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

# The last step will just be to add in some "P" events? I think it needs to happen after we've counted S's and I's
# How did I add in P events before?  I transformed the data using dcast, then looked for a column that gave the "P" status

# Lastly, we can use dcast to create status+location vs. time - this is the final product we can put out in a csv!
tmp6 <- dcast(tmp5, time ~  status + location, value.var="N")

# make a plot:
ggplot(data = tmp6) +
  geom_line(mapping = aes(x = time, y = I_1), color = "Red") +
  geom_line(mapping = aes(x = time, y = S_1), color = "Blue") +
  ggtitle("Location 1") + theme(plot.title = element_text(hjust = .5)) +
  xlab("Time") + ylab("") +
  xlim(0,1000) + ylim(0,120)

# A more elegant way of plotting multiple patches:
# http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/

tmp5$location = as.character(tmp5$location)
tmp5$location[tmp5$location == "1"] = "Patch~1"
tmp5$location[tmp5$location == "2"] = "Patch~2"

ggplot(data = tmp5) +
  geom_line(mapping = aes(x = time, y = N, color = status)) +
  facet_wrap(~location, ncol = 2, labeller = label_parsed) +
  scale_color_manual(name = "Status",
                       values = c("#ff0000", "#000cff"),
                       breaks = c("I", "S"),
                       labels = c("Infected (PR)", "Susceptible")) +
  xlim(0,500) + ylim(0,120) +
  xlab("Time") + ylab("N")

# Check on the conservation of humans:
# https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
tmp5 = tmp4[gridtime==1, .N, by=.(location, time, status)]
# The ugly way:
tmp6$I_1 + tmp6$I_2 + tmp6$S_1 + tmp6$S_2
# The data.table way:
t <- aggregate(tmp5$N, by=list(time=tmp5$time), FUN=sum)
ggplot(data = t) +
  geom_point(mapping = aes(x = time, y = x))



# Another test to make: does this still work with single patches?
# This is a data set with humanID, time, event, vectorID
human.pathogen <- fread("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/rho0/HumanPathogen_Run0.csv")
# This is a data set with humanID, time, event, location
human.move <- fread("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/rho0/HumanMove_Run0.csv")
setnames(human.move, "event", "travelEvent")

# Bind these two together
tmp1 = rbind(human.move, human.pathogen, fill=T)
# Sort by time
tmp1 = tmp1[order(humanID, time)]

# Now, we want to be able to keep track of different events as they occur in discrete time:
gridtime = data.table(humanID=unique(tmp1$humanID),time=rep(seq(0, 1000, by=1), length(unique(tmp1$humanID))), gridtime=1)
# Merge together
tmp2 <- merge(tmp1, gridtime, by=c('humanID','time'), all=TRUE)

# Fill in the starting location for each person
patch_id = rep(x=1:1,1000)
initLoc = data.table(humanID=unique(tmp1$humanID), time = 0.0, location = patch_id)
tmp2[, location:= as.integer(location)]
tmp2[time==0]$location <- initLoc$location

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

# We are now going to operate on each time step, and on each location
# This gives a table where I can look up counts by location, time, and status
tmp5 = tmp4[gridtime==1, .N, by=.(location, time, status)]

# The last step will just be to add in some "P" events? I think it needs to happen after we've counted S's and I's
# How did I add in P events before?  I transformed the data using dcast, then looked for a column that gave the "P" status

# Lastly, we can use dcast to create status+location vs. time - this is the final product we can put out in a csv!
tmp6 <- dcast(tmp5, time ~  status + location, value.var="N")

ggplot(data = tmp5) +
  geom_line(mapping = aes(x = time, y = N, color = status)) +
  facet_wrap(~location, ncol = 2, labeller = label_parsed) +
  scale_color_manual(name = "Status",
                     values = c("#ff0000", "#000cff"),
                     breaks = c("I", "S"),
                     labels = c("Infected (PR)", "Susceptible")) +
  xlim(0,1000) + ylim(0,1000) +
  xlab("Time") + ylab("N")


# Another test to make: does this still work with single patches?
# This is a data set with humanID, time, event, vectorID
human.pathogen <- fread("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/single/HumanPathogen_Run0.csv")
# This is a data set with humanID, time, event, location
human.move <- fread("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/single/HumanMove_Run0.csv")
setnames(human.move, "event", "travelEvent")

# Bind these two together
tmp1 = rbind(human.move, human.pathogen, fill=T)
# Sort by time
tmp1 = tmp1[order(humanID, time)]

# Now, we want to be able to keep track of different events as they occur in discrete time:
gridtime = data.table(humanID=unique(tmp1$humanID),time=rep(seq(0, 1000, by=1), length(unique(tmp1$humanID))), gridtime=1)
# Merge together
tmp2 <- merge(tmp1, gridtime, by=c('humanID','time'), all=TRUE)

# Fill in the starting location for each person
patch_id = rep(x=1:1,1000)
initLoc = data.table(humanID=unique(tmp1$humanID), time = 0.0, location = patch_id)
tmp2[, location:= as.integer(location)]
tmp2[time==0]$location <- initLoc$location

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

# We are now going to operate on each time step, and on each location
# This gives a table where I can look up counts by location, time, and status
tmp5 = tmp4[gridtime==1, .N, by=.(location, time, status)]

# The last step will just be to add in some "P" events? I think it needs to happen after we've counted S's and I's
# How did I add in P events before?  I transformed the data using dcast, then looked for a column that gave the "P" status

# Lastly, we can use dcast to create status+location vs. time - this is the final product we can put out in a csv!
tmp6 <- dcast(tmp5, time ~  status + location, value.var="N")

ggplot(data = tmp5) +
  geom_line(mapping = aes(x = time, y = N, color = status)) +
  facet_wrap(~location, ncol = 2, labeller = label_parsed) +
  scale_color_manual(name = "Status",
                     values = c("#ff0000", "#000cff"),
                     breaks = c("I", "S"),
                     labels = c("Infected (PR)", "Susceptible")) +
  xlim(0,1000) + ylim(0,1000) +
  xlab("Time") + ylab("N")




# Another test to make: does this still work with single patches?
# This is a data set with humanID, time, event, vectorID
human.pathogen <- fread("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/single_rho0/HumanPathogen_Run0.csv")
# This is a data set with humanID, time, event, location
human.move <- fread("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/single_rho0/HumanMove_Run0.csv")
setnames(human.move, "event", "travelEvent")

# Bind these two together
tmp1 = rbind(human.move, human.pathogen, fill=T)
# Sort by time
tmp1 = tmp1[order(humanID, time)]

# Now, we want to be able to keep track of different events as they occur in discrete time:
gridtime = data.table(humanID=unique(tmp1$humanID),time=rep(seq(0, 1000, by=1), length(unique(tmp1$humanID))), gridtime=1)
# Merge together
tmp2 <- merge(tmp1, gridtime, by=c('humanID','time'), all=TRUE)

# Fill in the starting location for each person
patch_id = rep(x=1:1,1000)
initLoc = data.table(humanID=unique(tmp1$humanID), time = 0.0, location = patch_id)
tmp2[, location:= as.integer(location)]
tmp2[time==0]$location <- initLoc$location

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

# The last step will just be to add in some "P" events? I think it needs to happen after we've counted S's and I's
# How did I add in P events before?  I transformed the data using dcast, then looked for a column that gave the "P" status

# Lastly, we can use dcast to create status+location vs. time - this is the final product we can put out in a csv!
tmp6 <- dcast(tmp5, time ~  status + location, value.var="N")

ggplot(data = tmp5) +
  geom_line(mapping = aes(x = time, y = N, color = status)) +
  facet_wrap(~location, ncol = 2, labeller = label_parsed) +
  scale_color_manual(name = "Status",
                     values = c("#ff0000", "#000cff"),
                     breaks = c("I", "S"),
                     labels = c("Infected (PR)", "Susceptible")) +
  xlim(0,1000) + ylim(0,1000) +
  xlab("Time") + ylab("N")


# Another test to make: does this still work with single patches?
# This is a data set with humanID, time, event, vectorID
human.pathogen <- fread("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/single_rho03/HumanPathogen_Run0.csv")
# This is a data set with humanID, time, event, location
human.move <- fread("/Users/dtcitron/Documents/MASH/MACRO-multipatch-test/data/single_rho03/HumanMove_Run0.csv")
setnames(human.move, "event", "travelEvent")

# Bind these two together
tmp1 = rbind(human.move, human.pathogen, fill=T)
# Sort by time
tmp1 = tmp1[order(humanID, time)]

# Now, we want to be able to keep track of different events as they occur in discrete time:
gridtime = data.table(humanID=unique(tmp1$humanID),time=rep(seq(0, 1000, by=1), length(unique(tmp1$humanID))), gridtime=1)
# Merge together
tmp2 <- merge(tmp1, gridtime, by=c('humanID','time'), all=TRUE)

# Fill in the starting location for each person
patch_id = rep(x=1:1,1000)
initLoc = data.table(humanID=unique(tmp1$humanID), time = 0.0, location = patch_id)
tmp2[, location:= as.integer(location)]
tmp2[time==0]$location <- initLoc$location

# Fill in a "currentPatch" column for the previous location (Thanks, David!)
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

# The last step will just be to add in some "P" events? I think it needs to happen after we've counted S's and I's
# How did I add in P events before?  I transformed the data using dcast, then looked for a column that gave the "P" status

# Lastly, we can use dcast to create status+location vs. time - this is the final product we can put out in a csv!
tmp6 <- dcast(tmp5, time ~  status + location, value.var="N")

t <- tmp5[tmp5$status != "F"]
ggplot(data = t) +
  geom_line(mapping = aes(x = time, y = N, color = status)) +
  facet_wrap(~location, ncol = 2, labeller = label_parsed) +
  scale_color_manual(name = "Status",
                     values = c("#ff0000", "#000cff", "#00ff1d"),
                     breaks = c("I", "S", "P"),
                     labels = c("Infected (PR)", "Susceptible", "Protected")) +
  xlim(0,1000) + ylim(0,1000) +
  xlab("Time") + ylab("N")

# Check on conservation of humans:
t = tmp4[gridtime==1, .N, by=.(location, time, status)]
u <- aggregate(t$N, by=list(time=t$time), FUN=sum)
ggplot(data = u) +
  geom_point(mapping = aes(x = time, y = x))

