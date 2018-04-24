#####
# compile_clustered_outputs.R
#
# Author: Alec Georgoff
#
# Date: 4/23/18
#
# Purpose: Compile single-cluster simulation outputs into one table for analysis
#####

rm(list = ls())

user <- "georgoff"

root <- paste0("/homes/", user)

sim_output_dir <- paste0(root, "/MASH-Main/MASH-dev/AlecGeorgoff/Bioko/Bioko_Island_Cluster_Simulations/BI_n_patch_trial_outputs")
compiled_output_dir <- paste0(root, "/MASH-Main/MASH-dev/AlecGeorgoff/Bioko/Bioko_Island_Cluster_Simulations")

clusters_to_compile <- c(1:41)
num_regions <- 8
tMax <- 750

#######################
## File Sourcing
#######################

source(paste0(compiled_output_dir, "/Multipatch_data_transform.R"))
source(paste0(compiled_output_dir, "/Bioko_Island_Simulation_Setup.R"))

#######################
## Get list of output files in directory
#######################

files <- list.dirs(sim_output_dir)

#######################
## Create data table for storing compiled results
#######################

compiled_conversion_curves <- data.table(time = NA, location = NA, status = NA, N = NA)
compiled_full_curves <- data.table(time = NA, location = NA, status = NA, N = NA)

#######################
## Loop through files and correct location names (Single trajectory)
#######################

for (i in clusters_to_compile) {
  # isolate the output folder for the current cluster:
  file <- files[grepl(paste0("y_", i, "$"), files) & grepl("Single", files)]
  print(file)
  human.pathogen.path <- paste0(file, "/HumanPathogen_Run0.csv")
  human.move.path <- paste0(file, "/HumanMove_Run0.csv")
  
  # retrieve the number of humans in the clusters of interest:
  patch_humans <- cluster.human.pops[c(i, c(42:48))]
  
  # create conversion curve:
  conversion_curve <- SIP.Conversion.Curves(human.pathogen.path = human.pathogen.path,
                                            human.move.path = human.move.path,
                                            patch_humans = patch_humans,
                                            tMax = tMax)
  
  # create full SIP curve:
  full_curve <- SIP.FULL(t = conversion_curve,
                         n = num_regions,
                         tMax = tMax,
                         status.list = c("S", "I", "P"))
  
  # rename regions to their actual names:
  conversion_curve$location <- as.character(conversion_curve$location)
  conversion_curve[location == "2", location := "Off"]
  conversion_curve[location == "3", location := "Baney"]
  conversion_curve[location == "4", location := "Luba"]
  conversion_curve[location == "5", location := "Malabo"]
  conversion_curve[location == "6", location := "Moka"]
  conversion_curve[location == "7", location := "Riaba"]
  conversion_curve[location == "8", location := "Ureka"]
  
  # rename "location 1" to current cluster:
  conversion_curve[location == "1", location := as.character(i)]
  
  # rename regions to their actual names:
  full_curve$location <- as.character(full_curve$location)
  full_curve[location == "2", location := "Off"]
  full_curve[location == "3", location := "Baney"]
  full_curve[location == "4", location := "Luba"]
  full_curve[location == "5", location := "Malabo"]
  full_curve[location == "6", location := "Moka"]
  full_curve[location == "7", location := "Riaba"]
  full_curve[location == "8", location := "Ureka"]
  
  # rename "location 1" to current cluster:
  full_curve[location == "1", location := as.character(i)]
  
  # append to data sets:
  compiled_conversion_curves <- rbind(compiled_conversion_curves, conversion_curve)
  compiled_full_curves <- rbind(compiled_full_curves, full_curve)
}

#######################
## Remove NAs from first row
#######################

compiled_conversion_curves <- compiled_conversion_curves[-1,]
compiled_full_curves <- compiled_full_curves[-1,]