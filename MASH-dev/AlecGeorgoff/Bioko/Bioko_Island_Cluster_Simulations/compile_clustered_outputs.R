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
final_output_dir <- paste0(root, "/MASH-Main/MASH-dev/AlecGeorgoff/Bioko/Bioko_Island_Cluster_Simulations/BI_compiled_data")

#######################
## OPTIONS
#######################

compile_single_trajectories <- T
compile_ensemble_trajectories <- T

clusters_to_compile <- c(1:41)
num_regions <- 8
tMax <- 750
ensemble_numbers <- c(0:9)

#######################
## File Sourcing
#######################

source(paste0(compiled_output_dir, "/Multipatch_data_transform.R"))
source(paste0(compiled_output_dir, "/Bioko_Island_Simulation_Setup.R"))

#######################
## Get list of output files in directory
#######################

files <- list.dirs(sim_output_dir)
files_single <- files[grepl("Single", files)]
files_ensemble <- files[grepl("Ensemble", files)]

#######################
## Create data table for storing compiled results
#######################

compiled_conversion_curves_single <- data.table(time = NA, location = NA, status = NA, N = NA)
compiled_full_curves_single <- data.table(time = NA, location = NA, status = NA, N = NA)
compiled_conversion_curves_ensemble <- data.table(time = NA, location = NA, status = NA, N = NA, ens_num = NA)
compiled_full_curves_ensemble <- data.table(time = NA, location = NA, status = NA, N = NA, ens_num = NA)

#######################
## Loop through files and correct location names (Single trajectory)
#######################

if (compile_single_trajectories) {
  for (i in clusters_to_compile) {
    # isolate the output folder for the current cluster:
    file <- files_single[grepl(paste0("y_", i, "$"), files_single)]
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
    compiled_conversion_curves_single <- rbind(compiled_conversion_curves_single, conversion_curve)
    compiled_full_curves_single <- rbind(compiled_full_curves_single, full_curve)
  }
}

#######################
## Loop through files and correct location names (Ensemble trajectory)
#######################

if (compile_ensemble_trajectories) {
  for (i in clusters_to_compile) {
    # isolate the output folder for the current cluster:
    file <- files_ensemble[grepl(paste0("s_", i, "$"), files_ensemble)]
    print(file)
    for (j in ensemble_numbers) {
      human.pathogen.path <- paste0(file, "/HumanPathogen_Run", j , ".csv")
      human.move.path <- paste0(file, "/HumanMove_Run", j, ".csv")
      print(human.pathogen.path)

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

      # add ensemble number to results:
      conversion_curve$ens_num <- j
      full_curve$ens_num <- j

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
      compiled_conversion_curves_ensemble <- rbind(compiled_conversion_curves_ensemble, conversion_curve)
      compiled_full_curves_ensemble <- rbind(compiled_full_curves_ensemble, full_curve)
    }
  }
}

#######################
## Remove NAs from first row
#######################

compiled_conversion_curves_single <- compiled_conversion_curves_single[-1,]
compiled_full_curves_single <- compiled_full_curves_single[-1,]
compiled_conversion_curves_ensemble <- compiled_conversion_curves_ensemble[-1,]
compiled_full_curves_ensemble <- compiled_full_curves_ensemble[-1,]

#######################
## Aggregate regions (Single Trajectory)
#######################

# aggregate by time-location-status combinations:
compiled_conversion_curves_single <- aggregate(compiled_conversion_curves_single$N,
                   by = list(time = compiled_conversion_curves_single$time,
                                                           location = compiled_conversion_curves_single$location,
                                                           status = compiled_conversion_curves_single$status),
                   FUN = sum)
names(compiled_conversion_curves_single)[names(compiled_conversion_curves_single) == "x"] <- "N"
compiled_conversion_curves_single <- as.data.table(compiled_conversion_curves_single)

compiled_full_curves_single <- aggregate(compiled_full_curves_single$N,
                                        by = list(time = compiled_full_curves_single$time,
                                                  location = compiled_full_curves_single$location,
                                                  status = compiled_full_curves_single$status),
                                        FUN = sum)
names(compiled_full_curves_single)[names(compiled_full_curves_single) == "x"] <- "N"
compiled_full_curves_single <- as.data.table(compiled_full_curves_single)

#######################
## Aggregate regions (Ensemble Trajectory)
#######################

# aggregate by time-location-status-ens_num combinations:
compiled_conversion_curves_ensemble <- aggregate(compiled_conversion_curves_ensemble$N,
                                               by = list(time = compiled_conversion_curves_ensemble$time,
                                                         location = compiled_conversion_curves_ensemble$location,
                                                         status = compiled_conversion_curves_ensemble$status,
                                                         ens_num = compiled_conversion_curves_ensemble$ens_num),
                                               FUN = sum)
names(compiled_conversion_curves_ensemble)[names(compiled_conversion_curves_ensemble) == "x"] <- "N"
compiled_conversion_curves_ensemble <- as.data.table(compiled_conversion_curves_ensemble)

compiled_full_curves_ensemble <- aggregate(compiled_full_curves_ensemble$N,
                                         by = list(time = compiled_full_curves_ensemble$time,
                                                   location = compiled_full_curves_ensemble$location,
                                                   status = compiled_full_curves_ensemble$status,
                                                   ens_num = compiled_full_curves_ensemble$ens_num),
                                         FUN = sum)
names(compiled_full_curves_ensemble)[names(compiled_full_curves_ensemble) == "x"] <- "N"
compiled_full_curves_ensemble <- as.data.table(compiled_full_curves_ensemble)

#######################
## Write out results
#######################

write.csv(compiled_conversion_curves_single, file = paste0(final_output_dir, "/compiled_conversion_curves_single.csv"))
write.csv(compiled_full_curves_single, file = paste0(final_output_dir, "/compiled_full_curves_single.csv"))
write.csv(compiled_conversion_curves_ensemble, file = paste0(final_output_dir, "/compiled_conversion_curves_ensemble.csv"))
write.csv(compiled_full_curves_ensemble, file = paste0(final_output_dir, "/compiled_full_curves_ensemble.csv"))