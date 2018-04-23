#####
# compile_clustered_outputs.R
#
# Author: Alec Georgoff
#
# Date: 4/23/18
#
# Purpose: Compile single-cluster simulation outputs into one .csv for analysis
#####

rm(list = ls())

user <- "georgoff"

root <- paste0("/homes/", user)

sim_output_dir <- paste0(root, "/MASH-Main/MASH-dev/AlecGeorgoff/Bioko/Bioko_Island_Cluster_Simulations/BI_n_patch_trial_outputs")
compiled_output_dir <- paste0(root, "/MASH-Main/MASH-dev/AlecGeorgoff/Bioko/Bioko_Island_Cluster_Simulations")

#######################
## File Sourcing
#######################

source(paste0(compiled_output_dir, "/Multipatch_data_transform.R"))
