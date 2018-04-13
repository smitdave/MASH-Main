######################################
# Author: Alec Georgoff
#
# Date: 4/11/18
#
# Purpose: Submit qsubs for BI_n_patch_trial.R using different parameters for each run
######################################

rm(list = ls())

### Define constant parameters ###

# Number of slots to use:
slots <- 2

# Directory where files are stored:
dir <- "/homes/georgoff/MASH-Main/MASH-dev/AlecGeorgoff/Bioko/Bioko_Island_Cluster_Simulations"

# Shell script to use:
shell <- "/homes/georgoff/MASH-Main/MASH-dev/AlecGeorgoff/Bioko/Bioko_Island_Cluster_Simulations/mash_cluster_shell.sh"

# Script to run in each qsub:
script <- "BI_n_patch_trial_qsub.R"

# Which clusters to use:
# clusters <- c(1:41)

# Grouping clusters together:
g1 <- c(1:4)
g2 <- c(5:8)
g3 <- c(9:12)
clusters <- rbind(g1,g2,g3)

# Where to store output and error files:
e_dir <- "/share/temp/sgeoutput/georgoff/errors/"
o_dir <- "/share/temp/sgeoutput/georgoff/output/"


### Loop through clusters and submit qsubs ###

for (i in 1:nrow(clusters)) {
  
  jname <- paste0("BI_patch_trial_cluster_", paste(clusters[i,], collapse = '_'))
  mem <- slots*2
  holds <- "no_holds"
  
  sys.sub <- paste0("qsub -cwd -N ", jname, " -pe multi_slot ", slots, " -P proj_mmc", " -l mem_free=", mem, "G -hold_jid ", holds, " -o ", o_dir, " -e ", e_dir)
  args <- paste("--args", dir, clusters[i,])
  
  print(paste(sys.sub, shell, script, args))
  
  system(paste(sys.sub, shell, script, args))
}
