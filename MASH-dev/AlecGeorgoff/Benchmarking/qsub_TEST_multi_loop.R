#####
# qsub_TEST_multi_loop
#
# Author: Alec Georgoff
#
# Overview: This script takes as input multiple vectors of parameter values and creates a qsub for each combination of
# those values
#####

rm(list = ls())

### Define looping parameters ###
# Possible combinations:
#     Loop over one parameter, hold others constant:
#       n_humans <- c(50000)
#       n_patches <- c(10,100,1000,10000)
#       tMax <- c(365)
#
#     Loop over multiple parameters:
#       n_humans <- c(100,1000,10000,100000)
#       n_patches <- c(10,100,1000,10000)
#       tMax <- c(365,365,365,365)
n_humans <- c(100000,200000,300000)
n_patches <- c(10)
tMax <- c(365)

## Output directories will be named "nh_XXX_np_XXX_tMax_XXX"

### Define constant parameters ###
slots <- 8
data_directory <- "/share/scratch/users/georgoff/benchmarking_results/" # Location for output folders to be created
shell <- "mash_shell_script.sh" # Shell script for MASH runs
script <- "TEST-MACRO-BENCHMARK.R" # Script to run in each qsub
optional_identifier <- "_weekend_run_2" # Put an identifier if running a job with the same parameters as a previously completed job (i.e. "_run2")
# WARNING: If optional_identifier is NOT included, results from previous runs that have identical parameters will be overwritten!!!

### Create flags for which parameter to vary over ###
# WARNING: Only one of these can be true, otherwise code will not function correctly!
vary_over_n_humans <- T
vary_over_n_patches <- F
vary_over_tMax <- F
vary_over_multi <- F

if (vary_over_multi) {
  for (i in 1:length(n_humans)) {
    
    jname <- paste0("Test-Macro_nh_", n_humans[i], "_np_", n_patches[i], "_tMax_", tMax[i])
    slots <- slots # [not necessary, just for clarity]
    mem <- slots*2
    holds <- "no_holds"
    
    output_folder <- paste0("nh_", n_humans[i], "_np_", n_patches[i], "_tMax_", tMax[i], optional_identifier)
    
    sys.sub <- paste0("qsub -cwd -N ", jname, " -pe multi_slot ", slots, " -l mem_free=", mem, "G -hold_jid ", holds)
    args <- paste("--args", n_humans[i], n_patches[i], tMax[i], data_directory, output_folder)
    # This is a shell script, says which R to use, and passes arguments to the R script
    shell <- shell # [not necessary, just for clarity]
    # R script to execute
    script <- script # [not necessary, just for clarity]
    
    # Print to screen the qsub
    print(paste(sys.sub, shell, script, args))
    
    # Execute!
    system(paste(sys.sub, shell, script, args))
  }
}

if (vary_over_n_humans) {
  ## loop over n_humans
  for (n in n_humans) {
    
    jname <- paste0("Test-Macro_nh_", n)
    slots <- slots # [not necessary, just for clarity]
    mem <- slots*2
    holds <- "no_holds"

    output_folder <- paste0("nh_", n, "_np_", n_patches, "_tMax_", tMax, optional_identifier)
    
    sys.sub <- paste0("qsub -cwd -N ", jname, " -pe multi_slot ", slots, " -l mem_free=", mem, "G -hold_jid ", holds)
    args <- paste("--args", n, n_patches, tMax, data_directory, output_folder)
    # This is a shell script, says which R to use, and passes arguments to the R script
    shell <- shell # [not necessary, just for clarity]
    # R script to execute
    script <- script # [not necessary, just for clarity]
    
    # Print to screen the qsub
    print(paste(sys.sub, shell, script, args))
    
    # Execute!
    system(paste(sys.sub, shell, script, args))
  }
}

if (vary_over_n_patches) {
  ## loop over n_patches
  for (n in n_patches) {
    
    jname <- paste0("Test-Macro_np_", n)
    slots <- slots # [not necessary, just for clarity]
    mem <- slots*2
    holds <- "no_holds"

    output_folder <- paste0("nh_", n_humans, "_np_", n, "_tMax_", tMax, optional_identifier)
    
    sys.sub <- paste0("qsub -cwd -N ", jname, " -pe multi_slot ", slots, " -l mem_free=", mem, "G -hold_jid ", holds)
    args <- paste("--args", n_humans, n, tMax, data_directory, output_folder)
    # This is a shell script, says which R to use, and passes arguments to the R script
    shell <- shell # [not necessary, just for clarity]
    # R script to execute
    script <- script # [not necessary, just for clarity]
    
    # Print to screen the qsub
    print(paste(sys.sub, shell, script, args))
    
    # Execute!
    system(paste(sys.sub, shell, script, args))
  }
}

if (vary_over_tMax) {
  ## loop over tMax
  for (n in tMax) {
    
    jname <- paste0("Test-Macro_tMax_", n)
    slots <- slots # [not necessary, just for clarity]
    mem <- slots*2
    holds <- "no_holds"

    output_folder <- paste0("nh_", n_humans, "_np_", n_patches, "_tMax_", n, optional_identifier)
    
    sys.sub <- paste0("qsub -cwd -N ", jname, " -pe multi_slot ", slots, " -l mem_free=", mem, "G -hold_jid ", holds)
    args <- paste("--args", n_humans, n_patches, n, data_directory, output_folder)
    # This is a shell script, says which R to use, and passes arguments to the R script
    shell <- shell # [not necessary, just for clarity]
    # R script to execute
    script <- script # [not necessary, just for clarity]
    
    # Print to screen the qsub
    print(paste(sys.sub, shell, script, args))
    
    # Execute!
    system(paste(sys.sub, shell, script, args))
  }
}
