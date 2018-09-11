###############################################################################
#         __  ___      ____  _____________________
#        /  |/  /     / __ )/  _/_  __/ ____/ ___/
#       / /|_/ /_____/ __  |/ /  / / / __/  \__ \
#      / /  / /_____/ /_/ // /  / / / /___ ___/ /
#     /_/  /_/     /_____/___/ /_/ /_____//____/
#
#     Simulations of Peri-domestic breeding resource-scapes
#     Script for submitting series of jobs in parallel
#     MBITES Team
#     May 2018
#
###############################################################################

rm(list=ls());gc()


###############################################################################
# Set up scripts for submitting qsub jobs:
###############################################################################


seeds <- 1:26

for (seed in seeds) {
  jname <- paste0("mash-macro-qsub_", seed)
  slots <- 1
  mem <- slots*2
  holds <- "no_holds"
  # Setting up the qsub job
  sys.sub <- paste0("qsub -cwd -N ", jname, " -pe multi_slot ", slots, " -l mem_free=", mem, "G -hold_jid ", holds #,
                    #" -o /ihme/malaria_modeling/dtcitron/MBITES_Experiment_Simulations/foo_outputs/coo_output", # where output will go
                    #" -e /ihme/malaria_modeling/dtcitron/MBITES_Experiment_Simulations/foo_outputs/coo_errors"  # where errors will go
  )
  args <- paste(seed, sep=" ")
  
  # This is a shell script, says which R to use, and passes arguments to the R script
  shell <- "/ihme/malaria_modeling/dtcitron/MBITES_Experiment_Simulations/rmash_shell.sh" # Add this to a r_shell.sh file #$ -S /bin/sh # r_dock <$1 --no-save $@
  # R script to execute
  script <- "/ihme/malaria_modeling/dtcitron/MBITES_Experiment_Simulations/peridom_qsub.R"
  
  
  # Print to screen the qsub
  print(paste(sys.sub, shell, script, "\"", args, "\""))
  
  # Execute!
  system(paste(sys.sub, shell, script, "\"", args, "\""))
}

rm(cl);gc()
