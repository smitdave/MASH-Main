# Use this script to start a job with foo.r using a qsub
seed <- 1

jname <- paste0("mash-macro-qsub_", seed)
slots <- 1
mem <- slots*2
holds <- "no_holds"

# Setting up the 
sys.sub <- paste0("qsub -cwd -N ", jname, " -pe multi_slot ", slots, " -l mem_free=", mem, "G -hold_jid ", holds,
                  " -o foo_outputs/boo_output",
                  " -e foo_outputs/boo_errors"
                  )
args <- paste(seed, sep=" ")
# This is a shell script, says which R to use, and passes arguments to the R script
shell <- "r_shell.sh" # Add this to a r_shell.sh file #$ -S /bin/sh # r_dock <$1 --no-save $@
# R script to execute
script <- "foo.r"

# Print to screen the qsub
print(paste(sys.sub, shell, script, "\"", args, "\""))

# Execute!
system(paste(sys.sub, shell, script, "\"", args, "\""))
