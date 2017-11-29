seed <- 0

jname <- paste0("mash-macro-qsub_", seed)
slots <- 1
mem <- slots*2
holds <- "no_holds"

# Setting up the 
sys.sub <- paste0("qsub -cwd -N ", jname, " -pe multi_slot ", slots, " -l mem_free=", mem, "G -hold_jid ", holds,
                  " -o /homes/dtcitron/Tutorials/qsub_practice/Test-MACRO-Emerge-outputs/test-macro-out",
                  " -e /homes/dtcitron/Tutorials/qsub_practice/Test-MACRO-Emerge-outputs/test-macro-err")
args <- paste(seed, sep=" ")
# This is a shell script, says which R to use, and passes arguments to the R script
shell <- "mash_shell_script.sh" # this shell script may need to call r_dock more explicitly?
# R script to execute
script <- "Test-MACRO-Emerge-qsub.R"

# Print to screen the qsub
print(paste(sys.sub, shell, script, args))

# Execute!
system(paste(sys.sub, shell, script, args))
