# Create a single qsub job to run Test-MACRO-Emerge-qsub.R

jname <- "Test-Macro_qsub_script_test"
slots <- 1
mem <- slots*2
holds <- "no_holds"

n = 10 # n parameter to be used by the TEST-MACRO script

# Setting up the 
sys.sub <- paste0("qsub -cwd -N ", jname, " -pe multi_slot ", slots, " -l mem_free=", mem, "G -hold_jid ", holds)
args <- paste0("--args ", n)
# This is a shell script, says which R to use, and passes arguments to the R script
shell <- "mash_shell_script.sh"
# R script to execute
script <- "TEST-MACRO.R"

# Print to screen the qsub
print(paste(sys.sub, shell, script, args))

# Execute!
system(paste(sys.sub, shell, script, args))
