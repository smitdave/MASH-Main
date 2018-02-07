# Create a single qsub job to run Test-MACRO-Emerge-qsub.R

n <- 100

jname <- paste0("Test-Macro_qsub_script_test_", n)
slots <- 1
mem <- slots*2
holds <- "no_holds"
output_folder <- paste0("n_humans_equals_", n)

sys.sub <- paste0("qsub -cwd -N ", jname, " -pe multi_slot ", slots, " -l mem_free=", mem, "G -hold_jid ", holds,
                  " -o /homes/georgoff/MASH-Main/MASH-dev/AlecGeorgoff/MACRO_test/", output_folder)
args <- paste("--args", n, output_folder)
# This is a shell script, says which R to use, and passes arguments to the R script
shell <- "mash_shell_script.sh"
# R script to execute
script <- "TEST-MACRO-BENCHMARK.R"

# Print to screen the qsub
print(paste(sys.sub, shell, script, args))

# Execute!
system(paste(sys.sub, shell, script, args))
