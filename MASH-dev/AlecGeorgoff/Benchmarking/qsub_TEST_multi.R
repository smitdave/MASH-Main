n_humans <- c(300000)

for (n in n_humans) {
  
  jname <- paste0("Test-Macro_qsub_script_test_", n)
  slots <- 16
  mem <- slots*2
  holds <- "no_holds"
  output_folder <- paste0("n_humans_equals_", n, "_run2")
  
  sys.sub <- paste0("qsub -cwd -N ", jname, " -pe multi_slot ", slots, " -l mem_free=", mem, "G -hold_jid ", holds)
  args <- paste("--args", n, output_folder)
  # This is a shell script, says which R to use, and passes arguments to the R script
  shell <- "mash_shell_script.sh"
  # R script to execute
  script <- "TEST-MACRO-BENCHMARK.R"
  
  # Print to screen the qsub
  print(paste(sys.sub, shell, script, args))
  
  # Execute!
  system(paste(sys.sub, shell, script, args))
}


