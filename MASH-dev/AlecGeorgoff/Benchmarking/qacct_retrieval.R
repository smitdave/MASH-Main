#####
# qacct_retrieval
#
# Author: Alec Georgoff
#
# Overview: Extract ru_wallclock and ru_maxrss from qacct output for given job_id(s)
#####
rm(list = ls())
library(stringr)

### Input job_ids ###
job_id_list <- c(162314631, 162314632, 162314633, 162314634, 162314636, 162314637)
unique_id <- "_vary_n_humans_3" # Unique ID to be appended to end of results file


### Create data frame to store results ###
job_run_info <- matrix(NA, nrow = length(job_id_list), ncol = 3)
job_run_info <- as.data.frame(job_run_info)
colnames(job_run_info) <- c("job_id","runtime", "memory")

i <- 1

for (job_id in job_id_list) {
  # Collect qacct output as a vector
  qacct_outputs <- system(paste0("qacct -j ", job_id), intern = T)
  
  # Store runtime information
  job_run_info$runtime[i] <- as.double(paste(str_match_all(qacct_outputs[24], "[1234567890.]")[[1]][,1], collapse = ''))
  
  # Store memory usage information
  job_run_info$memory[i] <- as.double(paste(str_match_all(qacct_outputs[27], "[1234567890.]")[[1]][,1], collapse = ''))
  
  # Store job_id
  job_run_info$job_id[i] <- job_id
  
  i <- i+1
}

write.csv(x = job_run_info, file = paste0("/homes/georgoff/MASH-Main/MASH-dev/AlecGeorgoff/Benchmarking", "/BM_results/job_run_info", unique_id, ".csv"))
