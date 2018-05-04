rm(list = ls())

library(data.table)
library(ggplot2)

user <- "georgoff"

root <- paste0("/homes/", user)

sim_output_dir <- paste0(root, "/MASH-Main/MASH-dev/AlecGeorgoff/Bioko/Bioko_Island_Cluster_Simulations/BI_n_patch_trial_outputs")
compiled_output_dir <- paste0(root, "/MASH-Main/MASH-dev/AlecGeorgoff/Bioko/Bioko_Island_Cluster_Simulations")
final_output_dir <- paste0(root, "/MASH-Main/MASH-dev/AlecGeorgoff/Bioko/Bioko_Island_Cluster_Simulations/BI_compiled_data")

cccs <- fread(file = paste0(final_output_dir, "/compiled_conversion_curves_single.csv"))
cfcs <- fread(file = paste0(final_output_dir, "/compiled_full_curves_single.csv"))
ccce <- fread(file = paste0(final_output_dir, "/compiled_conversion_curves_ensemble.csv"))
cfce <- fread(file = paste0(final_output_dir, "/compiled_full_curves_ensemble.csv"))

plot1 <- ggplot(data = cccs[location == "41" & status == "I"]) +
  geom_line(aes(x = cccs[location == "41" & status == "I", time],
                y = cccs[location == "41" & status == "I", N]))

plot1

plot2 <- ggplot(data = cccs[location == "41" & status == "S"]) +
  geom_line(aes(x = cccs[location == "41" & status == "S", time],
                y = cccs[location == "41" & status == "S", N]))

plot2

plot3 <- ggplot(data = cccs[location == "41" & status == "P"]) +
  geom_line(aes(x = cccs[location == "41" & status == "P", time],
                y = cccs[location == "41" & status == "P", N]))

plot3