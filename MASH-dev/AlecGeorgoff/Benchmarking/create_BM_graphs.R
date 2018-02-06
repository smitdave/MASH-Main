#####
# create_BM_graphs
#
# Author: Alec Georgoff
#
# Overview: Create graphs for runtime and memory usage for .csv files output by qacct_retrieval
#####
rm(list = ls())
library(ggplot2)

data <- read.csv("/homes/georgoff/MASH-Main/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/job_run_info_vary_n_humans_3.csv")

### Specify x values for graph (name and value) ###
x_var_text <- "n_humans"
x_var_value <- c(1000,3000,10000,30000,100000,200000,300000)

subtitle <- "Vary over n_humans"

### Open pdf device to write to pdf ###
# pdf(file = "/homes/georgoff/MASH-Main/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_patches.pdf", width = 6, height = 6)

runtime_plot <- ggplot(data,
                 aes(x = log(x_var_value),
                     y = log(runtime/60))) +
  geom_point()

runtime_plot +
  ggtitle(paste0(x_var_text, " vs. Runtime (Log Scale)"), subtitle = subtitle) +
  labs(x = paste0(x_var_text, " (log(", x_var_text, "))"), y = "Runtime (log(Minutes))") +
  annotate("text", x = log(x_var_value[length(x_var_value)-1]), y = log(data$runtime[1]/60),
           label = paste0("Slope = ", # Add slope
                          round(lm(log(runtime)~log(x_var_value), data = data)$coefficients[[2]], digits = 3),
                          ", R^2 = ", # Add R^2
                          round(summary(lm(log(runtime)~log(x_var_value), data = data))$r.squared, digits = 4))) +
  geom_smooth(method = "lm")


memory_plot <- ggplot(data,
                       aes(x = log(x_var_value),
                           y = log(memory/1000))) +
  geom_point()

memory_plot +
  ggtitle(paste0(x_var_text, " vs. Memory Usage (Log Scale)"), subtitle = subtitle) +
  labs(x = paste0(x_var_text, " (log(", x_var_text, "))"), y = "Max Resident Set Size (log(MB))") +
  annotate("text", x = log(x_var_value[length(x_var_value)-1]), y = log(data$memory[1]/1000),
           label = paste0("Slope = ", # Add slope
                          round(lm(log(memory)~log(x_var_value), data = data)$coefficients[[2]], digits = 3),
                          ", R^2 = ", # Add R^2
                          round(summary(lm(log(memory)~log(x_var_value), data = data))$r.squared, digits = 4))) +
  geom_smooth(method = "lm")

### Turn off pdf device, write to file ###
# dev.off()