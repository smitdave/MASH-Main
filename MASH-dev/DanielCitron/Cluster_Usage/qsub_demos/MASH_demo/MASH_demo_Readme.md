### Cluster job submission demo, with MASH

#### - Daniel Citron, 11/29/2017

In this directory you will find three R scripts and one shell script (".sh").

To start, open a qlogin session on cluster-dev.

#### Base script: Test-MACRO-Emerge-qsub.R
Try executing this from the command line using `r_dock < Test-MACRO-Emerge-qsub.R`, or from inside an RStudio session.  Note that you will need to use the containerized version of R to get the MASH library to load (or, whichever version of R you used to build the MASH library). 

This script is adapted from Sean's Test-MACRO-Emerge.R script, which sets up and populates 10 patches and runs a MACRO simulation of malaria transmission for a year.  Calling this script from the command line will load the MASH library, then run an example simulation and save the output (`pfsiHist`) to the Test-MACRO-Emerge-outputs directory.  

To check that the output is correct, you can use the following to try printing out the disease status time series of each person in the population:

`ph <- readRDS("Test-MACRO-Emerge-outputs/pfsiHist_42.rds")`

`plot_PfSI(ph)`

#### Shells script: mash_shell_script.sh
This is adapted from IHME's cluster training - it tells the Unix shell to use R (using an alias `r_dock` to invoke the containerized version of R that we use with MASH) to execute a particular script (`<$1 --no-save`) with certain arguments (`$@`).

#### Single job submission: single_thread_qsub.R
Try executing this from an RStudio session, or from the command line.

This script constructs and executes a command-line job submission version of Test-MACRO-Emerge-qsub.R.  The qsub command includes:

* a job name
* some locations to store output and errors created by R
* a shell script to execute
* an R script to pass to the shell script
* arguments to pass to the R script from the shell script

#### Multiple job submission multi_thread_qsub.R
Try executing this from an RStudio session, or from the command line.

This script merely loops over multiple seeds, and essentially calls the same code as in single_thread_qsub.R in order to construct and execute multiple qsub commands.

In practice, when you want to create a very large number of qsub jobs, you probably instead want to submit an array job.
