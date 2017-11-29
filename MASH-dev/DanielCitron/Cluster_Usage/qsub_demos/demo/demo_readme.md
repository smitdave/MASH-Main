### Cluster job submission demo

#### - Daniel Citron, 11/28/2017

In this directory you will find three R scripts and one shell script (".sh").

To start, open a qlogin session on cluster-dev.

#### Base script: foo.R
Try executing this from the command line using `r < foo.R 3`, or from inside an RStudio session.

This script is very boring - it prints out the command line arguments passed to it, then selects one of them ("3" in the example above) and saves it in a variable ("seed") if it looks like an integer.  The output of the script is to create a blank file, subscripted with the variable seed.

#### Shells script: r_shell.sh
This is adapted from IHME's cluster training - it tells the Unix shell to use R (`/usr/local/R-current/bin/R`) to execute a particular script (`<$1 --no-save`) with certain arguments (`$@`).

#### Single job submission: boo.R
Try executing this from an RStudio session, or from the command line.

This script constructs and executes a command-line job submission version of foo.R.  The qsub command includes:

* a job name
* some locations to store output and errors created by R
* a shell script to execute
* an R script to pass to the shell script
* arguments to pass to the R script from the shell script

#### Multiple job submission coo.R
Try executing this from an RStudio session, or from the command line.

This script merely loops over multiple seeds, and essentially calls the same code as in boo.R in order to construct and execute multiple qsub commands.

In practice, when you want to create a very large number of qsub jobs, you probably instead want to submit an array job.
