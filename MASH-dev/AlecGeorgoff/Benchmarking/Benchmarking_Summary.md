## MASH MACRO Benchmarking Summary
##### Alec Georgoff
#### Background
The purpose of this testing was to determine how the time and memory demands of the `TEST-MACRO.R` script vary with varying parameters, such as _n\_humans_, _n\_patches_, and _tMax_. Determining how these attributes scale with changing parameters is key to understanding what time and computing resources will be needed to run MACRO at larger scales.

#### Methods
_Note: all files used in this testing can be found [here](https://github.com/smitdave/MASH-Main/tree/master/MASH-dev/AlecGeorgoff/Benchmarking)_.

##### Modifying script
The testing used a modified version of `TEST-MACRO.R`, titled `TEST-MACRO-BENCHMARK.R`, which has been modified so that its arguments can be expressed from the command line when the script is called.

##### Submitting jobs on the cluster
IHME's cluster computing environment was used for this testing, on the "production" cluster. In order to submit jobs, a qsub command needed to be issued: `qsub -cwd -N [Job Name] -pe multi_slot 1 -l mem_free=2G -hold_jid no_holds mash_shell_script.sh TEST-MACRO-BENCHMARK.R`.

In order to submit multiple jobs in succession without typing out multiple qsub commands, a script titled `qsub_TEST_multi_loop.R` was created. This script accepts inputs for the varying parameters and submits a qsub statement for each combination of parameters.

##### Retrieving qacct information
IHME's cluster has a command, `qacct [Job ID]`, which returns information about jobs that have finished running. A script titled `qacct_retrieval.R` was created to retrieve this information. This script requires the job_id of each job that was submitted using the `qsub_TEST_multi_loop.R` script; therefore, it is critical that the job_ids are recorded at the time of job submission.

This script returns the following parameters in a .csv file:
* _ru\_wallclock_ - computing time from start to finish of job (in seconds)
* _ru\_maxrss_ - maximum residential set size (in kB)

##### Graphing results
To graph the benchmarking results, a script titled `create_BM_graphs.R` was written. This script reads the .csv file from the `qacct` retrieval and creates graphs to display how runtime and memory usage vary with the varying parameter of the specific job. The data are displayed on a log scale.

##### Parameters
As discussed in the Background section, this testing involved running a simulation multiple times while only varying one parameter between runs; this was done to isolate the effect of that parameter on time and memory usage. The following charts detail the varying of certain parameters:

###### _**Vary over tMax (500 Humans)**_
![Vary over tMax, 500 Humans](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_outline/vary_tmax_nh_500.PNG?raw=true)

###### _**Vary over tMax (5000 Humans)**_
![Vary over tMax, 5000 Humans](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_outline/vary_tmax_nh_5000.PNG?raw=true)

###### _**Vary over n\_patches**_
![Vary over n\_patches](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_outline/vary_n_patches.PNG?raw=true)

###### _**Vary over n\_patches, keep humans per patch constant**_
![Vary over n\_patches, keep humans per patch constant](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_outline/vary_n_patches_const_ratio.PNG?raw=true)

###### _**Vary over n\_humans (A)**_
![Vary over n\_humans (A)](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_outline/vary_n_humans_A.PNG?raw=true)

###### _**Vary over n\_humans (B)**_
![Vary over n\_humans (B)](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_outline/vary_n_humans_B.PNG?raw=true)

#### Results

##### _**Vary over tMax (500 Humans)**_
![Vary over tMax, 500 Humans, data](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_tMax_nh_500_DATA.PNG?raw=true)

![Vary over tMax, 500 Humans, runtime](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_tMax_nh_500_RUNTIME.png?raw=true)

![Vary over tMax, 500 Humans, memory](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_tMax_nh_500_MEMORY.png?raw=true)

##### _**Vary over tMax (5000 Humans)**_
![Vary over tMax, 5000 Humans, data](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_tMax_nh_5000_DATA.PNG?raw=true)

![Vary over tMax, 5000 Humans, runtime](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_tMax_nh_5000_RUNTIME.png?raw=true)

![Vary over tMax, 5000 Humans, runtime](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_tMax_nh_5000_MEMORY.png?raw=true)

##### _**Vary over n\_patches**_
![Vary over n\_patches, data](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_patches_DATA.PNG?raw=true)

![Vary over n\_patches, runtime](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_patches_RUNTIME.png?raw=true)

![Vary over n\_patches, memory](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_patches_MEMORY.png?raw=true)

##### _**Vary over n\_patches, keep humans per patch constant**_
![Vary over n\_patches, keep humans per patch constant, data](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_patches_const_ratio_DATA.PNG?raw=true)

![Vary over n\_patches, keep humans per patch constant, runtime](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_patches_const_ratio_RUNTIME.png?raw=true)

![Vary over n\_patches, keep humans per patch constant, memory](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_patches_const_ratio_MEMORY.png?raw=true)

##### _**Vary over n\_humans (A)**_
![Vary over n\_humans (A), data](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_humans_A_DATA.PNG?raw=true)

![Vary over n\_humans (A), runtime](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_humans_A_RUNTIME.png?raw=true)

![Vary over n\_humans (A), memory](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_humans_A_MEMORY.png?raw=true)

##### _**Vary over n\_humans (B)**_
![Vary over n\_humans (B), data](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_humans_B_DATA.PNG?raw=true)

![Vary over n\_humans (B), runtime](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_humans_B_RUNTIME.png?raw=true)

![Vary over n\_humans (B), memory](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_humans_B_MEMORY.png?raw=true)

#### Conclusions

_[In Progress]_