## MASH MACRO Benchmarking Summary
##### Alec Georgoff
### Background
The purpose of this testing was to determine how the time and memory demands of the `TEST-MACRO.R` script vary with varying parameters, such as _n\_humans_, _n\_patches_, and _tMax_. Determining how these attributes scale with changing parameters is key to understanding what time and computing resources will be needed to run MACRO at larger scales.

### Methods
_Notes:_

_The original `TEST-MACRO.R` file can be found [here](https://github.com/smitdave/MASH-Main/tree/master/MASH-Test)_.

_All custom files used in this testing can be found [here](https://github.com/smitdave/MASH-Main/tree/master/MASH-dev/AlecGeorgoff/Benchmarking)_.

_Testing was conducting using a verion of MASH built on 12/7/17. Subsequent versions of MASH are not guaranteed to produce the same results_.

#### Modifying script
The testing used a modified version of `TEST-MACRO.R`, titled `TEST-MACRO-BENCHMARK.R`, which has been modified so that its arguments can be expressed from the command line when the script is called.

#### Submitting jobs on the cluster
IHME's cluster computing environment was used for this testing, on the "production" cluster. In order to submit jobs, a qsub command needed to be issued: `qsub -cwd -N [Job Name] -pe multi_slot 1 -l mem_free=2G -hold_jid no_holds mash_shell_script.sh TEST-MACRO-BENCHMARK.R`.

In order to submit multiple jobs in succession without typing out multiple qsub commands, a script titled `qsub_TEST_multi_loop.R` was created. This script accepts inputs for the varying parameters and submits a qsub statement for each combination of parameters.

#### Retrieving qacct information
IHME's cluster has a command, `qacct [Job ID]`, which returns information about jobs that have finished running. A script titled `qacct_retrieval.R` was created to retrieve this information. This script requires the job_id of each job that was submitted using the `qsub_TEST_multi_loop.R` script; therefore, it is critical that the job_ids are recorded at the time of job submission.

This script returns the following parameters in a .csv file:
* _ru\_wallclock_ - computing time from start to finish of job (in seconds)
* _ru\_maxrss_ - maximum residential set size (in kB)

#### Graphing results
To graph the benchmarking results, a script titled `create_BM_graphs.R` was written. This script reads the .csv file from the `qacct` retrieval and creates graphs to display how runtime and memory usage vary with the varying parameter of the specific job. The data are displayed on a log scale.

#### Parameters
As discussed in the Background section, this testing involved running a simulation multiple times while only varying one parameter between runs; this was done to isolate the effect of that parameter on time and memory usage. The following charts detail the varying of certain parameters:

##### _**Vary over tMax (500 Humans)**_

n\_humans | n\_patches | tMax | slots
:---: | :---: | :---: | :---:
500 | 10 | **100** | 2
500 | 10 | **300** | 2
500 | 10 | **1000** | 2
500 | 10 | **3000** | 2
500 | 10 | **10000** | 2

##### _**Vary over tMax (5000 Humans)**_

n\_humans | n\_patches | tMax | slots
:---: | :---: | :---: | :---:
5000 | 10 | **100** | 2
5000 | 10 | **300** | 2
5000 | 10 | **1000** | 2
5000 | 10 | **3000** | 2
5000 | 10 | **10000** | 2

##### _**Vary over n\_patches**_

n\_humans | n\_patches | tMax | slots
:---: | :---: | :---: | :---:
50000 | **10** | 365 | 2
50000 | **100** | 365 | 2
50000 | **1000** | 365 | 2
50000 | **100000** | 365 | 2

##### _**Vary over n\_patches, keep humans per patch constant**_

n\_humans | n\_patches | tMax | slots
:---: | :---: | :---: | :---:
**100** | **10** | 365 | 2
**300** | **30** | 365 | 2
**1000** | **100** | 365 | 2
**3000** | **300** | 365 | 2
**10000** | **1000** | 365 | 2
**100000** | **3000** | 365 | 2

##### _**Vary over n\_humans (A)**_

n\_humans | n\_patches | tMax | slots
:---: | :---: | :---: | :---:
**1000** | 10 | 365 | 2
**3000** | 10 | 365 | 2
**10000** | 10 | 365 | 2
**30000** | 10 | 365 | 2
**100000** | 10 | 365 | 2
**200000** | 10 | 365 | 2
**300000** | 10 | 365 | 2

##### _**Vary over n\_humans (B)**_

n\_humans | n\_patches | tMax | slots
:---: | :---: | :---: | :---:
**1000** | 10 | 365 | 2
**3000** | 10 | 365 | 2
**10000** | 10 | 365 | 2
**30000** | 10 | 365 | 2
**100000** | 10 | 365 | **8**
**200000** | 10 | 365 | **8**
**300000** | 10 | 365 | **8**

### Results
_A standard linear regression fit is added to each graph to show linearity or lack thereof_

##### _**Vary over tMax (500 Humans)**_

n\_humans | n\_patches | tMax | slots | runtime (min) | memory (MB)
:---: | :---: | :---: | :---: | :---: | :---:
500 | 10 | 100 | 2 | 0.5157 | 95.63
500 | 10 | 300 | 2 | 1.440 | 98.40
500 | 10 | 1000 | 2 | 4.588 | 102.2
500 | 10 | 3000 | 2 | 13.57 | 104.0
500 | 10 | 10000 | 2 | 27.25 | 107.2

![Vary over tMax, 500 Humans, runtime](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_tMax_nh_500_RUNTIME.png?raw=true)

![Vary over tMax, 500 Humans, memory](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_tMax_nh_500_MEMORY.png?raw=true)

##### _**Vary over tMax (5000 Humans)**_

n\_humans | n\_patches | tMax | slots | runtime (hr) | memory (MB)
:---: | :---: | :---: | :---: | :---: | :---:
5000 | 10 | 100 | 2 | 0.05285 | 318.3
5000 | 10 | 300 | 2 | 0.1032 | 308.1
5000 | 10 | 1000 | 2 | 0.3899 | 308.8
5000 | 10 | 3000 | 2 | 1.193 | 311.3
5000 | 10 | 10000 | 2 | 3.506 | 316.6

![Vary over tMax, 5000 Humans, runtime](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_tMax_nh_5000_RUNTIME.png?raw=true)

![Vary over tMax, 5000 Humans, runtime](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_tMax_nh_5000_MEMORY.png?raw=true)

##### _**Vary over n\_patches**_

n\_humans | n\_patches | tMax | slots | runtime (hr) | memory (MB)
:---: | :---: | :---: | :---: | :---: | :---:
50000 | 10 | 365 | 2 | 2.177 | 2122
50000 | 100 | 365 | 2 | 2.511 | 2136
50000 | 1000 | 365 | 2 | 3.883 | 2223
50000 | 10000 | 365 | 2 | 6.856 | 6634

![Vary over n\_patches, runtime](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_patches_RUNTIME.png?raw=true)

![Vary over n\_patches, memory](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_patches_MEMORY.png?raw=true)

##### _**Vary over n\_patches, keep humans per patch constant**_

n\_humans | n\_patches | tMax | slots | runtime (min) | memory (MB)
:---: | :---: | :---: | :---: | :---: | :---:
100 | 10 | 365 | 2 | 0.5161 | 85.77
300 | 30 | 365 | 2 | 0.4976 | 103.1
1000 | 100 | 365 | 2 | 3.624 | 193.3
3000 | 300 | 365 | 2 | 14.45 | 298.3
10000 | 1000 | 365 | 2 | 38.63 | 575.3
30000 | 3000 | 365 | 2 | 175.9 | 2011

![Vary over n\_patches, keep humans per patch constant, runtime](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_patches_const_ratio_RUNTIME.png?raw=true)

![Vary over n\_patches, keep humans per patch constant, memory](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_patches_const_ratio_MEMORY.png?raw=true)

##### _**Vary over n\_humans (A)**_

n\_humans | n\_patches | tMax | slots | runtime (hr) | memory (MB)
:---: | :---: | :---: | :---: | :---: | :---:
1000 | 10 | 365 | 2 | 0.05036 | 133.6
3000 | 10 | 365 | 2 | 0.1709 | 214.4
10000 | 10 | 365 | 2 | 0.3911 | 519.6
30000 | 10 | 365 | 2 | 1.138 | 1238
100000 | 10 | 365 | 2 | 5.168 | 4365
200000 | 10 | 365 | 2 | 15.89 | 7523
300000 | 10 | 365 | 2 | 36.36 | 12940

![Vary over n\_humans (A), runtime](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_humans_A_RUNTIME.png?raw=true)

![Vary over n\_humans (A), memory](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_humans_A_MEMORY.png?raw=true)

##### _**Vary over n\_humans (B)**_

n\_humans | n\_patches | tMax | slots | runtime (hr) | memory (MB)
:---: | :---: | :---: | :---: | :---: | :---:
1000 | 10 | 365 | 2 | 0.02456 | 143.8
3000 | 10 | 365 | 2 | 0.0.7603 | 221.9
10000 | 10 | 365 | 2 | 0.2904 | 526.2
30000 | 10 | 365 | 2 | 0.9765 | 1244
100000 | 10 | 365 | 8 | 4.955 | 4373
200000 | 10 | 365 | 8 | 13.92 | 7529
300000 | 10 | 365 | 8 | 30.18 | 12968

![Vary over n\_humans (B), runtime](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_humans_B_RUNTIME.png?raw=true)

![Vary over n\_humans (B), memory](https://github.com/smitdave/MASH-Main/blob/master/MASH-dev/AlecGeorgoff/Benchmarking/BM_results/benchmarking_results_vary_n_humans_B_MEMORY.png?raw=true)

#### Conclusions

* Increasing _tMax_ causes a roughly linear increase in runtime, with little effect on memory usage
* Increasing _n\_patches_ increases runtime in a non-linear way; there is only an effect on memory usage after reaching 1000 patches
* Increasing _n\_patches_ while keeping the ratio of _n\_patches_ to _n\_humans_ constant results in a roughly linear increase in runtime, and a non-linear increase in memory usage
* Increasing _n\_humans_ results in a roughly linear increase in runtime, although this linearity seems to break down around 100000 humans
* Increasing _n\_humans_ results in a roughly linear increase in memory usage
* Adding 6 slots (for a total of 8) to the runs with >100000 humans provides some decrease in runtime, but only a maximum decrease of approximately 16%