## MASH (Modular Analysis and Simulation for Human Health - Mosquito-borne Pathogen Transmission)

### Overview of MASH

* This repository contains the most current stable version of the `MASH` project. This repository is a package for the R programming language, and can be installed by the R command: `devtools::install_github(repo = "smitdave/MASH")`. 
* `MASH` is being reprogrammed in R6 OOP style for greater flexibility of the code, better representation of the authors' vision in code, more durable codebase, true modularity, and more logical opportunities for parallelism.
* The R6 class framework can be found here: https://github.com/wch/R6
* `MASH` currently has a placeholder docs website at: https://smitdave.github.io/MASH/index.html which is being built with `pkgdown`.
* For more information on the `MASH` project please see: http://smitdave.github.io/MASH-Development/index.html
* For bug reports please open an issue on this GitHub repository by clicking on the "Issues" tab at the top of the page and then "Create an Issue". If possible please provide a detailed explanation of the circumstances under which the bug was encountered.

### System Requirements

  1. C++11 compiler support
    * For Mac OS systems installing xcode command line tools includes a C++11 compatible compiler, please install command line tools if you are having trouble compiling MASH
  2. GNU make support.
  3. For certain parallel algorithms, users need to be on a Linux or Mac OS system that supports forking (see: https://en.wikipedia.org/wiki/Fork–exec).

### Code Optimization

* There is a project to use `RcppR6` to export `C++` classes and methods to the `R6` framework to optimize bits of the MASH codebase most in need, and can be found here: https://slwu89.github.io/RcppQueues/
