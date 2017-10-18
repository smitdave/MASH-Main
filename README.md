# MASH-Main (Modular Analysis & Simulation for Human Health)

 subdirs contain various elements of the MASH project.
 
## MASH-CPP: MASHcpp package
* Please install with `devtools::install_github(repo = "smitdave/MASH-Main",subdir = "MASH-CPP")`
* This repository stores `C++` classes exposed to `R` through the `RcppR6` package.
* This repository also implements as `R6` hash table style container for storing populations through `R`'s native `environment` data structure (somewhat analgous to `std::unordered_map`). Hash tables allow O(1) lookup time and cheap deletion/insertion of elements because they are not stored in contiguous memory; their disadvantage is slow iteration through elements when compared to data structures in contiguous memory. Because mosquitoes and humans may need to move between patches, and in the case of mosquitoes, frequently die and need to be replaced, a hash table can provide performance optimizations over `R`'s `list` objects, which frequently involve large amounts of copying.

## MASH-Docs: Documentation
 * This directory is not set up as an R package and contains .tex and .Rmd documents describing functionality of MASH.

## MASH-GUI: MASHgui package
* Please install with `devtools::install_github(repo = "smitdave/MASH-Main",subdir = "MASH-GUI")`
* This repository contains necessary code to run R Shiny GUIs for parameterizing and visualizing MASH.

## MASH-MACRO: MASHmacro package
* Please install with `devtools::install_github(repo = "smitdave/MASH-Main",subdir = "MASH-MACRO")`
* This repository contains necessary classes and functions to run macrosimulation tiles.

## MASH-MICRO: MASHmicro package
* Please install with `devtools::install_github(repo = "smitdave/MASH-Main",subdir = "MASH-MICRO")`
* This repository contains necessary classes and functions to run microsimulation tiles.

## MASH-Prototype: MASHprototype package
 * Please install with `devtools::install_github(repo = "smitdave/MASH-Main",subdir = "MASH-Prototype")`
 * This repository is intended for serious prototyping of unstable code and not intended for general users.
 
 ## MASH-Test: Testing Scripts
  * This directory is not set up as an R package and is solely intended to hold test scripts and plan package vignettes. 
