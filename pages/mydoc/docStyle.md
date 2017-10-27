---
title: Style
sidebar: mydoc_sidebar
permalink: docStyle.html
toc: false
folder: mydoc
summary: The following is a set of guidelines for development of new algorithms for MASH.   
---

## Naming Conventions 

MASHHOME is the root directory for the MASH Code and documentation. 

All names of directories, files, and functions should be descriptive. The directory structure should reflect the hierarchical structure of the algorithms and their execution, insofar as that is possible.  

A COMPONENT is either: 

1. a slot for a Module
2. it should contain a set of nested COMPONENTs 

A directory name is all in caps if it is a COMPONENT.

A Module is a set of code that produces output for a COMPONENT. Each Module is contained in its own sub-directory. Usually, only the first letter is capitalized for a directory name if it is a Module (e.g. Rectangle). The convention if it borrows from another convention (e.g. Plasmodium.falciparum is a COMPONENT and PfSI one of its Modules). 

Many functions or objects that are part of a Module have a corresponding set of functions in another Module. Function names should have a descriptive name followed by a Module-specific suffix moniker: Modulename.suffix. For example, *infectHumanPf.SI* and *infectHumanPf.LOME* are parallel functions that initiate Plasmodium falciparum infections.  

An exquisitely detailed Module is often a set of many different functions and options that are tunable. Each Module of this type can / should be named with a sensible acronymn (e.g. DHM = Distributed Hazards Model).

Each Module should have a file called setup.Modulename.R that sets the options for each file. 

None of the directories in HOME has a Module. Instead, the directories in Home contain other necessary or useful supporting documents or functions.  

## Commenting and Documentation 

Every function should have a comment describing:

1. what it does
2. who wrote it
3. who modified it 


MASH is designed to be plug-n-play, so each COMPONENT is
defined by an API.  The four main COMPONENT 





