---
title: Documentation
last_updated: July 16, 2016
summary: "Following MASH's modularity principles the repository is divided into sub-folders for each one of the components of the simulation."
sidebar: mydoc_sidebar
permalink: docStructure.html
folder: mydoc
---


## Archive

Mostly depreceated code.

## Documents

Some helpfull documents that explain the basics of MASH.

## HUMAN

Routines related to human characteristics and behaviour.

### ACTIVITY.SPACE

#### MICRO

### BEHAVIOR

#### PROTECTIVE.CLOTHING

#### SWATTING

### MASS.TREAT

### VACCINES

## LANDSCAPE

Defines the setup of the environment in which the simulation will run. In the case of mosquito-borne diseases the sugar sources, feeding sites, oviposition spots and mating landmarks are defined here.

### MACRO

Not implemented yet.

### MICRO

High resolution environments for detailed analysis of interactions.

### Micro2Macro

### PDG

Not implemented yet.

### PLACES

Not implemented yet.

## Lessons

## MOSQUITO

### AQUATIC.ECOLOGY

Variations of the routines needed for aquatic mosquito stages.

* emerge: mosquitos emerge at a constant rate from oviposition spots (with seasonality variance)
* EL4P: mosquitos eggs are laid and develop as they would expect in reality through four larval instars and pupa stage (needs to be fixed)

### CHOOSE.HOST

Routines that define which host will be bitten by each mosquito.

### FEEDING.CYCLE

#### DHM

Contains all the main routines for the full mosquito life-cycle

* setup.DHM.R: contains the parameters necessary to change mosquito's behaviours in the simulation
* test.DHM.R: temporarily the main routine to test DHM functions

#### DHM-Basic

#### RM

### SEARCH

Routines that deal with the movement of mosquitos through the landscape.

### VECTOR.CONTROL

Implementation of several vector control interventions.
*setup.VectorControl.R* contains the main functions and switches for the interventions to be activated. Each folder contains the implementation of the routines for the control measure.

{% highlight r %}

#Basic random uniform coverage example
setup.Random_Uniform_LANDSCAPE_BasedInterventions(IRS_Coverage = .5, LARVICIDING_Coverage = .5)

{% endhighlight %}

## PATHOGEN

### Generic

### Plasmodium.falciparum

## Simulations

Stores the main MASH simulations routines.

### Default

### MosquitoDefault

## Utilities

Assortment of various routines for auxiliary tasks.

### AnalysisRoutines

Contains some preliminary analysis routines for experiments performed in MASH. They are currently implemented in *Mathematica* but most of them can easily be translated to R.

Some of the notebooks require <a href="https://bitbucket.org/chipdelmal/pajarolocopublic">"PajaroLoco"</a> (networks). HMSC coded that package and it is publicly available so it is not a problem using or maintaining it.

Open the individual notebooks for a description on what they were created for and please remember to *Cell/Delete All Output* to avoid repository bloating.

### sinHaz
