---
title: Vector Control
sidebar: mydoc_sidebar
permalink: docVectorControl.html
folder: mydoc
toc: true
summary: "Description of vector cotrol functions, parameters and how to use them in the simulation."
---

##  Description
"MOSQUITO/VECTOR.CONTROL/" contains all the sub-folders and files required to setup and activate the vector control interventions. These interventions are activated and distributed in "MOSQUITO/VECTOR.CONTROL/setup.VectorControl.R". 

There are three steps to setup an intervention in a particular landscape:

1. Activate its bool controller in "setup.VectorControl.R"
2. Call the <i>setup.VectorControlPackage</i> function. This function is a wrapper that defines probability parameters for the interventions that have been implemented in MASH. The probabilities vectors can be modified for each intervention by setting up the following values (note that not all of these parameters are required or used in each intervention and are kept only for standarisation purposes):
    * ACTIVE: defines in an intervention should or should not be activated in the simulation
    * SurviveP: probability to survive an encounter with a given intervention
    * ProbeP: probability to probe on a host (if available)
    * RepelP: probability of being repelled by an intervention if encountered
    * FeedP: probability to succesfully feed on a host given the encounter with the intervention
    * SurviveProbeP: probability to survive a probe attempt given the encounter with the intervention
3. Call either <i>setup.Manually_LANDSCAPE_BasedInterventions</i> or <i>setup.Random_Uniform_LANDSCAPE_BasedInterventions</i> to define where, in the LANDSCAPE object, the interventions should be distributed.
4. INTERVENTIONS_PARAMETERS must be defined as a global as it is the variable that controls the killing probabilities of all the interventions.

These interventions work by adding columns to the LANDSCAPE data frame in which booleans values (0 or 1) define wether an intervention is present in a given point or not. The "MOSQUITO/VECTOR.CONTROL/setup.VectorControl.R" contains examples of how to use these functions.

##  Interventions 
The ones marked with (+) are the ones that have been integrated into the newest version of MASH, the ones marked with (-) were integrated but might be wrong (they depend on a weird check: <i>notInIXFZero=ixf(M)</i>) and the ones marked with (=) were integrated but untested.

<embed src="http://smitdave.github.io/MASH-Development/images/VectorControlDiagram.png" width="100%">

### ATSB (-)
{% highlight r %}
* Name: Attractive sugar baits
* Description: Poisoned sugar sources to kill mosquitoes trying to feed.
* Located in: sugar feeding sites (s)
* DHM Stage: S
* Called by: SugarFeedingBout
{% endhighlight %}

### AREA.REPEL (-)
{% highlight r %}
* Name: Area repellents 
* Description: Mosquitoes get spatially repelled by measures like citronella coils.
* Located in: haunts (f)
* DHM Stage: F
* Called by: landingSpot
{% endhighlight %}

### BAITED.TRAP (-)
{% highlight r %}
* Name: Odor baits 
* Description: Traps intended to draw mosquitoes trying to blood-feed by using human odor as bait.
* Located in: haunts (f)
* DHM Stage: F
* Called by: BloodFeedingSearchBout
{% endhighlight %}

### BIOLOGICAL.CONTROL
{% highlight r %}
* Name: Larvivorous fish
* Description:
* Located in:
* DHM Stage: 
{% endhighlight %}

### FOUL.HABITAT 
{% highlight r %}
* Name: Habitat not able for development
* Description:
* Located in:
* DHM Stage: 
{% endhighlight %}

### EAVE.TUBE (+)
{% highlight r %}
* Name: Eave tubes
* Description: House modifications that repel or kill mosquitoes trying to enter a house. 
* Located in: haunts (f)
* DHM Stage: F
* Called by: enterHouse
{% endhighlight %}

### GM
{% highlight r %}
* Name: Genetically modified mosquitoes
* Description:
* Located in:
* DHM Stage: 
{% endhighlight %}

### IRS (-)
{% highlight r %}
* Name: Indoor residual spraying
* Description: Indoor spraying of insecticide to kill mosquitoes who rest on walls.
* Located in: haunts (f)
* DHM Stage: R
* Called by: landingSpot
{% endhighlight %}

Note: For now inside and outside walls are treated the same (as in they call the same set of parameters). That should be changed later.

### ITN
{% highlight r %}
* Name: Insecticide-treated nets
* Description:
* Located in:
* DHM Stage: B 
* Called by: humanEncounter
{% endhighlight %}

### SWAT
{% highlight r %}
* Name: Human swatting
* Description:
* Located in:
* DHM Stage: 
{% endhighlight %}

### IVERMECTIN
{% highlight r %}
* Name: Ivermectin
* Description:
* Located in:
* DHM Stage: 
{% endhighlight %}

### LARVICIDES
{% highlight r %}
* Name: 
* Description:
* Located in:
* DHM Stage: 
{% endhighlight %}

### LARVICIDING
{% highlight r %}
* Name: Poisoning larvae
* Description:
* Located in:
* DHM Stage: 
{% endhighlight %}

### IMPROVE.HOME
{% highlight r %}
* Name: House modifications
* Description: House are modified to prevent mosquitoes from entering.
* Located in: haunts (f)
* DHM Stage: F
* Called by: landingSpot
{% endhighlight %}

### PROTECT
{% highlight r %}
* Name:
* Description:
* Located in:
* DHM Stage:
{% endhighlight %}

### REPEL
{% highlight r %}
* Name:
* Description:
* Located in:
* DHM Stage:
{% endhighlight %}

### PERSONAL.REPELLANT
{% highlight r %}
* Name:
* Description:
* Located in:
* DHM Stage:
{% endhighlight %}

### OVITRAP
{% highlight r %}
* Name: Traps for eggs
* Description:
* Located in:
* DHM Stage:
{% endhighlight %}

### SOURCE.REDUCTION
{% highlight r %}
* Name: Destruction of aquatic habitats
* Description:
* Located in:
* DHM Stage:
{% endhighlight %}

### SWARM.SPRAYING (=)
{% highlight r %}
* Name: Insecticide spraying of mating swarms 
* Description: Spraying insecticide on located swarms to kill potential mates.
* Located in: mating spots (m)
* DHM Stage: M
* Called by: SwarmingBout
{% endhighlight %}

### ZOOSPRAY 
{% highlight r %}
* Name:
* Description:
* Located in:
* DHM Stage:
{% endhighlight %}
