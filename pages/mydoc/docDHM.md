---
title: M-BITES Documentation
sidebar: mydoc_sidebar
permalink: docDHM.html
folder: mydoc
toc: true
summary: "Description of the functions that define the mosquito life-cycle model."
---
# M-BITES: Mosquito Bout-based and Individual-based Transmission Ecology Simulation

## Female M-BITES

### Core Functions

Female M-BITES functions are largely defined in <code>MBITES-Bouts.R</code>, calling other source files as appropriate. Due to the number and complexity of the female life cycle model, and future development of new life stages or more detailed implementations of current work should define new <code>MBITES-MODULE.R</code> source files if differences with existing code are significant.

#### boutGeneric

Generic function to be called for every bout. The mechanism is described below.

1. Update time and state:
    * Upon entering a bout, a mosquito's tNow is updated to tNext. All computations within the bout (all survival, energetics, as well as the bout itself, boutFun), must only make reference to tNow as the main calling function <code style="color: #e60000;">MBITESi()</code>.
    * After updating tNow, state is updated to stateNew; all computations within the bout must assume the current state is "state"; stateNew will be modified during the bout. All functions within a bout should make appropriate use of <code style="color: #e60000;">isAlive()</code> and/or <code style="color: #e60000;">isActive()</code> so that if a mosquito's stateNew is set to dead during a function call it is non subsequently overwritten.
    * Finally, <code style="color: #e60000;">timingExponential()</code> will update tNext based on current state, "state". This is the <i>holding time</i> of the mosquito in this state until the next state transition (ie; we view the mosquito as a discrete-time Markov process with exponential holding times, which is equivalent to a continuous-time Markov process, but we do not directly specify the infentesimal generator matrix Q; this is why we update tNext regardless of the next state, because we are sampling the time spent in the current state).
2. Movement:
    * <code style="color: #e60000;">rMove()</code> returns a list of the new point set of the mosquito and the new site index she moved to. If new point sets are added, movement rules must be appropriately added to the function, found in <code>MOVEMENT/movement.R</code>
    *

{% highlight r %}
boutGeneric <- function(M,P,boutFun,...){

  # update time and state
  M$tNow = M$tNext # update time
  M$state = M$stateNew # update current state
  M = timingExponential(M,P) # update tNext

  # movement
  move = rMove(ix = M$ix,pSet = M$inPointSet,bState = M$state)
  M$ix = move$ixNew
  M$inPointSet = move$pSetNew

  # bout
  M = boutFun(M,P,...)

  # energetics
  M = energetics(M,P)

  # landing spot
  M = landingSpot(M,P)

  # survival
  M = surviveResting(M,P)
  M = surviveFlight(M,P)

  # check queueing
  # queueEstivation()
  # queueMating() only queue if mated = FALSE

  return(M)
}
{% endhighlight %}

## Male M-BITES

### Core Functions

Male-specific M-BITES functions are mostly defined in MBITES-Swarming.R because of the restricted number and complexity of the male life cycle model.

#### boutGenericMale

<code>TEST THE CODEZ</code>











<!-- # DHM Visualization

<center>{% include image.html file="dhmP_visualization.png" alt="" caption=""  max-width=300 %}</center>

# Female DHM

NOTE: The plots shown represent the state before (first row) and after (second row) the action has taken place.

## Sugar Feeding

### SugarFeedingBout

Mosquitoes enter this state when their energy reserves are low. When completed succesfully the sugar feedding routine returns a mosquito high higher energy level and in a new life-cycle state.

{% highlight r %}
# Mosquito variables that are updated
M$bStateNew = {"F", "S", "D"}
M$energy = 1
{% endhighlight %}

<center>{% include image.html file="DHMSugarFeeding.png" alt="" caption=""  max-width=300 %}</center>

## Mating

### MatingBout
Not working!!!!!
{% highlight r %}
# Mosquito variables that are updated
M$bStateNew = {"F","M","D"}
{% endhighlight %}


## Estivating

### EstivationBout

Note that survival through estivation is determined in the queue estivation function and if a mosquito would not have survived, it is immediately killed. The "estivation bout" simply changes bState to "F".

{% highlight r %}
# Mosquito variables that are updated
M$bStateNew = "F"
{% endhighlight %}

<center>{% include image.html file="DHMEstivation.png" alt="" caption=""  max-width=300 %}</center>

## Blood Feeding

### probing
Mosquitos try to bite hosts and update their infection status.

#### updateMosquitoInfectionStatus
Currently not implemented.

### BloodFeedingSearchBout
Mosquitos in this state perform movement actions in search of hosts to feed upon.

{% highlight r %}
# Mosquito variables that are updated
M$bStateNew = {"B" or same as input if lspot=="l"}
M$inPointSet="f"
{% endhighlight %}

<center>{% include image.html file="DHMBloodSearch.png" alt="" caption=""  max-width=300 %}</center>

### BloodFeedingAttemptBout
Routines involved with the probing and feeding on a host.

{% highlight r %}
# Mosquito variables that are updated
M$bStateNew = {"D", "R"}
M$bmSize = {0,1}
M$inPointSet="f"
{% endhighlight %}

<center>{% include image.html file="DHMBloodFeed.png" alt="" caption=""  max-width=300 %}</center>

#### chooseHost

#### nullHost
Returns the mosquito unchanged.
{% highlight r %}
# Mosquito variables that are updated
M$bStateNew = {"R","D"}
{% endhighlight %}

#### humanEncounter
Routine that deals with the case in which a mosquito chooses a human host. Some mosquito control interventions take place in this function (ITN, REPEL, PROTECT) and some human actions can kill them (SWAT)
{% highlight r %}
# Mosquito variables that are updated
M$bStateNew = {"R","D"}
{% endhighlight %}

#### livestockEncounter
Routine that is called whenever a mosquito chooses livestock as its host.

{% highlight r %}
# Mosquito variables that are updated
M$bStateNew = {"R"}
{% endhighlight %}

<center>{% include image.html file="DHMLivestockEncounter.png" alt="" caption=""  max-width=300 %}</center>

#### getInfected
Currently not implemented.



## Resting

### RestingBout
Mosquitoes enter this stage after blood feeding. They can either die, re-feed or enter laying stage.

{% highlight r %}
# Mosquito variables that are updated
M$bStateNew = {"B","L","D"}
{% endhighlight %}

### myHaz
Returns the hazard level of the mosquito according to the type of place it is located at.
{% highlight r %}
# Returns
float
{% endhighlight %}

### surviveRestingHazard
This function is called in the resting bout and contains the death probability due to IRS and environmental hazards.
{% highlight r %}
# Mosquito variables that are updated
M$bStateNew = {"D" or same as input}
{% endhighlight %}


## Egg-Laying

### EggLayingSearchBout
Mosquitoes search for a place to lay eggs in this stage.

{% highlight r %}
# Mosquito variables that are updated
M$inPointSet = "l"
mosquitoOut$bStateNew = {"L","O"}
{% endhighlight %}


### EggLayingAttemptBout
Female mosquitos try to oviposit in this routine.

{% highlight r %}
# Mosquito variables that are updated
M$bStateNew = {"O","F","D"}
M$batch = {0,1}
{% endhighlight %}

#### layEggs
Performs the egg laying routine actions. Makes a batch and lays eggs.
Does not currently update LANDSCAPE object.
{% highlight r %}
# Returns value
{i=1,w=2,v=3,r=4,l=5}
{% endhighlight %}

##### makeBatches
Don't quite follow the logic but runs.
{% highlight r %}
# Modified values
EggQ
{% endhighlight %}

## Auxiliaries

### lspotix
Converts the landing spot action char identifier into an integer (l: leave, r: reattempt withour resting, v: rest on vegetation, w: rest on outside wall, i: rest on inside wall).
{% highlight r %}
# Returns value
{i=1,w=2,v=3,r=4,l=5}
{% endhighlight %}

### myLocType
Returns the type of the location the mosquito is on (according to ix).
NOTE: {0: non peri-domestic?, 1: peri-domestic?}
{% highlight r %}
# Returns value
LANDSCAPE${f,l,m,s}$type[M$ix]
{% endhighlight %}

### ixf
Returns the index of the haunt/feeding site associated with the peri-domestic habitat/laying site, sugar site, or mating site that the mosquito is in. If the mosquito is already in a feeding site, it just returns the mosquito's current position (M$ix).

### getWTS
Returns the list of search weights accordint to the mosquito's bStateNew. Vector of probabilities (l: leave, r: reattempt withour resting, v: rest on vegetation, w: rest on outside wall, i: rest on inside wall).
{% highlight r %}
# Returns value
{Fwts,Rwts,Lwts,Owts,Mwts,Swts}
{% endhighlight %}

### landingSpot
Assigns the new landing spot of the mosquito. If the mosquito is in a feeding site or a peri-domestic sugar, mating, or laying site, it chooses from l,r,v,w,i as its new landing spot based on search weights and InAndOut matrix. If not, it always lands on v, vegetation. Applies the enter house, area repel and IRS interventions.
{% highlight r %}
# Mosquito variables that are updated
M$bStateNew = {"D" or initial state}
# if the mosquito's current site is not peri-domestic
M$lspot = {"v"}
# if the mosquito in a feeding site or peri-domestic site
M$lspot = {"i","w","v","r","l"}
{% endhighlight %}

### enterHouse
Performs routines needed for mosquitoes to enter houses to blood-feed. Flight survival and Eave Tubes actions take place here.
{% highlight r %}
# Mosquito variables that are updated
M$lspot
{% endhighlight %}

### newSpot
Returns a new type of spot in which the mosquito is located. Calculates the probabilities using getWTS.
{% highlight r %}
# Returns value
{"i","w","v","r","l"}
{% endhighlight %}

### isAlive
Returns 1 if the mosquito is alive and 0 if it is dead.
{% highlight r %}
# Returns value
{0: dead, 1: alive}
{% endhighlight %}

### isActive
Returns 1 if the mosquito is active (not estivating) and alive and 0 otherwise.
{% highlight r %}
# Returns value
{0: estivating/dead, 1: active}
{% endhighlight %}


## DHM Functions Tree

<center>
{% include image.html file="functionsTree.png" alt="Networks are Awesome!" caption=""  max-width=1000 %}
</center>

# Male DHM

## SwarmingBout
Swarming behaviour in male mosquitos. Swarm sparying intervention takes place here.

{% highlight r %}
# Mosquito variables that are updated
M$bStateNext = {"M","S"}
{% endhighlight %}

### enterSwarm -->
