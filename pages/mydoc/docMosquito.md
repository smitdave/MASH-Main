---
title: Mosquito Documentation
sidebar: mydoc_sidebar
permalink: docMosquito.html
folder: mydoc
toc: true
summary: "Description of the variables that make up mosquitos in the model."
---
# Female Mosquito Object

## Mosquito Variables
*   id: identifier
*   bday: birthday
*   tnow: current event time
*   tnext: time to next event
*   bState: life cycle event 
*   bStateNew: life cycle event 
*   inPointSet: type of site in which the mosquito is currently located
*   ix: location index
*   mature: 
*   type: mosquito type
*   lspot: type of site in which the mosquito is currently located
*   damage: damage sustained by the individual (0 to 1)
*   energy: energy reserves (0 to 1)
*   female: male or female individual
*   bmSize: size of blood meal (0 to 1)
*   batch: female eggs in batch
*   eggT: minimum time before eggs are mature
*   eggP: minimum provision (in ) for eggs to mature
*   mated: has mosquito mated in this gonotrophic cycle?
*   sire:
*   sire.type:
*   energyPreG: If eDefPreG > 0, it defines a pre-gonogrophic energy requirement that could be filled up.
*   hostID: identifier of bitten individuals
*   Pf: plasmodium falsciparum 

## Mosquito Life Cycle States
*   F: blood-feed search
*   B: blood-feed attempt
*   R: rest
*   L: egg-laying search
*   O: egg-laying attempt
*   S: sugar-feed attempt
*   M: mating attempt
*   E: estivating
*   D: dead

## Point Sets
*   l: habitat/laying site (oviposition spot)
*   f: feeding site (haunt)
*   s: sugar feeding site
*   m: mating swarm site

## Landing Spots: House Entering & Resting Behavior
At the end of the search bout, attempt bout, or after egg laying, a mosquito has entered the area around a feeding station and either rested or attempted to rest:

*   l: leave the area
*   r: reattempt without resting
*   v: rest on vegetation
*   w: rest on the outside wall of a structure
*   i: rest on the inside wall of a structure

## Female DHM

{% highlight r %}
while(M$bState!="D" & M$tnext < env$TMAX){

    #update mosquito M's time
    M$tnow = M$tnext 

    #run bout based on bState
    M = switch(M$bState,
        F = BloodFeedingSearchBout(M),
        B = BloodFeedingAttemptBout(M),
        R = RestingBout(M),
        L = EggLayingSearchBout(M),
        O = EggLayingAttemptBout(M),
        M = MatingBout(M),
        S = SugarFeedingBout(M),
        E = EstivationBout(M)
    )

    M = queueEstivation(M)
    M = queueMating(M)
    M = landingSpot(M)
    M = surviveRestingHazard(M)
    M = energetics(M)
    M = surviveFlight(M)
    M = timing(M)
    M$bState = M$bStateNew

}
{% endhighlight %}

# Male Mosquito Object

## Male DHM

{% highlight r %}
while(M$bState != "D" & M$tnext < env$TMAX){
    M$tnow = M$tnext
    M = switch(M$bState,
        M = SwarmingBout(M,env=env),
        S = SugarFeedingBout(M)
    )
    if((M$bStateNew == "F") | M$bStateNew == "B"){
        M$bStateNew = "S"
    } 

    M=queueMating(M)
    M=landingSpot(M) 
    M=surviveRestingHazard(M) 
    M=maleFlightStress(M,env=env)
    M=timing(M) 
    M$bState = M$bStateNew

}
{% endhighlight %}