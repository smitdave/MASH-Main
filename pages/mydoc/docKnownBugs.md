---
title: Known Bugs
sidebar: mydoc_sidebar
permalink: docKnownBugs.html
folder: mydoc
toc: false
summary: "'Any sufficiently advanced bug is indistinguishable from a feature'- Rich Kulawiec"
---
## Things to change later

1. zeroBatch() to zero out egg batches is not called when using the emerge module; this is because egg laying has no effect on emergence (lambda is directly calculated from the "season" vector from seasonally forced emergence). For large simulations this will mean the eggQ will become excessively large. We should log the data from the eggQ periodically and then zero it out. Ask Dave his thoughts on this.
2. In EL4Pt (the daily difference equations) it is possible for population sizes to become arbitrarily small (but still non-zero) due to the nature of the equations. This is a problem because each "cohort" begins as an egg batch and starts moving through the EL4Pt equations but never fully leaves. Besides the logical issues, this creates a massive memory problem as the aquaPops object to store all the EL4P populations will have to become larger +1 every time a new eggBatch is laid in that site. We have to do some rounding/truncation. Ask Dave his thoughts on this.

## Sean's to-do

1. Rewrite rMove to be more generic: just take in M and output modified M. This is because we need a family of movement functions for when we update SEARCH component, rMove is a module of search; all modules need to take M as argument and return modified M. first new module is bMove (basic-Move) for MBITES-BASIC.
2. Check in original DHM files when eDefPreG or energyPreG is updated and checked for maturity; need to make sure it is consistent and logical (ie; if energyPreG = 0 at initialization then they are instantly mature). Also need to avoid weird conflicts; if energetics is called before the check would occur in boutS but energyPreG = 0 so they should be mature already (ie; mature in energetics).
3. Rewrite LANDSCAPE creation function to take P as input; make hazard and weight parameters directly adjustable by user (this has already been done to calculate mean values in order to run MBITES-Basic; need to implement for full M-BITES simulation).

<!-- ## To Do List

1. Fix EL4P fitting procedure based on 2004 Statics & Dynamics paper. Do this with JM.
2. Make male DHM-Basic.
3. Incorporate mating into DHM-Basic Cohort simulation.
4. Collective Agent emergent behaviors; look for continuous approximations. See papers from Luca Bortolussi.
5. Run DHM-Basic spatial dynamics based on movement model from 2013 PLoS Heterogeneity... paper.

## Bugs

1. <s> DHM-BloodMeal.R, reFeed function, line 63, references non-existent function reFeedF </s>
2. <s>DHM.R, line 396, energetics function causes bStateNew to be set to "S"; see DHM-Sugar.R, line 76 in function queueSugarBout almost always evaluates to TRUE and causes next behavioral state to go to "S", could be one of two (or both) things:</s>
    *   <s> M$energy is not decreasing as it should be </s> <b>It is decreasing correctly each time energetics(M) is called. A function named immatureEnergetics was added for symmetry in energetics.</b>
    *   <s>The probability pSugarBout is incorrectly specified.</s>
3. <s> DHM.R; lines 361: isAlive and isActive cannot properly subset M (Error in M$bStateNew : $ operator is invalid for atomic vectors) </s>
4. <s> DHM-Mating/chooseMate does now work (dhmP$nMates does not exist) </s> <b> Assigned it to 1 in setup.DHM.R for the time being but need to verify with Dave (should work assuming it is the number of potential mates each female has) </b>
    *   <s> not returning mosquito object </s>
    *   <s> dhmP.N not initialised </s>
    *   <s> Error in runif(1 + rpois(1, dhmP$nMates), 0, N) : object 'N' not found </s>
5.  MatingQs = NULL ("mosquito 182, TRUE, is mating!" Error in MatingQs[[D]] : attempt to select less than one element)
    *   Pops up when D = 0
6. <s> Corrected an error in "DHM-Bloodmeal/reFeed" </s> <b>reFeedF.Pr was returning bStateNew but it was being assign to whole M instead of M$bStateNew </b>
7. <s> enterHouse </s> <b> This error pops up when mosquito tries to enter a house from m, s or l because there are no houses so it is not an error </b>
    *   <s> Error in if (foundGap == TRUE) { : missing value where TRUE/FALSE needed. In addition: Warning message: In rbinom(1, 1, LANDSCAPE$f$enterHouseP[ixf(M)]) : NAs produced </s>
8. <s> queueSugarBout </s>
    *   <s> Energy levels are sometimes negative M$energy in pSugarBout </s>
    *   <s> Error in if (rbinom(1, 1, pSugarBout(M$energy))) M$bStateNew = "S" : argument is not interpretable as logical In addition: Warning message:In rbinom(1, 1, pSugarBout(M$energy)) : NAs produced </s>
9. <s> surviveFlight </s>  <b> Added definition of parameters in setup.DHM.R </b>
    *   <s> Error in if (!rbinom(1, 1, p)) M$bStateNew = "D" : missing value where TRUE/FALSE needed </s>
    *   <s> Tatter is not working: returns NA's </s>
        *   <s> dhmP$ttr.a and dhmP$ttr.b = NULL </s>
10. <s> ixf </s> <b> Added 'haunt' column to LANDSCAPE so that when ixf is called when mosquito is in m then can choose a feeding site that the m site is 'in'. </b>
11. <s> SugarFeedingBout causes an error to pop-up in some situations </s> <b>Clipped the minimum value of energy to 0 to avoid negative probabilities </b>
    *   <s> energetics evaluates to NA </s>
12. activitySpace.daily.Np.i (in activitySpace.daily.Np.R, line 41): n.d is a poisson distributed count of number of places human i visits besides his/her home location on day t. But line 41 if statement only evaluates as true if n.d > 1; should it actually be >= 1? Need to confirm with Dave.
13. probing function in DHM.R (line 134) has input to probeHost "spz", which is a now deprecated element in the mosquito object. All p. falciparum objects are now stored in M$Pf. Need to change this when the Pf.infection modules are working.
14. While running DHM with sugar feeding turned off
    *   <b> Changed line to: EggQ$batch[[EggQ$ix]] <<- allocEggQ(eggs)   [SEE CODE HIGHLIGHT 14]</b>
    *   <s> Error in allocEggQ() : argument "N" is missing, with no default </s>
15. <b> All mosquitos have a 'mated' state == FALSE'</b>
    * <b>Females are emerging in F state so they rarely try to mate again.</b>
16. <s>Mosquitos sugar feed a lot</s>
    * <b>The problem turned out to be that the opportunistic sugar feeding probability was being calculated incorrectly (it should have been the inverse of the one that was being computed). The value also needed to be clipped as sometimes energy level is higher than 1. The change is: pSugarBout(1-clipped)) in queueSugarBout. Some of the dhmP parameters were also changed.</b>
    * <s> Turning sugar feeding behaviour seems to make the simulation run correctly [SEE IMAGE 16]</s>
    * <b>Another part of the problem is that females are emerging in F state instead of M.</b>
17. <s>Females are not trying to mate.</s>
    * <b>Females are emerging in F state instead of M but until the mating problems are fixed we can not change it back as it would produce errors and stop the simulation from running. The line that should be added is: MPop$MM[[index]] = resetMosy(MPop$MM[[index]], MPop$idMX, t, "M", "l", lloc, type, female) instead of:MPop$MM[[index]]=resetMosy(MPop$MM[[index]], MPop$idMX, t, "F", "l", lloc, type, female)in DHM-Mosquito.R "renewMosquito".</b>
18. <s>Females are never entering "O" state</s>
    * <s>Sugar feeding parametrisation is not allowing them to go through that stage. As soon as mosquitoes find an oviposition spot they need to sugar feed and they have no memory on the last state so they go back to blood feed.</s>
    * <b>Turning sugar feeding off "corrects" the error but there is something else. If mosquitos feel the need to sugar feed oportunistically they should return to their previous activity, not draw one at random.</b>
19. <s>Mosquitoes are sugar feeding all the time</s>
    * <b>The blood-feed energetic parameter was way to high!!! Females blood-feed quickly so they never needed to sugar-feed. There might be a problem with the timing of events.</b>
    * <b>Sugar-feeding related functions were re-parametrised:</b>
        * <b>dhmP$S.sa=100</b>
        * <b>dhmP$S.sb=500</b>
        * <b>dhmP$S.sz=1</b>
        * <b>dhmP$B.energy=1/10</b>
20. Female mosy are always emerge with bmSize = 1; after they call BloodMeal this actually decreases!!! Is this biologically correct?
21. Error pops up with "atRisk" list. See figure 21.


{% highlight r %}
(14)

function(eggs, l.i, tm, dam, sire, type){
    EggQ$ix <<- EggQ$ix + 1
    if(EggQ$ix > EggQ$mx){
        #EggQ$batch[[EggQ$ix]] <<- allocEggQ()#MODIFIED IT TEMPORARILY TO MAKE DHM WORK BUT I'M NOT SURE IT'S CORRECT!!!!
        EggQ$batch[[EggQ$ix]] <<- allocEggQ(eggs)
        EggQ$mx <<- EggQ$ix
    }
    EggQ$batch[[EggQ$ix]]$nEggs     <<- eggs
    EggQ$batch[[EggQ$ix]]$l.ix      <<- l.i
    EggQ$batch[[EggQ$ix]]$tm        <<- tm
    EggQ$batch[[EggQ$ix]]$dam       <<- dam
    EggQ$batch[[EggQ$ix]]$sire      <<- sire
    EggQ$batch[[EggQ$ix]]$type      <<- type
}
{% endhighlight %}

<center>{% include image.html file="statesWSugarOff.png" alt="" caption="(16)"  max-width=650 %}</center>



{% highlight r %}
(21)

[1] "ERROR: atRisk overrun"
Error in eval(expr, envir, enclos) : object 'N' not found
In addition: There were 22 warnings (use warnings() to see them)
Called from: eval(expr, envir, enclos)
{% endhighlight %}

### More Substantial Problems & Work To Do:

1.  <s>In Simulations/Default/runit.R: the for loops that runs over the temporal granularity indifference window (Ti) uses a different iterator for time than the main simulation while loop which updates T. This causes problems. NOT SURE, CHECK LATER.</s> Mosy are on different time scale than humans, so it doesn't matter; mosy run in continuous time, humans run in discrete time. Ti only affects human events (that's why recovery from Pf is geometric distribution not exponential, which is discrete form of exponential anyway.)
2. <s>In DHM-Sugar.R, function "sugarWeight" has a switch where if the mosquito is in the "s" point set, sugarWeight is 100; might be too high and this could be causing them to only sugar feed?</s>
    * <b>Changing this value makes them sugar feed less. We should probably re-scale all the values in LANDSCAPE$X$sugar to 0-1.</b>
3. <s>Males emerge into the environment as mature = TRUE. Not sure if this is intentional or not; seems to conflict with DHM-Mating.R queueMating function.</s>
    * <b>Not a problem.</b>
4. <s>DHM-Swarming.R: DailyMatingQs is defined in setup.DHM-Swarming.R to MatingQs = DailyMatingQs(ceiling(Ti)), but does not seem to be called in actual simulation (ie; in runit.R). Need to address this and find out when to set up the mating queue/add to it when running the simulation. RELATED TO 2.
    MatingQs does not update and mosquitoes are not pushed into the mating queue.</s>

    - it's possible that this happens in enterSwarm function found in DHM-Swarming.R which is called during   the maleDHM. Need to check later that this is properly pushing males into the MatingQs.
    - Nope, subsetting MatingQs is just messed up. MatingQs[[D]]$MatingQ[[M$ix]] should be MatingQs[[D]][[M$ix]]
    - in maleDHM there are some functions shared between male and female mosy; it is necessary to make sure they work with both male and females and also to update the env passing in DHM_swarming.R after it is set up in DHM.R:

        * landingSpot
        * surviveRestingHazard
        * timing

    - Mating/Swarming has been sorted out!! Will update how routines work in Mosquito and DHM pages later.

5. <s>clearSwarm in DHM-Swarming.R is never called. Not sure if the MatingQs need to be cleared but just an obervation.</s>

    - Update: Mating now works and MatingQs are created and cleared on daily basis!

6. <s>enterHouse function in DHM.R is recursive; it calls itself. Compare to original enterHouse from MICRO: </s>

{% highlight r %}
enterHouse=function(M,LANDSCAPE,CMParameters,Fwts,Rwts,Lwts,dhmP,SUGAR,TATTER,SENESCE,InAndOut){
    outside=TRUE
    while(outside==TRUE & M$pState=="F" & M$iwofle!=5){
        if(rbinom(1,1,LANDSCAPE$f$eh[M$f.i])){
            outside=FALSE
            if(CMParameters$EAVE.TUBE$ACTIVE==TRUE) M=fEaveTube(M,LANDSCAPE,CMParameters)
            if(CMParameters$IMPROVE.HOME$ACTIVE==TRUE) M=fImproveHome(M,LANDSCAPE,CMParameters)
        } else {
            M$iwofle=newSpot(M, F.wts=Fwts*c(0,1,1,0,1,0),Rwts,Lwts,InAndOut)
            #if(CMParameters$IRS$ACTIVE==TRUE) M=fIRS(M,LANDSCAPE,CMParameters) # REMOVED BY HMSC
            M=surviveRestingHazard(M,LANDSCAPE)
            M=surviveFlightStress(M,dhmP,SUGAR,TATTER,SENESCE)
        }
    }
    return(M)
}
{% endhighlight %}

Here is the new version of enterHouse:
{% highlight r %}
enterHouse <- function(M){
    foundGap <- rbinom(1,1,LANDSCAPE$f$enterHouseP[ixf(M)])
    ###PATCH###
    if(is.na(foundGap)){
        M$lspot <- "l"
        return(M)
    }
    ###PATCH###
    if(foundGap == TRUE){
        if(EAVE.TUBE == TRUE){
            M <- fEaveTube(M)
        }
    } else {
        M$lspot <- newSpot(M)
        M <- surviveFlight(M)
        if(!isAlive(M)){
            return(M)
        } else {
            if(M$lspot == "i"){
                M <- enterHouse(M)
            }
        }
    }
    return(M)
}
{% endhighlight %}

    - Not a problem; they repeat if they are not killed by EAVE.TUBE; if so they pick a new spot and surviveFlight,

7. Added patch to getAtRisk.daily; there is some problems when some mosy tnow/tnext (not sure, check saturday) is less than any of the values of "today" inside of riskList; just made d equal to min of the today vector if this is the case. Need to look more deeply into this phenomenon.

### Notes
Might or might not be bugs. Some are clearly just temporary developmental decisions for testing.

1. Wts's are all 1
2. <s>probing always finds human'</s>
    * <b>They no longer find always human but the biting function is completely random and doesn't depend on wether there are humans or not.
3. <s>Females do not appear to be laying eggs into the LANDSCAPE</s>
    Females now lay eggs into EggQ daily -->
