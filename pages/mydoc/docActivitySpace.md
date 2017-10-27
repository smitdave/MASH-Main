---
title: Activity Space Documentation
sidebar: mydoc_sidebar
permalink: docActivitySpace.html
folder: mydoc
toc: true
summary: "The ActivitySpace structures store information about who is at risk at each feeding station. Each riskList stores information from a human Activity.Space algorithm and is used by a corresponding chooseHost function. These are called only by MICRO as location is resolved only to the patch level in MACRO."
---
##  TimeAtRisk
atRisk.daily models total daily risk: The list stores the id of the human and the proportion of their time at risk.

##  riskList
The riskList describes the distribution of human time at risk across the feeding sites on the landscape. The riskList models total daily risk; the list stores the id of the human and the proportion of their time at risk. Besides humans, there are a list of "other" hosts that a mosquito may encounter:

1.  livestock = -1
2.  ivermectin.livestock = -2
3.  sugar = -3 *DEPRECATED:* sugar sources are now independent landscape objects
4.  atsb = -4
5.  bloodtrap = -5

The structure of the riskList follows:

*   N.d: number of days which risk is modeled for (this is a rolling window in model time).
*   today: integer vector of which days risk is modeled for.
*   atRisk: a list of length equal to N.d; each element is a risk structure for that day.
    * N.f: number of feeding sites for which risk is calculated for (typically set to all sites on the landscape).
    * maxH: the number of humans for which risk is calculated for.
    * N: an integer vector of length equal to N.f; each element is the number of humans at risk at that site at any point on that day.
    * pTm: percent of time that human i is at risk at that feeding site.
    * w: biting weight for human i at that feeding site (distributed as rgamma(1,1,1)).
*   N.o: number of non-human hosts being modeled.
*   other: a list of length equal to N.o which models risk for non-human hosts.
    * zoo: risk for livestock.
        * w: numeric vector of gamma distributed biting weights; rgamma(N.f,1,1)/3
        * typeID: type of the non-human host. 

## activitySpace
The activitySpace functions simulate how each human moves and distributes their time at risk across the feeding sites on the landscape.

*   activitySpace.daily.Np.i: this function puts human i in the atRisk list of riskList for day t.
    1. p.d: proportion of time human i spends at their home feeding site on day t.
    2. add1toAtRisk.daily: this adds human i's risk at home to the atRisk object at day ix, home hhID, human myID, proportion of risk at home p.d, and biting weight w. 
    3. n.d: poisson distributed count of other places visited on that day. If greater than 1 the following routine is called:
        1. For the other places visited a random uniform value is picked from all possible feeding sites (iterate from 1:n.d)
        2. human i is added to the riskList at that randomly picked site, their pTm at that site is calculated as (1-p.d)/n.d 

{% highlight r %}
# add human i to the riskList
activitySpace.daily.Np.i = function(i,ix){with(HUMANS[[i]],{
    with(myTimeAtRisk,{
        p.d = rbeta(1,100,100*(1-p)/p) # proportion of time at home today
        add1toAtRisk.daily(ix, hhID, myID, p.d, w)
        n.d = rpois(1,nDaily)   # other places visited
        if(n.d > 1) for(i in 1:n.d){
            f.n = ceiling(runif(1,0.0000000001,Nplaces))
            add1toAtRisk.daily(ix, loc[f.n], myID,(1-p.d)/n.d, w) 
        }
    })
})}
{% endhighlight %}

Because the activitySpace function does not simulate human movement in terms of explicity journeys to and from destinations, but rather as a distribution of an individual's *time at risk* across sites on the landscape, weighted by an individual level parameter of biting attractiveness, it is appropriate to visualize the distribution as a heatmap. In this visual representation, one can see how an individual's time at risk varies over time. For each unique human in the simulation a hue is sampled with equal distance from all other hues on the color wheel. Each human's time at risk at different sites is then represented by varying the luminosity of that hue at that site based on their contribution to the overall risk at that site.

<center><embed src="http://smitdave.github.io/MASH-Development/images/risk.gif" width="512px" height="683px"></center>

## chooseHost
The chooseHost structures return the identity of the human host or other type of "host" encountered, whether livestock (perhaps treated with ivermectin), ~~sugar source~~, ATSB, or bloodtrap. The class "livestock" is set up by default, a master list for all types is set up in timeAtRisk.MICRO, and the associated functions are defined in the appropriate VECTOR.CONTROL component.

This documentation is described here in Activity Space because while it is called during the mosquito DHM cycle it is almost entirely a function of the riskList and other activitySpace objects.

The chooseHost.daily function locates the focal mosquito and then selects a host at that feeding site based on pTm * w (each human's time at risk multiplied by their biting weight) as a multinomially distributed vector.

{% highlight r %}
chooseHost.daily = function(M){with(M,{
    with(getAtRisk.daily(tnow),{
        nn  = N[ix]
        pTm = pTm[ix,1:nn]
        who = Who[ix,1:nn]
        N   = length(who)
        ww  = w[ix,1:nn]

    for(i in 1:riskList$N.o){
        N = N+1
        ww = c(ww, riskList$other[[i]]$w[ix])
        who = c(who,riskList$other[[i]]$typeID)
        pTm = c(pTm,1)
    }

    M$hostID = i.rmultinom(list(id=who,pr=ww*pTm))
M})})}
{% endhighlight %}