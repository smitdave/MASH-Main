# Sean Wu

## Work Queue

### Debugging:
* PfSI.Setup in MACRO and MICRO need to be de-coupled. MACRO setup should only target objects from MACRO and MICRO setup should only target objects from MICRO.

### MACRO:
* Support initial simulations on Bioko Island data through bugfixes.
  * Recommended Bug Fixes, from DTCitron (10/31/17)
  * When PfSI.Parameters() is called, does this automatically revert to the hard-coded parameter values?  Can we change this to allow users to view the parameters without re-writing the hard-coded values?
  * Double-queueing of (P)rophylaxis - it is possible for someone who is already (P)rotected to receive a second (P)rophylaxis - unclear how these two events are handled at the same time, and also makes event logging difficult
  * Specifying a list of people to infect initially
* See folder MACRO, it is an R package that is mostly just a wrapper for C++ code (mostly drawn from my pet project https://github.com/slwu89/InfectionSim)
* Need to get MACRO running in C++ ASAP:
  * Need to implement PTMF for event queue (queue of pointer to memeber function; will need to use std::function and std::bind to bind function pointer and necessary arguments...need to check with a C++ guru on this).
  * Each event will have to be a struct, other stuff will include the time of event and the tag.
  * How to use `delete this` with smart pointers (`std::unique_ptr`)? When object suicides will all pointers to it also be deleted? or will they go stale? This will be necessary for mosquitoes, which need to manage their own lifetimes, less relevant for MACRO.
 
 ### MBITES:
 *  Have to give a talk on MBITES at computational biology retreat (Nov 18). Will need to run simulations on MBITES-DE and present interesting graphs; talk to John Henry about this. C++ version of upwind PDE solver should help.
 *  Have to add wing tattering and energetics as function of distance to R code.
 *  Have to add diurnal forcing (check with Dave on this...does it just change time to event sampling?)
 *  Have to add gene drive (port MGDrivE inheritance dynamics eventually, maybe wait until we discuss C++ versions).
 
 ### Mosquito Pop Dynamics:
 *  Work with Biyonka to look at suitable algorithms for community detection in bipartite networks (maybe send out an email to the CSSS17 folks to see who knows a lot about multilayer networks...Surendra should know a bit, check the Slack).
 *  Look into Mondrian Processes, seem to be distributions over probability mass functions on kd-trees. Not sure if useful or not, needs thought: http://danroy.org/papers/RoyTeh-NIPS-2009.pdf
 *  Consider swapping tiles in spanning trees if we need to keep geographic contiguity (avoids weirdness in purely statistical approaches).
