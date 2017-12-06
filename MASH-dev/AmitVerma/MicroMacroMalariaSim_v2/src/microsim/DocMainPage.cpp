/**
 * \file DocMainPage.cpp
 * \brief This file is used for documentation
 * 
 * \todo
 * - Documentation
 *  - complete MalariaSim.cpp documentation
 *  - Edit the main page
 *  - fix images for latex version
 *  - fix latex version with proper toc etc.
 *  - make a new flowchart for mozzy cycle
 */

/** \mainpage MalariaMicroSim Documentation
 *
 *
 *
 * \section Contents
 * - \ref intro_sec 
 * - \ref over_ver
 * - \ref inst
 * - \ref exec
 * - \ref sim_over 
 *  - \ref sim_over_imp
 *  - \ref sim_over_bi
 *  - \ref sim_over_se
 *  - \ref sim_over_ds
 * - \ref flowc
 * \section intro_sec Introduction
 *
 * This document details the functional specifications along with the software and
 * hardware requirements for the design of the simulator developed for the
 * purpose of the MAP-E project. It is a living document that is expected to
 * evolve throughout the design process. The primary focus of this document is
 * to illustrate the details of the simulator design with the intent that
 * future users will be able to incorporate their research easily vis-à-vis
 * modify one or more parts of the simulator. Easy usability of the simulator
 * for the general (technologically non-advanced) user is a key element of
 * good software design and the methodology used here takes that perspective
 * into account.
 *
 * \section over_ver Overview of the current version
 *
 * \section inst Installation
 *
 * \section exec Execution
 *
 * \section sim_over Simulator Overview
 * \subsection sim_over_imp Implementation
 * \subsection sim_over_bi Background Information
 *
 * The purpose of the simulator is to simulate a malaria transmission model.
 * Malaria transmission models all differ from one another in many ways,
 * including the biological details that are included in each one of the
 * modules and their coupling, whether they are deterministic or stochastic,
 * whether they track individuals or classes of individuals, and whether they
 * include spatial information or not.
 *
 * The dynamics and control of malaria transmission are affected by three
 * biological cycles:
 * -# the mosquito life-cycle
 * -# the adult-mosquito feeding cycle
 * -# the parasite life cycle
 *
 * The relevant details for the process of malaria transmission can be
 * described quantitatively in three modules:
 * 1) a module that describes mosquito larval ecology in aquatic habitats;
 * 2) a module that describes adult mosquito feeding behavior and the
 * epidemiology of malaria infection in adult mosquitoes; and 3) a module
 * that describes the epidemiology of human malaria infections. Adult
 * mosquitoes, where sexual reproduction in the parasites occurs is
 * dynamically linked to the other two modules through a set of feedbacks:
 * adult mosquitoes lay eggs that develop into emerging adults, and mosquitoes
 * take up parasites during a bloodmeal and if they then survive long enough
 * to become infectious, they may pass the infections back to the humans.
 *
 * The outputs of the simulator, along with the relevant inputs provided with
 * the help of a file or directly through a Graphical User Interface (GUI) can
 * be used to obtain several metrics of interest. One such metric records the
 * number of infectious bites, per person, per day, called the entomological
 * inoculation rate (EIR). Another quantity of interest is the prevalence of
 * infection by age in humans, called the parasite rate (PR). Both these
 * metrics are critical to support effective public policy implementation to
 * control malaria outbreak, spread and eradication.
 *
 *
 * \subsection sim_over_se System Evolution Description
 *
 * Malaria is transmitted when uninfected mosquitoes bite infected humans and
 * these infected mosquitoes, in turn, pass on the infection when they bite
 * humans.  For this simulator, the effect of the commonly occurring Plasmodium
 * falciparum parasite is considered. This simulator has been designed using a
 * stochastic malaria transmission model with an individual-mosquito based
 * implementation. We keep track of every mosquito in this simulation and its
 * interaction with the human population. The simulator is designed with the
 * following fundamental assumptions:
 * • All humans are bitten at the same rate, and mosquitoes can bite a person
 * selected at random (i.e. transmission assumes mass-action or it is well-mixed).
 * • Human population size is constant - when a person dies, he is immediately
 * replaced by another human being.
 * • Humans are confined to a fixed location. No emigration or immigration is
 * considered.
 * • Biting rates are the same for every person in the population.
 * • Mosquitoes die a natural death depending on its age. Although, there is
 * some probability that it may die randomly (based on the outcome of a uniform
 * random coin-toss) on any day during its life-cycle.
 * • The human population size is constant.
 * • When the simulation begins mosquitoes are uninfected, whereas an expected
 * fraction of the human population is infected.
 *
 * \image html worldview.png
 * \image latex worldview.png width=7cm
 *
 * Geographically we can represent our entire system as shown in Fig. [PUT NUM HERE]
 * A region or map is broken up into patches. Ponds and houses are located
 * inside a patch. The information on which pond(s) and house(s) constitute a
 * patch is available in an input file. Also, the information of humans living
 * inside those houses are available. We track individual humans and assume
 * they are not mobile as long as the simulation is running, i.e. they are
 * confined to a fixed location (house). Conceptually, house represents an
 * abstract location where humans can be found and need not indicate an actual
 * house.
 * Mosquitoes are born at a pond, fly to a house to take a bloodmeal, rest, and
 * then fly to a pond to oviposit. Thus a mosquito moves through three important
 * states: 1) Feeding 2) Resting 3) Oviposition. This is illustrated with the
 * help of a state transition diagram in Fig. [Fig No]
 * Let the number of emerging adult mosquitoes from a pond per day be denoted
 * as λ. This may vary seasonally and even from region to region. In this
 * simulator, we assume the value of λ varies for every day of a calendar year
 * before the simulation starts. Also, the value of λ is assumed to be the same
 * for every pond. On any given day, newly emerging adult mosquitoes as well as
 * those mosquitoes that have successfully completed oviposition travel to the
 * houses inside a patch for a bloodmeal. Note that there is some probability
 * that mosquitoes might travel to houses located in neighboring patches. We
 * assume that every patch has a set of neighboring patches and these neighbors
 * are known.
 * 
 * Further, we assume humans have a biting-weight that determines how likely is
 * the human to get bitten. We consider humans may or may not own an ITN and if
 * he owns one, it deteriorates with age. Simply owning an ITN doesn’t guarantee
 * its usage, and hence we have introduced another variable that decides how
 * often the ITN is being used. All these parameters are assumed to be input.
 *
 * Humans on being infected, can treat themselves or may choose to ignore
 * medical help. Thus, we consider the use of a fraction (available as an input
 * parameter) to determine whether the human is likely to get drug treatment or
 * not. For example, the head of the household will probably be more serious in
 * obtaining treatment.
 * 
 * Each mosquito moves from the bloodmeal, heavily laden with blood and
 * physiologically stressed, to a wall indoors or to a wall outdoors according
 * to a probability R. Note that the probability of the mosquito staying alive
 * when resting indoors, p r,i depends on the effects of the IRS and ITN on that
 * day. They are represented with the variables e and  respectively in Fig.
 * Their initial strengths are referred to as the initial IRS and initial ITN.
 * Both the IRS and ITN effects are assumed to decay exponentially starting from
 * the date when such intervention is performed. The initial strengths of IRS
 * and ITN as well as both their decay rates are assumed to be given as inputs.
 *
 * \image html mozzylifecycle1.png
 *
 * At a feeding station, a mosquito bites a human being as described in the
 * model shown in Fig. .  Following successful feeding, a mosquito goes into
 * rest as explained in Fig. . On the other hand, if the feeding was
 * unsuccessful, the mosquito attempts to bite another human in the same house.
 * In the event that it is unable to bite any human at that house, it flies to
 * the next house in that patch and repeats the same process until it obtains a
 * bloodmeal. Incidentally, on a particular day, if a mosquito is unable to find
 * any bloodmeal at any house in that patch, it returns back to the pond and
 * resumes the feeding process the next day.
 *
 * \image html mozzylifecycle2.png
 *
 * When an infected mosquito feeds on an uninfected human, the human being is
 * susceptible to being infectious with a probability b. These interactions are
 * expressed with the help of Fig. . Humans get infectious after a fixed latency
 * period. Further, we assume that the infectiousness of a human is constant,
 * regardless of the age of the infection. The waiting time to clear an
 * infection is exponentially distributed, with mean waiting time to clear 1/r,
 * where the value of the rate r is given. In addition, we assume that humans
 * who are infectious can develop a fever according to some probability that
 * depends on the malaria age. This fever starts after a fixed number of days
 * on being bitten, which is referred to as the incubation days. Here, malaria
 * age is the number of days in a human’s life when he is infected.
 *
 * After a human gets a fever, a uniform random number is drawn from the unit
 * interval, and if it is less than his propensity to get treated (which is a
 * fraction and available as input), then drug treatment is administered.
 * 
 * In the case when an uninfected mosquito bites a infected human, the
 * probability of the mosquito getting infected is c. When an infected mosquito
 * bites an infected human, then the human gets a superinfection. In that case
 * his infection is already started and is cleared on the day which happens to
 * be the maximum of the clear infection dates corresponding to the old and the
 * new infection.
 *
 * \image html mozzylifecycle3.png
 *
 * \subsection sim_over_ds Fundamental Data Structures
 *
 * In this simulator, we conceptually attach human(s) with a house. One or
 * several houses and ponds constitute an arbitrary geographical area called a
 * patch. Thus, from our discussion earlier, at a top-level abstraction, the
 * five fundamental data structures of the simulator can be : Mosquito, Human,
 * House, Pond, and Patch. These are implemented as classes with
 * inter-dependencies and member variables/methods as shown in Fig. . All the
 * member variables and functions of the classes used in this simulator have
 * been declared public. Further, we have invoked the STL class by creating
 * instances of <vector> containers on several of the above-mentioned classes.
 * The reason for choosing <vector> class is the constant time operation of
 * accessing individual elements similar to array manipulations. However,
 * addition and deletion of elements to instances of this class is handled
 * automatically by the compiler allowing greater ease in handling. The STL
 * provides support of several library functions for performing standard
 * operations on instances of the <vector> class.
 *
 * \section flowc Flow Chart
 * \image html flowchart.png
 *
 */
