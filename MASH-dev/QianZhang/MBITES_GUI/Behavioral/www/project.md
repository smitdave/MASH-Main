
#### What is MASH

**NOTE:** *The following section gives some background on the basic concepts and design principles of MASH. We recommend reading it at some point, but feel free to skip it if you want to get on with learning MBITES.* 

MASH is a framework for simulating human health events and related phenomena (e.g. pathogen transmission by mosquito populations). MBITES is a flexible module that runs the adult mosquito component of MASH. The terms *component* and *module* and *flexible* all have a specific meaning in MASH. The following section explains the terminology used in MBITES and MASH and the rationale for developing it.

MASH is a simulation modeling framework, not a simulation model. To put it another way, the problem MASH was trying to solve was how to build a plug-and-play simulator for human health, not how to simulate human health. The core design problem for MASH was how to build an API (application program interface) defining how various components of a model interact. The philosophy of MASH was that every component should have at least two modules: one would be highly realistic and simulate the relevant phenomena in exquisite detail. Another would be dead simple. 

The first disease to be implemented within MASH was malaria. The collection of all mechanistic models of human malaria have the same basic structural parts: a model of human malaria infection and immunity; a model of mosquito populations, including infection and development of malaria parasites; and some model of immature mosquito  populations in their aquatic habitats. In MASH, each one of these parts is called a COMPONENT. Four distinct biological processes determine how these three components interact: malaria parasites can be passed from mosquitoes to infect humans when mosquitoes probe in anticipation of taking a blood meal; parasites infecting humans can be passed in the blood meal, infecting mosquitoes; adult mosquitoes lay eggs in aquatic habitats; and the eggs hatch, develop through four larval instars, then pupate and emerge as imago (the mature form) from the aquatic habitats to begin their lives as volant adults. A structure allowing MASH components to interact is called a QUEUE. In its most general form, transmission describes the events when pathogens are transmitted, either from mosquitoes to humans or *vice versa*, but it also the dispersion of pathogens, the distances traveled by the pathogens between infection events. The LANDSCAPE is the part of the model that determines how dispersion is defined. 

To be truly plug-and-play, it must be possible to run a component in standalone mode, by passing the inputs from other components trivially as parameters. This is what we have done here. 

MBITES is the exquisitely detailed module for simulating the blood feeding behavior of adult mosquito populations. This document also describes how MBITES is coupled to the component describing the aquatic ecology. We describe how MBITES can be used to simulate pathogen transmission among populations of humans and mosquitoes; here we describe how that would happen without actually simulating transmission. This document describes how to run MBITES in standalone mode. 

The structure of MASH is summarized in the following way:

* A population of **HUMANS** and their health states and health events is the core of MASH. 

* A **COMPONENT** is a basic functional part of any human health model. 

* A **LANDSCAPE** defines the spatial scaffold for interactions. 

* A **QUEUE** is a component level structure allowing two distinct components to interact. 

* A **Module** is a specific set of algorithms that instantiates a component. A module could be flexible in the sense that the same process could be represented by one of many different functional forms. 

* A **Synthetic Population** is the fully configured set of initial conditions for the populations in a model.  

MASH is a framework for building models that is modular and flexible. This gives rise to a complementary vocaublary to describe the models that arise. Borrowing from Linnaeus, we consider MBITES to be a CLASS of models:

* Two models belong to the same ORDER if they belong to the same CLASS and share the same state space. 

* Two models belong to the same FAMILY if they belong to the same ORDER and use the same functional forms.

* Two models belong to the same GENUS if they belong to the same FAMILY and share the same parameters.

* Two models belong to the same SPECIES if they belong to the same GENUS and share the same synthetic population.

* An individual model is an individual member of a SPECIES defined by its random number seed. 

Species-level similarity is the default meaning of **model** in MASH. Without some explicit way of mapping the state space, it is difficult to compare two models belonging to a different order. This hierarchical nomenclature system makes it possible to talk about models defined by some other level of similarity (e.g. model families).

*-Professor David L Smith, Institute for Health Metrics & Evaluation, University of Washington*
