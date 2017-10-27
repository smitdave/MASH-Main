---
title: MASH (Modular Analysis & Simulation for Health)
keywords: homepage
sidebar: mydoc_sidebar
permalink: index.html
summary:
toc: false
summary: MASH started as a tool for simulating malaria. It has evolved into a tool that is capable of doing simulation and analysis of a wide range of problems for translating evidence into health policy.
---

<center>
{% include image.html file="graph.jpg" alt="Networks are Awesome!" caption="Mosquito movement kernels mapped spatially in a network."  max-width=500 %}
</center>

## What is MASH? What problem is it trying to solve? 

Analysis that can effectively translate evidence into policy advice must confront several core challenges. On the one hand, policy advice should be based on analysis of what we know.  On the other hand, analysis for policy addresses a different kind of question: not “what do we know?” but “what should we do?” Knowing is different from acting, so there is an inevitable frame shift from the epistemologies and conventions familiar to science to the process of making a decision.

Translation from evidence about the state of the world to a set of policy often involves some sort of quantitative model, but there is usually a great deal of quantitative (and sometimes qualitative) uncertainty about the mechanisms, biological processes, and the medical and public health interventions designed to control disease. Data describing the health of human populations and the prevalence of various diseases is uneven in space and time, and this uncertainty about the spatio-temporal distribution of diseases among populations must be accounted for in offering advice about what to do, where, and when. There is almost always some degree of uncertainty about the mechanisms, but compared with evidence describing disease dynamics or risk factors, there is generally more uncertainty about the effectiveness of various medical and public health interventions. A core general problem for translating evidence into sound policy advice is thus how to take all the uncertainty into account in an appropriate way. 
 
A critical appraisal of the grand process of the translation from science to policy for human health – starting with metrology and metrics, through study design, analysis and interpretation of data, model building and synthesis, analysis, policy advice, implementation, monitoring and surveillance, and policy evaluation – highlighted the need for a tool that could help identify good models for various kinds of policy and a process that could stitch them all together seamlessly. On top of all this, policy decisions move on their own timelines, and they are often made without the kind of sound advice that is based on careful analysis. Engaging in the process of making policy is best done in a timely way, regardless of how much evidence exists, so analysts are often required to answer new questions very quickly. 

What is the most useful or most urgently needed information? What models tend to give robust policy advice?  What are the relevant details for making health policy? Attempts to grapple with these challenges in a timely way motivated development of the software package we are calling **MASH** (Modular Analysis & Simulation for Health). 

## MASH Design and Basic Concepts

MASH started as a tool for malaria, but it's design is capable of simulation and analysis to address a wide range of problems in human health. It was designed to be a flexible and extensible tool for simulating human health and disease  in arbitrary detail to study and tame various sorts of complexity. Einstein offered the generic advice that models should be as simple as possible, but no simpler. The advice came with no practical advice for how to build such a model. What is often done is to build models *ad hoc*. What is needed to address the general problem is a way of building suites of models of varying levels of complexity and then doing rigorous model-model comparison. At least one exquisitely detailed model is required to push the limits, but exquisitely detailed and complex models are not the point of MASH. The point is to conduct *in silico* studies to identify the simplest model. The point is to facilitate development of theory for public health.  

To serve its goals, MASH would need to implement true plug-and-play modularity to facilitate hierarchical analysis and simulation of various models and the pieces that comprise them. Developing the initial prototype involved defining the independent parts of a model that would interact, each one hereafter called a **COMPONENT**. Each variant of the algorithm in each *COMPONENT* is called a **Module**. The design of an application program interface (API) in the initial phase called for development of at least one exquisitely complicated and one dead simple *Module* for each *COMPONENT*. The software design problem for MASH was how to parse a processes into *COMPONENT*s and link them, regardless of the complexity of the particular *Module*. To be truly plug-n-play, it should be possible to run any *Module* in stand-alone mode, where any required inputs are passed as a parameter, effectively as a minimal *Module* of the relevant *COMPONENT*. It should thus be possible to analyze any component of the system in isolation. The design principles called for development of complimentary logging, visualization, and analysis programs for each module. To meet all these requirements, the software design problem was solved by requiring components to interact through an **Event Queue**.

As MASH development proceeded and tried to cope with the problems of malaria, it became increasingly clear that the core component of the model was an object representing health states and health events for each individual human in a population of **HUMANS**. MASH is, by design, an individual-based simulation model for human health events, but no other *COMPONENT* is required to be individual based. In driving towards the end goal of developing theory, it should be remembered that a key activity of MASH will be to show when it is appropriate to use simpler approximating models. 

To deal with the problem of mosquito-borne pathogen transmission, in particular, and infectious disease transmission, in general, theory linking interacting individuals in MASH required development of the concept of a **LANDSCAPE**. On the one hand, MASH facilitates development of microsimulation models where interactions occur on point sets: these point-set based simulations are what MASH calls **MICRO**. The complement to *MICRO* is **MACRO**, a *LANDSCAPE* that utilizes coarse information about location, based on the concept of a patch. *MACRO* is MASH's structure for implementing traditional patch-based metapopulation models. Utilities exist to map a simulation in *MICRO* onto a simulation in *MACRO*. MASH is thus designed to evaluate how different ways of defining a “population” affect the conclusions of a simulation study.  What are some good ways of defining populations for models that examine sets of interacting populations?  These model-model comparisons of transmission topologies are what map calls **landscape analysis**. 

Running a model in MASH requires iteratively updating the *HUMANS* by running through and implementing each individual's health *eventQ*. The model then cycles through each interacting *COMPONENT* that could generate new health events at some point in the future. For some kinds of processes, most notably for infectious diseases, there are critically important contingencies that determine how far in advance each *COMPONENT* can be updated. For example, when a person becomes infectious, it elevates the risk of infection in susceptible individuals nearby. Similar problems arise for the interactions among adult and immature populations of mosquitoes that transmit pathogens. A *COMPONENT* can only be updated over a time interval that would not be affected by a contingent event in another *COMPONENT*. The maximum time interval for each interaction must be identified by giving careful thought to these contingencies. The interval for updating interacting components must be shorter than this interval, that MASH calls the Temporal Window for Indifference to Contingent Events (**TWICE**). Delays, such as the incubation and latent periods for infectious diseases, are often a nuisance for simple compartmental models, but they make computation easier in MASH.  

## MASH Development

MASH is still a *beta* release. The first release will have limited capabilities as we continue to work on code design issues.  Updates are forthcoming for **MASH-1.0**. For those who are interested in following updates or contributing, code updates will be released as an **R** package and as archived GitHub code.  

For more information about MASH, please read on. 

*-Professor David L Smith, Institute for Health Metrics & Evaluation, University of Washington*

*If you would like to contribute, please write smitdave@uw.edu* 


<center>
{% include image.html file="MASH_v2.gif" alt="Networks are Awesome!" caption=""  max-width=800 %}
</center>
