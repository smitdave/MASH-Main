---
title: Pathogens
sidebar: mydoc_sidebar
permalink: docPathogens.html
folder: mydoc
toc: true
summary: "Description of pathogen models."
---
# Plasmodium falciparum

## PfSI

PfSI defines a simple susceptible-infected model for human infection with the plasmodium falciparum strain of malaria. The PfSI module of the PATHOGEN component relies on the PfM packet, which is a package of information the mosquito carries about current infection(s) that she is carrying.

<center>
<embed src="http://smitdave.github.io/MASH-Development/images/uninterpretableChain.html" width="1000px" height="500px">
</center>

<center>
{% include image.html file="humansPfSI.png" alt="Human PfSI" caption="Human trajectories for PfSI module." %}
</center>

## LOME (Lifetime Ontogeny of Malaria Epidemiology)

### Summary of LOME

LOME was designed to be an extremely detailed within-host model of plasmodium falciparum infection to investigate the "problem" with malaria for a specific set of vector life-cycle parameters, host population, and landscape. LOME is designed to answer the question: for a given host, given a history of N previous infections, what is the outcome of the N+1 infection? LOME is thus a suite of models that can exhaustively and flexibly address the following issues regarding outcomes of infection:

1.  What is the probability of bloodstream infection in a host following an infectious bite?
2.  What is the time course of a given infection?
  *  Given an initial strain of pathogen infection, what are the parasite densities over time until none of the clonal descendants of the original strain remain in the host?
3.  What is the infectiousness of the host to the mosquito vectors over time?
4.  What are disease outcomes?
5.  How does this infection course stimulate immunity affecting subsequent infections (the functions that govern immunity to the initial clonal variant's infection is governed by the functions that address question 2)?
6.  How do different clonal variants interact in a host?
  *  Interference between clonal variants: either through modifying immune response, specific immune responses, or through direct competition for resources.

### Within-host Infection Time-course Simulation

LOME is a system of functions to simulate the within host time course of plasmodium falciparum infection that begins with successful vector to host transmission of the pathogen to mature parasite production in the host blood and successful host to vector transmission. The time course of a <i>Pf</i> infection can be broken down as:

1.  Mosquito bite to bloodstream infection (vector to host transmission of sporozoites)
2.  Asexual blood stage infections, which:
  1.  cause fever and other disease;
  2.  make gametocytes;
  3.  stimulates immunity that affects future infections
3.  Mature gametocytes to mosquito infection.
