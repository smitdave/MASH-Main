---
title: DHM Parameters Documentation
sidebar: mydoc_sidebar
permalink: docDHMP.html
folder: mydoc
toc: true
summary: "Description of parameters of the Distributed Hazards Model (DHM)"
---
## Timing
Time unit is one day, so 24/2 is 2 hours

*   B.t = 24/2
*   F.t = 24/1
*   R.t = 1.5  
*   L.t = 24/1
*   O.t = 24/1
*   M.t = 24*2
*   S.t = 24/1

## Flight Survival

## Senescence

## Wing Tattering

## Blood Meal Size

* bm_a = 7.5; shape parameter for beta-distributed blood meal size
* bm_b = 2.5; shape parameter for beta-distributed blood meal size

### Refeeding Shape

* rf_a = 18; shape parameter for refeeding probability as function of blood meal size
* rf_b = 10000; shape parameter for refeeding probability as function of blood meal size

### Overfeeding Shape

* of_a = 5; shape parameter for overfeeding death probability as function of blood meal size
* of_b = 500; shape parameter for overfeeding death probability as function of blood meal size

## Egg Batch

* emt_m = 3; mean time to egg maturation
* emt_v = 0.1; variance of egg maturation time
* bs_m = 30; mean egg batch size
* bs_v = 5; variance of egg batch size

## Sugar Bout

## Energetics
1/(Flights Using Full Reserves)

## Mating
*   nMates = 1
*   M.s = .95 ; probability to set bStateNew = "S" post mating bout
*   M.p = .95 ;
*   M.d = 1 - dhmP$M.p ; probability to set bStateNew = "D" post mating bout
*   M.f = 1 - dhmP$M.s - dhmP$M.d ; probability to set bStateNew = "F" post mating bout

## Swarming

*   maleM.s = 0.99 ; probability of successfully entering swarm
*   maleM.p = 0.95 ; probability of surviving maleFlightStress
*   If eDefPreG > 0, it defines a pre-gonogrophic energy requirement that could be filled up preGsugar or preGblood units from each sugar or blood meal.
    *   eDefPreG  = 0
    *   preGsugar = 0
    *   preGblood = 0

## Estivation

*   Probability of surviving estivation
    *   E.p = 0.5
*   Onset of the dry season; a day of the calendar year
    *   Emax = 90
    *   Eb = 0.9
*   End of Estivation; a day of the calendar year
    *   eEndm = 180
    *   eEndV = 30

## Bout Parameters

*   B.s = .99 ; probability of successful blood feeding attempt bout: if successful calls the chooseHost routines, otherwise set hostID = 0
*   F.s = .99 ; probability of successful blood feeding search bout: if successful set bStateNew = "B", otherwise no change
*   L.s = .99 ; probability of successful egg laying search bout: if successful set bStateNew = "O", otherwise no change
*   O.s = .99 ; probability of successful egg laying attenpt bout: if successful calls oviposition and egg batch routines, otherwise no change
*   If skip0 is TRUE
    *   dhmP$skipBatch = dhmP$maxBatch/3
