# Vector Control

This is a re-implementation of my old mosquito-control routines. The whole set is coded in R6 object-oriented form with inheritance patterns that obey the following structure:

* ControlIntervention (virtual: implements generic killing and repelling routines)
  - AquaticIntervention (virtual)
    - Ovitrap:  [x] [x] [x] [ ] [ ]
    - Larvicide: [ ] [ ] [ ] [ ] [ ]
    - SourceReduction: [ ] [ ] [ ] [ ] [ ]
    - FoulHabitat: [ ] [ ] [ ] [ ] [ ]
    - BiologicalControl: [ ] [ ] [ ] [ ] [ ]
  - CattleIntervention (virtual)
    - Ivermectin: [ ] [ ] [ ] [ ] [ ]
    - Zoospray: [ ] [ ] [ ] [ ] [ ]
  - FeedingIntervention (virtual)
    - OdorBaitedTrap [ ] [ ] [ ] [ ] [ ]
    - IRS: [ ] [ ] [ ] [ ] [ ]
    - EaveTube: [ ] [ ] [ ] [ ] [ ]
    - ImproveHome: [ ] [ ] [ ] [ ] [ ]
  - HumanIntervention (virtual)
    - PersonalRepellant: [ ] [ ] [ ] [ ] [ ]
    - ITN: [ ] [ ] [ ] [ ] [ ]
    - Swat: [ ] [ ] [ ] [ ] [ ]
  - MatingIntervention (virtual)
    - SwarmSpray: [x] [x] [x] [ ] [ ]
  - SugarIntervention (virtual)
    - ATSB: [x] [x] [x] [ ] [ ]
  - MultiLocationIntervention (virtual)
    - AerialSpray: [ ] [ ] [ ] [ ] [ ]

Checkboxes key: [addedInLandscape/Human, addedInteractionWithMosquito, runsWithoutErrors, tested, hasBeenVerified]

The accessors/mutators have been coded as part of the main package in: https://github.com/smitdave/MASH-Main/blob/master/MASH-MICRO/R/Site-Methods-VectorControl.R; but have not been documented.

_Note: The repellency effects are yet to be tested by SLWU, so these might not be in place yet._
