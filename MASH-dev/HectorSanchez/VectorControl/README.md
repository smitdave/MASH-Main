# Vector Control

This is a re-implementation of my old mosquito-control routines. The whole set is coded in R6 object-oriented form with inheritance patterns that obey the following structure:

* ControlIntervention (virtual)
  - AquaticIntervention (virtual)
    - Ovitrap [Re-incorporated but awaiting thorough tests]
    - Larvicide
    - SourceReduction
    - FoulHabitat
    - BiologicalControl
  - CattleIntervention (virtual)
    - Ivermectin
    - Zoospray
  - FeedingIntervention (virtual)
    - OdorBaitedTrap
    - IRS
    - EaveTube
    - ImproveHome
  - HumanIntervention (virtual)
    - PersonalRepellant
    - ITN
    - Swat
  - MatingIntervention (virtual)
    - SwarmSpray
  - SugarIntervention (virtual)
    - ATSB [Re-incorporated but awaiting thorough tests. It needs to be changed to _damage_ instead of _kill_]
  - MultiLocationIntervention (virtual)
    - AerialSpray

The accessors/mutators have been coded as part of the main package in: https://github.com/smitdave/MASH-Main/blob/master/MASH-MICRO/R/Site-Methods-VectorControl.R; but have not been documented.

_Note: The repellency effects are yet to be tested by SLWU, so these might not be in place yet._
