# PLoS submission dev folder for MACRO.

## Requirements:
  * Mosquito:
    * RM model for mosy dynamics (nothing fancy)
  * Human:
    * PfSI and PfMOI
    * Poisson and Negative Binomial `human::queue_bites`

## to-do
  * headers checked:
    1. Tile.hpp
    2. PRNG.hpp
    3. Patch.hpp
    4. Parameters.hpp
    5. Mosquito.hpp
    6. Mosquito-RM.hpp
    7. Logger.hpp
  * headers created:
    1. Patch-Logger.hpp/cpp
    2. Human-QueueBites.hpp/cpp
  * headers in work:
    1. Patch-Logger.hpp/cpp: these need to wait for the PfMOI/PfSI humans to be fully fleshed out, then can make sure logging is set up properly.
