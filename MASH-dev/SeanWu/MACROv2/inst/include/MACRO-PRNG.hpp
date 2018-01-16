/*
 * ################################################################################
 *        __  ______   __________  ____
 *       /  |/  /   | / ____/ __ \/ __ \
 *      / /|_/ / /| |/ /   / /_/ / / / /
 *     / /  / / ___ / /___/ _, _/ /_/ /
 *    /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *    PRNG Singleton
 *    MASH Team
 *    January 2017
 *
 * ################################################################################
*/

#ifndef _MACRO_PRNG_
#define _MACRO_PRNG_

#include <random>
#include <iostream>

#include "MACRO-DEBUG.hpp"

/* threadsafe prng singleton */
class prng final {
public:
    /* utility methods */
    static prng*                           instance(); /* get instance */
    void                                   set_seed(const uint_least32_t& seed);
    void                                   suicide();

    /* random variate sampling */
    double                                 get_runif();
    double                                 get_rexp(const double& rate);
    double                                 get_rnorm(const double& mean, const double& sd);
    double                                 get_rlnorm(const double& meanlog, const double& sdlog);
    int                                    get_rbinom(const int& n, const double& p);

private:
  /* constructor & destructor */
  prng();
  ~prng();

  /* delete all copy & move semantics */
  prng(const prng&) = delete;
  prng& operator=(const prng&) = delete;
  prng(prng&&) = delete;
  prng& operator=(prng&&) = delete;

  std::mt19937                            rng;
  std::uniform_real_distribution<double>  runif;
};

#endif
