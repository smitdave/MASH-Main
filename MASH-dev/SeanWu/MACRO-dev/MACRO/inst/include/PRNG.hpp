/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Pseudo random number generator
 *
 *  Sean Wu
 *  November 2018
 */

#ifndef PRNG_hpp
#define PRNG_hpp

/* C++ includes */
#include <random>
#include <vector>
#include <iostream>
#include <math.h>

#include <RcppArmadillo.h>

class prng {
public:

    /* constructor & destructor */
    prng(const uint_least32_t seed);
    ~prng();

    /* delete copy constructor/assignment operator, default move constructor/assignment operator */
    prng(const prng&) = delete;
    prng& operator=(const prng&) = delete;
    prng(prng&&) = default;
    prng& operator=(prng&&) = default;

    /* continuous random univariate sampling */
    double                                 get_runif();
    double                                 get_rexp(const double rate);
    double                                 get_rnorm(const double mean, const double sd);
    double                                 get_rlnorm(const double meanlog, const double sdlog);
    double                                 get_beta_1_b(const double b){return 1.0 - pow(runif(rng), 1.0/b);};

    /* continuous random multivariate sampling */
    arma::Row<double>                      get_rdirichlet(const arma::Row<double>& prob);

    /* discrete random univariate sampling */
    int                                    get_rpois(const double lambda);
    int                                    get_rbinom(const int n, const double p);

    /* discrete random multivariate sampling */
    int                                    get_rcategorical(const arma::Row<double>& prob);
    arma::Row<int>                         get_rmultinom(int size, const arma::Row<double>& prob, double switchover = 1.0);

    /* resample template type T x 'size' times */
    template<typename T>
    T                                      get_resample(const T& x, const int& size);

private:
  std::mt19937                            rng;
  std::uniform_real_distribution<double>  runif;
};

#endif
