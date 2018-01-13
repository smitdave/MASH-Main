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
  /* default constructor & destructor */
  prng() = default;
  ~prng() = default;

  /* delete all copy & move semantics */
  prng(const prng&) = delete;
  prng& operator=(const prng&) = delete;
  prng(prng&&) = delete;
  prng& operator=(prng&&) = delete;

  std::mt19937                            rng;
  std::uniform_real_distribution<double>  runif;
};

/* utility methods */
prng* prng::instance(){
    static prng instance;
    return &instance;
};

void prng::set_seed(const uint_least32_t &seed){
  rng.seed(seed);
  runif = std::uniform_real_distribution<double>(0,1);
};

void prng::suicide(){
  prng::~prng();
};

/* random variate sampling */
double prng::get_runif(){
  return runif(rng);
};

double prng::get_rexp(const double &rate){
  std::exponential_distribution<double>rexp(rate);
  return rexp(rng);
};

double prng::get_rnorm(const double &mean, const double &sd){
  std::normal_distribution<double>rnorm(mean,sd);
  return rnorm(rng);
};

double prng::get_rlnorm(const double &meanlog, const double &sdlog){
  std::lognormal_distribution<double>rlnorm(meanlog,sdlog);
  return rlnorm(rng);
};

int prng::get_rbinom(const int& n, const double& p){
  std::binomial_distribution<int>rbinom(n,p);
  return rbinom(rng);
};

#endif
