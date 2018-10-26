#include "MACRO-PRNG.hpp"

/* constructor & destructor */
prng::prng(){
  #ifdef DEBUG_MACRO
  std::cout << "prng being born at " << this << std::endl;
  #endif
};

prng::~prng(){
  #ifdef DEBUG_MACRO
  std::cout << "prng being killed at " << this << std::endl;
  #endif
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
