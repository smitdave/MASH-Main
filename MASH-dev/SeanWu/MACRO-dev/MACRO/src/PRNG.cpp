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

#include "PRNG.hpp"


/* ################################################################################
* constructor & destructor
################################################################################ */

prng::prng(const uint_least32_t seed) : rng(seed){
 runif = std::uniform_real_distribution<double>(0,1);
 #ifdef DEBUG_MACRO
 std::cout << "prng born at " << this << std::endl;
 #endif
};

prng::~prng(){
 #ifdef DEBUG_MACRO
 std::cout << "prng dying at " << this << std::endl;
 #endif
};


/* ################################################################################
* continuous random univariate sampling
################################################################################ */

double prng::get_runif(){
 return runif(rng);
};

double prng::get_rexp(const double rate){
 std::exponential_distribution<double>rexp(rate);
 return rexp(rng);
};

double prng::get_rnorm(const double mean, const double sd){
 std::normal_distribution<double>rnorm(mean,sd);
 return rnorm(rng);
};

double prng::get_rlnorm(const double meanlog, const double sdlog){
 std::lognormal_distribution<double>rlnorm(meanlog,sdlog);
 return rlnorm(rng);
};


/* ################################################################################
* continuous random multivariate sampling
################################################################################ */

arma::Row<double> prng::get_rdirichlet(const arma::Row<double>& prob){

 arma::Row<double> sample(prob);
 double hold(0.0);

 for(auto& sampleIT : sample){
   if(sampleIT==0)continue;
   std::gamma_distribution<double> gamma(sampleIT, 1.0);
   sampleIT = gamma(rng);
   hold += sampleIT;
 }
 // invert
 hold = 1.0/hold;

 // normalize and return
 return sample*hold;
}


/* ################################################################################
* discrete random univariate sampling
################################################################################ */

int prng::get_rpois(const double lambda){
 std::poisson_distribution<int>rpois(lambda);
 return rpois(rng);
};

int prng::get_rbinom(const int n, const double p){
 if(p >= 1.){
   return n;
 } else {
   std::binomial_distribution<int>rbinom(n,p);
   return rbinom(rng);
 }
};


/* ################################################################################
* discrete random multivariate sampling
################################################################################ */

/* generic discrete distribution */
int prng::get_rcategorical(const arma::Row<double>& prob){
 std::discrete_distribution<int>rcategorical(prob.begin(),prob.end());
 return rcategorical(rng);
};

/* multinomial (from: arXiv:1611.00532) */
arma::Row<int> prng::get_rmultinom(int size, const arma::Row<double>& prob, double switchover){

 arma::Row<int> sample(prob.n_elem,arma::fill::zeros);

 double pprob(0.0);
 double cprob(0.0);
 unsigned int pidx(0);
 while(size > 0)
 {
   pprob += prob[pidx];
   while(((pprob - cprob) * size / (1.0 - cprob)) < switchover)
   {
     cprob += get_beta_1_b(size) * (1.0 - cprob);
     while(pprob < cprob)
       pprob += prob[++pidx];
     if(sample.size() == pidx)
       sample[pidx] = 1;
     else
       sample[pidx] += 1;
     size--;
     if(size == 0)
       break;
   }
   if(size == 0)
     break;
   double p = (pprob-cprob)/(1.0-cprob);
   int nrtaken(0);
   if(p >= 1.){
     nrtaken = size;
   } else {
     if(p > 0.){
       std::binomial_distribution<int> rbinom(size, p);
       nrtaken = rbinom(rng);
     }
   }
   if(nrtaken > 0)
   {
     if(sample.size() == pidx)
       sample[pidx] = nrtaken;
     else
       sample[pidx] += nrtaken;
   }
   size -= nrtaken;
   pidx++;
   cprob = pprob;
 }

 return sample;
};

/* resample template type T x 'size' times */
template<typename T>
T prng::get_resample(const T &x, const int &size){
 std::uniform_int_distribution<int>runif_int(0,x.size()-1); /* no modulo arithmetic bias */
 T out;
 size_t j;
 for(size_t i=0; i<size; i++){
   j = runif_int(rng);
   out.emplace_back(x[j]);
 };
 return out;
};
