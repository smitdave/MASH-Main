/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  specialized random variate samplers not in base Rmath.h
 *
 *  Sean Wu (slwu89@berkeley.edu)
 *  July 2019
*/

#ifndef RNG_HPP
#define RNG_HPP

#include <RcppArmadillo.h>

// movement needs to sample a categorical distribution
// stupid simple inversion method (Devroye III.2)
static int rcategorical(const arma::Row<double>& probs){
  int x = 0;
  if(probs.size() == 1){
    return x;
  }
  double s = probs.at(0);
  double u = R::runif(0.,1.);
  while(u > s){
    x += 1;
    s += probs.at(x);
  }
  return x;
};


/* PDG requires a multivariate hypergeometric random number, which we adapt the algorithm from Agner Fog here (cite him!) */
// citation: Fog, A. "Non-uniform random number generators." (2005).
// destination: array to fill the drawn "balls"
// source: number of "balls" in each "urn"
// n: number of draws to take
// k: number of "urns"
static void rmhyper(int* destination, int const* source, int n, int k){
  int sum, x, y;
  size_t i;
  if(n < 0 || k < 0){Rcpp::stop("Invalid parameters of distribution");}

  // total number of "balls"
  for(i = 0, sum = 0; i < k; i++){
    y = source[i];
    if(y < 0){Rcpp::stop("Cannot have a negative number of balls in an urn");}
    sum += y;
  }
  if(n > sum){Rcpp::stop("Distribution undefined for n > sum");}

  for(i=0; i<k-1; i++){
    // generate ouput by calling rhyper k-1 times
    y = source[i];
    x = (int)R::rhyper((double)y,(double)sum-y,(double)n);
    n -= x;
    sum -= y;
    destination[i] = x;
  }
  // get the last one
  destination[i] = n;
};

#endif
