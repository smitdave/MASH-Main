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

#include <RcppArmadillo.h>

// movement needs to sample a categorical distribution
// stupid simple inversion method (Devroye III.2)
int rcategorical(const arma::Row<double>& probs){
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
