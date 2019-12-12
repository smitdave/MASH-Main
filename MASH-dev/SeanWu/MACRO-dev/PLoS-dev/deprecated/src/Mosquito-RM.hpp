/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Simple Ross-MacDonald mosquito model
 *  Requires logging stream 'mosquito'
 *
 *  Sean Wu
 *  November 2018
 */

#ifndef MOSQUITO_RM_HPP
#define MOSQUITO_RM_HPP

/* standard includes */
#include <stdio.h>
#include <iostream>
#include <vector>
#include <algorithm>

/* base-class */
#include "Mosquito.hpp"

/* RcppArmadillo */
#include <RcppArmadillo.h>


/* ################################################################################
 * class declaration
################################################################################ */

/* RM-style deterministic mosquito model */
class mosquito_rm : public mosquito {
public:

  /* constructor & destructor */
  mosquito_rm(const Rcpp::List& mosquito_pars,
             tile* tileP_);
  ~mosquito_rm();

  /* move operators */
  mosquito_rm(mosquito_rm&&) = default;
  mosquito_rm& operator=(mosquito_rm&&) = default;

  /* copy operators */
  mosquito_rm(mosquito_rm&) = delete;
  mosquito_rm& operator=(mosquito_rm&) = delete;

  /* simulation interface */
  virtual void simulate();
  virtual double get_beta(const size_t p);
  virtual void initialize_logging();

private:

  /* private simulation methods */
  void                    aquatic_dynamics(const u_int tnow);
  void                    adult_dynamics(const u_int tnow);

  size_t                  N; /* number of patches */
  arma::Mat<double>       lambda; /* emergence matrix (365 X N) */
  arma::SpMat<double>     psi; /* diffusion matrix (N X N) */

  arma::Col<size_t>       EIP; /* EIP on each day of the year */
  size_t                  maxEIP;
  arma::Col<double>       P; /* survival over EIP */
  arma::Row<double>       kappa; /* net infectiousness to mosquitos (1 X N) */

  double                  p; /* daily survival */
  double                  f; /* blood feeding rate */
  double                  Q; /* human biting rate */
  double                  a; /* human biting rate */
  double                  v; /* daily egg laying rate */

  /* life stages */
  arma::Row<double>       M; /* adult females */
  arma::Row<double>       Y; /* incubating mosquitos */
  arma::Row<double>       Y0; /* newly infected mosquitos */
  arma::Row<double>       Z; /* infectious mosquitos */
  arma::Mat<double>       ZZ; /* incubating mosquitos */
  arma::Mat<int>          ZZ_shift; /* matrix to shift ZZ up */

};


#endif
