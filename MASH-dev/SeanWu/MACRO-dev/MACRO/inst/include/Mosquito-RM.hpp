/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Simple Ross-MacDonald mosquito model
 *
 *  Sean Wu
 *  November 2018
 */

#ifndef MosquitoRM_hpp
#define MosquitoRM_hpp

/* standard includes */
#include <stdio.h>
#include <iostream>

/* RcppArmadillo */
#include <RcppArmadillo.h>

/* base-class */
#include "Mosquito.hpp"

/* RM-style deterministic mosquito model */
class mosquito_rm : public mosquito {

public:

  /* constructor & destructor */
  mosquito_rm(const size_t N_, const arma::Mat<double>& lambda_, const arma::Mat<double>& psi_,
             const arma::Col<size_t>& EIP_, const size_t maxEIP_,
             const double p_, const double f_, const double Q_, const double v_,
             const arma::Row<double>& M_, const arma::Row<double>& Y_, const arma::Row<double>& Z_,
             tile* tileP_);
  ~mosquito_rm();

  /* move operators */
  mosquito_rm(mosquito_rm&&);
  mosquito_rm& operator=(mosquito_rm&&);

  /* copy operators */
  mosquito_rm(mosquito_rm&) = delete;
  mosquito_rm& operator=(mosquito_rm&) = delete;

  /* simulation interface */
  virtual void simulate();

private:

  size_t                  N; /* number of patches */
  arma::Mat<double>       lambda; /* emergence matrix (365 X N) */
  arma::Mat<double>       psi; /* diffusion matrix (N X N) */

  arma::Col<size_t>       EIP; /* EIP on each day of the year */
  size_t                  maxEIP;
  arma::Col<double>       P; /* survival over EIP */

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
