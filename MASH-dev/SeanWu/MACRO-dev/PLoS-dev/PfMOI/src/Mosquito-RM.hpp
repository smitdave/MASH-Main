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

#ifndef MOSY_RM_HPP
#define MOSY_RM_HPP

/* standard includes */
#include <iostream>
#include <vector>
#include <algorithm>

/* RcppArmadillo */
#include <RcppArmadillo.h>


/* ################################################################################
 * forward declarations & alias/typedefs
################################################################################ */

/* forward declarations */
class tile;


/* ################################################################################
 * class declaration
################################################################################ */

/* RM-style deterministic mosquito model */
class mosquito {
public:

  /* constructor & destructor */
  mosquito(const Rcpp::List& mosquito_pars,
             tile* tileP_);
  ~mosquito() = default;

  /* move operators */
  mosquito(mosquito&&) = default;
  mosquito& operator=(mosquito&&) = default;

  /* copy operators */
  mosquito(mosquito&) = delete;
  mosquito& operator=(mosquito&) = delete;

  /* simulation interface */
  void    simulate();
  double  get_beta(const u_int p){return a * Z.at(p);};
  void    initialize_logging();

private:

  tile*                 tileP;

  /* private simulation methods */
  void                    aquatic_dynamics(const u_int tnow);
  void                    adult_dynamics(const u_int tnow);

  u_int                   N; /* number of patches */
  arma::Mat<double>       lambda; /* emergence matrix (365 X N) */
  arma::SpMat<double>     psi; /* diffusion matrix (N X N) */

  arma::Col<u_int>        EIP; /* EIP on each day of the year */
  u_int                   maxEIP;
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
