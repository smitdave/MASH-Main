/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Patch: a cell in a tile (conditional on stuff in the patch, the humans & mosquitos are independent)
 *
 *  Sean Wu
 *  November 2018
*/

#ifndef PATCH_HPP
#define PATCH_HPP

/* standard includes */
#include <iostream>
#include <array>
#include <string>

/* RcppArmadillo */
#include <RcppArmadillo.h>


/* ################################################################################
 * forward declarations & alias/typedefs
################################################################################ */

/* forward definitions */
class tile;


/* ################################################################################
 * class declaration
################################################################################ */

class patch {
public:
  /* constructor & destructor */
  patch(const Rcpp::List& patch_pars, tile* tileP_);
  ~patch() = default;

  /* move operators */
  patch(patch&&) = default;
  patch& operator=(patch&&) = default;

  /* copy operators */
  patch(patch&) = delete;
  patch& operator=(patch&) = delete;

  /* accessors */
  u_int                 get_id(){return id;}
  arma::Row<double>&    get_move(){return move;}
  double                get_bWeightHuman(){return bWeightHuman;}
  double                get_bWeightZoo(){return bWeightZoo;}
  double                get_bWeightZootox(){return bWeightZootox;}
  double                get_kappa(){return kappa;}
  bool                  get_reservoir(){return reservoir;}
  double                get_res_EIR(){return res_EIR;}

  /* debug */
  void                  print();

  /* increments */
  void                  accumulate_bWeightHuman(double b){bWeightHuman += b;}
  void                  decrement_bWeightHuman(double b){bWeightHuman -= b;}
  void                  zero_bWeightHuman(){bWeightHuman = 0.0;}

  void                  accumulate_bWeightZoo(double b){bWeightZoo += b;}
  void                  decrement_bWeightZoo(double b){bWeightZoo -= b;}
  void                  zero_bWeightZoo(){bWeightZoo = 0.0;}

  void                  accumulate_bWeightZootox(double b){bWeightZootox += b;}
  void                  decrement_bWeightZootox(double b){bWeightZootox -= b;}
  void                  zero_bWeightZootox(){bWeightZootox = 0.0;}

  void                  accumulate_kappa(double k){kappa += k;}
  void                  decrement_kappa(double k){kappa -= k;}
  void                  zero_kappa(){kappa = 0.0;}
  void                  normalize_kappa();

  /* PfSI specific member functions */
  void                  update_incidence(const bool travel);
  void                  update_SIP_visitor(const std::string state);
  void                  update_SIP_resident_home(const std::string state);
  void                  update_SIP_resident_away(const std::string state);
  void                  reset_SIP();
  void                  log_output();

private:

  /* id */
  u_int                id;

  /* movement vector */
  arma::Row<double>     move;

  /* infection dynamics */
  double                bWeightHuman;
  double                bWeightZoo;
  double                bWeightZootox;
  double                kappa;

  /* infection reservoir? */
  bool                  reservoir;
  double                res_EIR;

  /* tile pointer */
  tile*                 tileP;

  /* PfSI specific data members */
  std::array<int,3>     SIP_visitor;
  std::array<int,3>     SIP_resident_home;
  std::array<int,3>     SIP_resident_away;
  int                   inc_travel;
  int                   inc_resident;
};


#endif
