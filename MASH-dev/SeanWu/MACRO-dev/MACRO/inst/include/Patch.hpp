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

#ifndef Patch_hpp
#define Patch_hpp

/* standard includes */
#include <stdio.h>
#include <iostream>

/* RcppArmadillo */
#include <RcppArmadillo.h>

/* forward definitions */
class tile;

/* class definition */
class patch {
public:
                        /* constructor & destructor */
                        patch(const Rcpp::List& patch_pars, tile* tileP_);
                        ~patch();

  /* accessors */
  size_t                get_id(){return id;}
  arma::Row<double>&    get_move(){return move;}
  double                get_bWeightHuman(){return bWeightHuman;}
  double                get_bWeightZoo(){return bWeightZoo;}
  double                get_bWeightZootox(){return bWeightZootox;}
  double                get_kappa(){return kappa;}
  bool                  get_reservoir(){return reservoir;}
  double                get_res_EIR(){return res_EIR;}

  /* JUST FOR PRISM DATA */
  double                get_res_EIR_prism(const size_t t){return res_EIR_prism[t];}

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

private:

  /* id */
  size_t                id;

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

  /* JUST FOR PRISM DATA */
  std::vector<double>                res_EIR_prism;
  /* END PRISM DATA STUFF */
  
  /* tile pointer */
  tile*                 tileP;

};


#endif
