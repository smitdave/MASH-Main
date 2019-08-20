/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Generic Mosquito Class: mosquitos are specialized by model type
 *
 *  Sean Wu
 *  November 2018
 */

#ifndef MOSQUITO_HPP
#define MOSQUITO_HPP

/* standard includes */
#include <stdio.h>
#include <iostream>
#include <memory>

/* Rcpp includes */
#include <RcppArmadillo.h>


/* ################################################################################
 * forward declarations & alias/typedefs
################################################################################ */

class tile;


/* ################################################################################
 * class declaration
################################################################################ */

/* abstract base mosquito */
class mosquito {
public:

  /* constructor & destructor */
  mosquito(tile* tileP_) : tileP(tileP_) {

    #ifdef DEBUG_MACRO
    std::cout << "mosquito born at " << this << std::endl;
    #endif

  };
  virtual ~mosquito();

  /* move operators */
  mosquito(mosquito&&) = default;
  mosquito& operator=(mosquito&&) = default;

  /* copy operators */
  mosquito(mosquito&) = delete;
  mosquito& operator=(mosquito&) = delete;

  /* factory method */
  static std::unique_ptr<mosquito> factory(const Rcpp::List& mosquito_pars, tile* tileP_);

  /* interface */
  virtual void simulate() = 0;
  virtual double get_beta(const size_t p) = 0; /* beta: number of infectious bites mosquitos produce today in patch p; in RM models it is a*Z */
  virtual void initialize_logging() = 0;

protected:

  tile*                 tileP;

};


#endif
