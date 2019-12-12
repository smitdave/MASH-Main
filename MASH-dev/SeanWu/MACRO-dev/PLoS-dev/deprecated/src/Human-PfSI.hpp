/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  PfSI Human Class
 *
 *  Sean Wu
 *  November 2018
 */

#ifndef HUMAN_PFSI_HPP
#define HUMAN_PFSI_HPP

/* Rcpp includes */
#include <RcppArmadillo.h>

/* standard includes */
#include <stdio.h>
#include <iostream>

/* human_pfsi-specific includes */
#include <string>

/* base-class */
#include "Human.hpp"


/* ################################################################################
 * human-PfSI derived class
################################################################################ */

class human_pfsi : public human {
public:
  human_pfsi(const int id_, const u_int home_patch_id_,
        const std::vector<double> trip_duration_, const double trip_frequency_,
        const double bweight_,
        // PfSI specific values
        const std::string state_, const double age_,
        // biting algorithm
        const int bite_algorithm, void* bite_data,
        tile* tileP_);
  ~human_pfsi();

  /* move operators */
  human_pfsi(human_pfsi&&) = default;
  human_pfsi& operator=(human_pfsi&&) = default;

  /* copy operators */
  human_pfsi(human_pfsi&) = delete;
  human_pfsi& operator=(human_pfsi&) = delete;

  /* simulation */
  virtual void    simulate();

  /* accessors */
  void            set_state(std::string s){ state = s; }
  std::string&    get_state(){ return state; }

  void            set_b(const double b_){ b = b_; }
  double          get_b(){ return b; }

  void            set_c(const double c_){ c = c_; }
  double          get_c(){ return c; }

  /* initialize movement */
  virtual void    initialize_movement();

  /* other implementation */
  virtual void    addVaxx2Q(const Rcpp::List& vaxx);

private:

  void            update_kappa();
  void            update_EIR();
  void            queue_bites();

  std::string     state; /* S,I,P */

  double          b; /* mosquito -> human transmission efficiency */
  double          c; /* human -> mosquito transmission efficiency */
  double          age;

  double          kappa; /* unnormalized kappa for an individual */
  double          EIR; /* individual level entomological inoculation rate */

};

#endif /* Human_PfSI_hpp */
