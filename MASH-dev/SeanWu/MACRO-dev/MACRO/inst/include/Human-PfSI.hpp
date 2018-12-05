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

#ifndef Human_PfSI_hpp
#define Human_PfSI_hpp

/* standard includes */
#include <stdio.h>
#include <iostream>

/* human_pfsi-specific includes */
#include <string>

/* base-class */
#include "Human.hpp"

class human_pfsi : public human {
public:
  human_pfsi(const int id_, const size_t home_patch_id_,
        const double trip_duration_, const double trip_frequency_,
        const double bweight_, tile* tileP_,
        /* human_pfsi specific arguments */
        const double age_, const bool inf_, const bool chx_);
  ~human_pfsi();

  /* move operators */
  human_pfsi(human_pfsi&&);
  human_pfsi& operator=(human_pfsi&&);

  /* copy operators */
  human_pfsi(human_pfsi&) = delete;
  human_pfsi& operator=(human_pfsi&) = delete;

  /* print */
  void print(){
    human::print();
    std::cout << "my infection is: " << infection << ", my age is: " << age << std::endl;
  }

  /* simulation */
  virtual void    simulate();

  /* accessors */
  void            set_infection(const bool inf){ infection = inf; }
  bool            get_infection(){ return infection; }

  void            set_chemoprophylaxis(const bool chx){ chemoprophylaxis = chx; }
  bool            get_chemoprophylaxis(){ return chemoprophylaxis; }

  void            set_b(const double b_){ b = b_; }
  double          get_b(){ return b; }

  void            set_c(const double c_){ c = c_; }
  double          get_c(){ return c; }

private:

  void            update_kappa();
  void            update_EIR();
  void            queue_bites();

  bool            infection; /* indicator variable (S,I) */
  bool            chemoprophylaxis; /* protected by drugs or not? */
  double          b; /* mosquito -> human transmission efficiency */
  double          c; /* human -> mosquito transmission efficiency */
  double          age;

  double          kappa; /* unnormalized kappa for an individual */
  double          EIR; /* individual level entomological inoculation rate */

};

#endif /* Human_PfSI_hpp */
