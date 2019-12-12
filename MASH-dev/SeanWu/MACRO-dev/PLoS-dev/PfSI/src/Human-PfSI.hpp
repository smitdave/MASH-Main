/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  PfSI human
 *
 *  Sean Wu (slwu89@berkeley.edu)
 *  July 2019
*/

#ifndef HUMAN_PFSI_HPP
#define HUMAN_PFSI_HPP

// C++ includes
#include <iostream>
#include <string>
#include <vector>
#include <memory>

// Rcpp includes
#include <RcppArmadillo.h>


/* ################################################################################
 * forward declarations & alias/typedefs
################################################################################ */

class event;
using eventP = std::unique_ptr<event>;

class queue_bites;
using queue_bitesP = std::unique_ptr<queue_bites>;

class tile;

class patch;


/* ################################################################################
 * human class
################################################################################ */

class human {
public:

  /* constructor & destructor */
  human(/* basic values */
        const int id_, const u_int home_patch_id_,
        const std::vector<double> trip_duration_, const double trip_frequency_,
        const double bweight_, tile* tileP_,
        // PfSI CoI model
        const std::string state_, const double age_
      );
  ~human();

  /* move operators */
  human(human&&) = default;
  human& operator=(human&&) = default;

  /* copy operators */
  // NOW delete
  human(human&) = delete;
  human& operator=(human&) = delete;

  /* accessors */
  u_int                 get_id(){return id;};
  bool                  get_alive(){return alive;};
  double                get_tnow(){return tnow;};

  u_int                 get_patch_id(){return patch_id;};
  void                  set_patch_id(const u_int pid){ patch_id = pid; };
  u_int                 get_home_patch_id(){return home_patch_id;};
  bool                  get_travel(){return travel;};
  void                  set_travel(const bool travel_){travel = travel_;};
  double                get_trip_duration(const u_int pid){return trip_duration.at(pid);};
  double                get_trip_frequency(){return trip_frequency;};
  patch*                get_patch();
  patch*                get_home_patch();

  double                get_bweight(){return bweight;};

  tile*                 get_tile(){return tileP;};

  /* biting */
  void                  decrement_bweight();
  void                  accumulate_bweight();

  /* event queue related functions */
  void                  addEvent2Q(event&& e);
  void                  rmTagFromQ(const std::string &tag);
  void                  fireEvent();
  void                  printEventQ();

  /* interface */
  void                  initialize_biting(const int algorithm, void* data);
  void                  initialize_movement();
  void                  initialize_courseofinf();
  void                  simulate();
  void                  addVaxx2Q(const Rcpp::List& vaxx);

  /* PfSI specific methods */
  void                  set_state(std::string s){ state = s; }
  std::string&          get_state(){ return state; }

  void                  set_b(const double b_){ b = b_; }
  double                get_b(){ return b; }

  void                  set_c(const double c_){ c = c_; }
  double                get_c(){ return c; }

  // PfSI logging
  void                  log_pfsi();

private:

  /* basic fields */
  u_int                 id; /* my id */
  bool                  alive; /* alive? */
  double                tnow; /* my local simulation time (time of last jump) */

  /* movement */
  u_int                 patch_id;
  u_int                 home_patch_id;
  bool                  travel; /* T: im travelling right now! F: i'm home right now */
  std::vector<double>   trip_duration;
  double                trip_frequency;

  /* biting */
  queue_bitesP          biting_algorithm;
  double                bweight; /* my relative biting weight */

  std::vector<eventP>   eventQ;

  tile*                 tileP;

  /* PfSI specific fields */
  void                  update_kappa();
  void                  update_EIR();
  void                  queue_bites();

  std::string           state; /* S,I,P */

  double                b; /* mosquito -> human transmission efficiency */
  double                c; /* human -> mosquito transmission efficiency */
  double                age;

  double                kappa; /* unnormalized kappa for an individual */
  double                EIR; /* individual level entomological inoculation rate */

};


#endif
