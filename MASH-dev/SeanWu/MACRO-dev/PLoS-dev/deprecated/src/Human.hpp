/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Generic Human Class: humans are specialized by model type
 *
 *  Sean Wu
 *  November 2018
*/

#ifndef HUMAN_HPP
#define HUMAN_HPP

/* C++ includes */
#include <stdio.h>
#include <iostream>
#include <string>
#include <vector>
#include <memory>

/* Rcpp includes */
#include <RcppArmadillo.h>


/* ################################################################################
 * forward declarations
################################################################################ */

class event;
using eventP = std::unique_ptr<event>;

class queue_bites;
using queue_bitesP = std::unique_ptr<queue_bites>;

class tile;

class patch;


/* ################################################################################
 * human class (abstract base)
################################################################################ */

class human {
public:

  /* constructor & destructor */
  human(const int id_, const size_t home_patch_id_,
        const std::vector<double> trip_duration_, const double trip_frequency_,
        const double bweight_,
        const int bite_algorithm, void* bite_data,
        tile* tileP_);
  virtual ~human();

  /* factory method */
  static std::unique_ptr<human> factory(const Rcpp::List& human_pars, tile* tileP_);

  /* move operators */
  human(human&&) = default;
  human& operator=(human&&) = default;

  /* copy operators */
  human(human&) = delete;
  human& operator=(human&) = delete;

  /* accessors */
  u_int                 get_id(){return id;};
  bool                  get_alive(){return alive;}
  double                get_tnow();

  /* travel and location */
  size_t                get_patch_id(){return patch_id;}
  void                  set_patch_id(const size_t pid){patch_id = pid;}
  size_t                get_home_patch_id(){return home_patch_id;}
  double                get_trip_duration(const u_int pid){return trip_duration.at(pid);}
  double                get_trip_frequency(){return trip_frequency;}
  patch*                get_patch();
  patch*                get_home_patch();
  bool                  get_travel();

  double                get_bweight(){return bweight;}

  tile*                 get_tile(){return tileP;}

  /* biting */
  void                  decrement_bweight();
  void                  accumulate_bweight();

  /* event queue related functions */
  void                  addEvent2Q(event&& e);
  void                  rmTagFromQ(const std::string &tag);
  void                  fireEvent();
  void                  printEventQ();

  /* interface */
  virtual void          initialize_movement() = 0;
  virtual void          simulate() = 0;
  virtual void          addVaxx2Q(const Rcpp::List& vaxx) = 0;

protected:

  /* basic fields */
  u_int                 id; /* my id */
  bool                  alive; /* alive? */
  double                tnow; /* my local simulation time (time of last jump) */

  /* movement */
  size_t                patch_id;
  size_t                home_patch_id;
  std::vector<double>   trip_duration;
  double                trip_frequency;

  /* biting */
  double                bweight; /* my relative biting weight */
  queue_bitesP          queue_bites_impl; // my bite queueing algorithm

  std::vector<eventP>   eventQ;

  tile*                 tileP;
};

#endif