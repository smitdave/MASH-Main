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

#ifndef Human_hpp
#define Human_hpp

/* Rcpp includes */
#include <RcppArmadillo.h>

/* standard includes */
#include <stdio.h>
#include <iostream>

/* human-specific includes */
#include <string>
#include <vector>

/* for smart pointers */
#include <memory>


/* ################################################################################
 * forward declarations
################################################################################ */

class event;
using eventP = std::unique_ptr<event>;

class tile;

class patch;


/* ################################################################################
 * human class
################################################################################ */

class human {
public:

  /* constructor & destructor */
  human(/* basic values */
        const int id_, const size_t home_patch_id_,
        const std::vector<double> trip_duration_, const double trip_frequency_,
        const double bweight_, tile* tileP_,
        /* PfSI specific values */
        const std::string state_, const double age_);
  ~human();

  /* move operators */
  human(human&&) = default;
  human& operator=(human&&) = default;

  /* copy operators */
  human(human&) = default;
  human& operator=(human&) = default;

  /* print */
  void print();

  /* accessors */
  u_int                 get_id();
  bool                  get_alive();
  double                get_tnow();

  size_t                get_patch_id();
  void                  set_patch_id(const size_t pid);
  size_t                get_home_patch_id();
  double                get_trip_duration(const size_t pid);
  double                get_trip_frequency();
  patch*                get_patch();
  patch*                get_home_patch();

  double                get_bweight();

  tile*                 get_tile();

  /* biting */
  void                  decrement_bweight();
  void                  accumulate_bweight();

  /* event queue related functions */
  void                  addEvent2Q(event&& e);
  void                  rmTagFromQ(const std::string &tag);
  void                  fireEvent();
  void                  printEventQ();

  /* interface */
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


/* ################################################################################
 * inlined methods
################################################################################ */

/* print */
inline void human::print(){
  std::cout << "human " << id << ", at patch: " << patch_id << ", with home patch: " << home_patch_id << ", is saying hi!" << std::endl;
}

/* accessors */
inline u_int human::get_id(){return id;}
inline bool human::get_alive(){return alive;}

inline size_t human::get_patch_id(){return patch_id;};
inline void human::set_patch_id(const size_t pid){ patch_id = pid; };
inline size_t human::get_home_patch_id(){return home_patch_id;};

inline double human::get_trip_duration(const size_t pid){return trip_duration.at(pid);};
inline double human::get_trip_frequency(){return trip_frequency;};

inline double human::get_bweight(){return bweight;};

inline tile* human::get_tile(){return tileP;};

#endif
