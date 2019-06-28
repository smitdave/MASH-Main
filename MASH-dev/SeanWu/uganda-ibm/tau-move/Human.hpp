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
#include <Rcpp.h>

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
 * human class (abstract base)
################################################################################ */

class human {
public:

  /* constructor & destructor */
  human(const int id_, const size_t home_id_,
        const double trip_duration_, const std::vector<double>& trip_frequency_,
        const double bweight_, tile* tileP_) :
    id(id_), tnow(0.0),
    patch_id(home_id_), home_id(home_id_),
    trip_duration(trip_duration_), trip_frequency(trip_frequency_),
    bweight(bweight_), tileP(tileP_)
  {};
  
  ~human(){};

  /* move operators */
  human(human&&);
  human& operator=(human&&);

  /* copy operators */
  human(human&) = delete;
  human& operator=(human&) = delete;

  /* accessors */
  u_int                 get_id();
  double                get_tnow();

  size_t                get_patch_id();
  void                  set_patch_id(const size_t pid);
  size_t                get_home_id();
  double                get_trip_duration();
  double                get_trip_frequency(const size_t pid);
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
  void                  simulate();

private:

  /* basic fields */
  u_int                 id; /* my id */
  double                tnow; /* my local simulation time (time of last jump) */

  /* address */
  u_int                 patch_id;
  u_int                 home_id;

  /* movement */
  double                trip_duration;
  std::vector<double>   trip_frequency;

  /* biting */
  double                bweight; /* my relative biting weight */

  std::vector<eventP>   eventQ;

  tile*                 tileP;
};


/* ################################################################################
 * inlined methods
################################################################################ */

/* accessors */
inline u_int human::get_id(){return id;}
inline double human::get_tnow(){return tnow;}

inline size_t human::get_patch_id(){return patch_id;};
inline void human::set_patch_id(const size_t pid){ patch_id = pid; };
inline size_t human::get_home_id(){return home_id;};

inline double human::get_trip_duration(){return trip_duration;};
inline double human::get_trip_frequency(const size_t pid){return trip_frequency.at(pid);};

inline patch* human::get_patch(){ return tileP->get_patch(patch_id);};
inline patch* human::get_home_patch(){return tileP->get_patch(home_patch_id);};

inline double human::get_bweight(){return bweight;};

inline tile* human::get_tile(){return tileP;};

#endif
