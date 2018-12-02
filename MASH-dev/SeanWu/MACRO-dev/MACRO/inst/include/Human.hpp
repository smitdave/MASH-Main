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

/* standard includes */
#include <stdio.h>
#include <iostream>

/* human-specific includes */
#include <string>
#include <vector>

/* for smart pointers */
#include <memory>

/* forward declaration */
class event;
using eventP = std::unique_ptr<event>;

class tile;

class patch;

/* human class (abstract base) */
class human {
public:

  human(const int id_, const double bweight_, tile* tileP_) : id(id_), alive(true), tnow(0.0), bweight(bweight_), tileP(tileP_) {
    #ifdef DEBUG_MACRO
    std::cout << "human " << id << " born at " << this << std::endl;
    #endif
  };
  virtual ~human() = 0;

  /* move operators */
  human(human&&);
  human& operator=(human&&);

  /* copy operators */
  human(human&) = delete;
  human& operator=(human&) = delete;

  /* print */
  void print();

  /* accessors */
  u_int                 get_id();
  bool                  get_alive();
  double                get_tnow();

  size_t                get_patch_id();
  void                  set_patch_id(const size_t pid);
  size_t                get_home_patch_id();
  double                get_trip_duration();
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

  /* simulation related functions */
  virtual void          simulate() = 0;

protected:

  /* basic fields */
  u_int                 id; /* my id */
  bool                  alive; /* alive? */
  double                tnow; /* my local simulation time (time of last jump) */

  /* movement */
  size_t                patch_id;
  size_t                home_patch_id;
  double                trip_duration;
  double                trip_frequency;

  /* biting */
  double                bweight; /* my relative biting weight */

  std::vector<eventP>   eventQ;

  tile*                 tileP;
};

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

inline double human::get_trip_duration(){return trip_duration;};
inline double human::get_trip_frequency(){return trip_frequency;};

inline double human::get_bweight(){return bweight;};

inline tile* human::get_tile(){return tileP;};

#endif
