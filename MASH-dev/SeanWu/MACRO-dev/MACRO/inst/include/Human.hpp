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

/* human class (abstract base) */
class human {
public:

  human(const int id_, tile* tileP_) : id(id_), alive(true), tnow(0.0), tileP(tileP_) {
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
  void print(){
    std::cout << "human " << id << " saying hi!" << std::endl;
  }

  /* event queue related functions */
  void addEvent2Q(event&& e);
  void rmTagFromQ(const std::string &tag);
  void fireEvent();
  void printEventQ();

  /* simulation related functions */
  void simulate(const double tmax);

protected:

  u_int                 id; /* my id */
  bool                  alive; /* alive? */
  double                tnow; /* my local simulation time (time of last jump) */

  std::vector<eventP>   eventQ;

  tile*                 tileP;
};


#endif
