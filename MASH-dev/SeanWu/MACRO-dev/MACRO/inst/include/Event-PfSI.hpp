/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  PfSI Event class
 *
 *  Sean Wu
 *  November 2018
 */

#ifndef Event_PfSI_hpp
#define Event_PfSI_hpp

/* standard includes */
#include <stdio.h>

/* for std::function and std::bind */
#include <functional>

using namespace std::placeholders;

#include "Event.hpp"

class human_pfsi;

/* infect a human */
class e_pfsi_infect : public event {
public:
  /* constructor */
  e_pfsi_infect(double tEvent_, human_pfsi* h);

  /* destructor */
  ~e_pfsi_infect();
};

/* human recovers */
class e_pfsi_recover : public event {
public:
  /* constructor */
  e_pfsi_recover(double tEvent_, human_pfsi* h);

  /* destructor */
  ~e_pfsi_recover();
};

// /* fever event */
// class e_pfsi_fever : public event {
// public:
//   /* constructor */
//   e_pfsi_fever(double tEvent_, double fever_,human_pfsi* h);
//
//   /* destructor */
//   ~e_pfsi_fever();
// };


#endif /* Event_PfSI_hpp */
