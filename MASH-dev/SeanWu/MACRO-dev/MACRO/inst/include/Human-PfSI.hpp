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

/* base-clasee */
#include "Human.hpp"

class human_pfsi : public human {
public:
  human_pfsi(const int id_, tile* tileP_, const double age_);
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
    std::cout << "my state is: " << state << ", my age is: " << age << std::endl;
  }

  /* simulation */
  virtual void simulate();

  /* accessors */
  void        set_state(const std::string& stateN){ state = stateN; }
  std::string get_state(){ return state; }


private:
  std::string     state;
  double          age;
};

#endif /* Human_PfSI_hpp */
