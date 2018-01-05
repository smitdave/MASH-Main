/*
 * ################################################################################
 *
 *        __  __                            ____
 *      / / / /_  ______ ___  ____ _____  / __ \____  ____
 *     / /_/ / / / / __ `__ \/ __ `/ __ \/ /_/ / __ \/ __ \
 *    / __  / /_/ / / / / / / /_/ / / / / ____/ /_/ / /_/ /
 *   /_/ /_/\__,_/_/ /_/ /_/\__,_/_/ /_/_/    \____/ .___/
 *                                                /_/
 *    HumanPop Class Implementation
 *    MASH Team
 *    October 2017
 *
 * ################################################################################
 */

#include "humanPop.hpp"
#include "human.hpp"

/*
 * ################################################################################
 *    Constructor & Destructor
 * ################################################################################
*/

humanPop::humanPop(const std::vector<int> &humanIDs){
  for(auto it = humanIDs.begin(); it != humanIDs.end(); it++){
    pop.emplace((*it),human_ptr(new human(*it)));
    pop.find((*it))->second->set_pop_ptr(this);
  }
  #ifdef DEBUG_INFSIM
  std::cout << "humanPop " << " being born at memory location: " << this << std::endl;;
  #endif
};

humanPop::~humanPop(){
  pop.clear();
  #ifdef DEBUG_INFSIM
  std::cout << "humanPop " << " getting killed at memory location: " << this << std::endl;
  #endif
};


/*
 * ################################################################################
 *    Getters & Setters
 * ################################################################################
 */

pop_hashMap& humanPop::get_pop(){
  return pop;
}


/*
 * ################################################################################
 *    Manage Population
 * ################################################################################
 */

human_ptr& humanPop::get_human(const int &id){
  return pop.find(id)->second;
};


// debug
void humanPop::printPop(){
  std::cout << "printing population" << std::endl;
  for(auto& it : pop){
    it.second->get_memLoc();
  }
}
