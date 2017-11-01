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

humanPop::humanPop(const Rcpp::IntegerVector humanIDs){
  for(auto it = humanIDs.begin(); it != humanIDs.end(); it++){
    humanPtr h (new human((*it)));
    pop.emplace((*it),h);
  }
  #ifdef DEBUG_INFSIM
  std::cout << "humanPop " << " being born at memory location: " << this << std::endl;;
  #endif
};

humanPop::~humanPop(){
  pop.clear();
  #ifdef DEBUG_INFSIM
  std::cout << "human " << " getting killed at memory location: " << this << std::endl;
  #endif
};




// debug
void humanPop::printPop(){
  std::cout << "printing population" << std::endl;
  for(auto& it : pop){
    std::cout << "human id: " << it.first << " with shared_ptr: " << it.second << std::endl;
    it.second->get_memLoc();
  }
}