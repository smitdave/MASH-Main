/*
 * ################################################################################
 * 
 *        __  __                            ____            
 *      / / / /_  ______ ___  ____ _____  / __ \____  ____ 
 *     / /_/ / / / / __ `__ \/ __ `/ __ \/ /_/ / __ \/ __ \
 *    / __  / /_/ / / / / / / /_/ / / / / ____/ /_/ / /_/ /
 *   /_/ /_/\__,_/_/ /_/ /_/\__,_/_/ /_/_/    \____/ .___/ 
 *                                                /_/     
 *    HumanPop Class Definition
 *    MASH Team
 *    October 2017
 *
 * ################################################################################
 */

#ifndef HUMANPOP_HPP
#define HUMANPOP_HPP

#include <unordered_map>

#include <RcppGSL.h>

#include "DEBUG.hpp"

// forward declarations
class human;

// typedefs
typedef std::shared_ptr<human> humanPtr;

/*
 * ################################################################################
 *    HumanPop Class
 * ################################################################################
 */

class humanPop {
public:
  humanPop(const Rcpp::IntegerVector humanIDs);
  ~humanPop();
  
  
  
  // debug
  void                  printPop();
  
private:
  
  std::unordered_map<int,humanPtr>            pop;
  
};

#endif /* HUMANPOP_HPP */