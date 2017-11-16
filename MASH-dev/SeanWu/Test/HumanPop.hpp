/*
 * ################################################################################
 *
 *        __  __                           ____
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

#include "DEBUG.hpp"

// forward declarations
class human;

// typedefs
typedef std::shared_ptr<human> human_ptr;
typedef std::unordered_map<int,human_ptr> pop_hashMap;

/*
 * ################################################################################
 *    HumanPop Class
 * ################################################################################
*/

class humanPop {
public:
  humanPop();
  ~humanPop();

  human_ptr&                        get_human(const int &id);
  void                              push_human(const int &id, human_ptr h);

  // debug
  void                              printPop();

private:

  pop_hashMap                       pop;

};

#endif /* HUMANPOP_HPP */
