/*
 *      __  ___                       _ __
 *     /  |/  /___  _________ ___  __(_) /_____
 *    / /|_/ / __ \/ ___/ __ `/ / / / / __/ __ \
 *   / /  / / /_/ (__  ) /_/ / /_/ / / /_/ /_/ /
 *  /_/  /_/\____/____/\__, /\__,_/_/\__/\____/
 *                       /_/
 *
 *  MASH Team
 *  Mosquito Abstract Base Class Declaration
 *  January 2018
 *
*/

#include <iostream>


/* mosquito abstract base class */
class mosquito {

public:
  /* virtual destructor */
  virtual ~mosquito() = 0;


  virtual void mbites() = 0;

protected:

};
