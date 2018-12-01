/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Tile: the unit of simulation
 *
 *  Sean Wu
 *  November 2018
 */

#ifndef Tile_hpp
#define Tile_hpp

/* standard includes */
#include <stdio.h>
#include <iostream>

/* data */
#include <vector>

/* for smart pointers */
#include <memory>

/* finding stuff */
#include <algorithm>

/* forward declaration */
class human;
using humanP = std::unique_ptr<human>;

class mosquito;
using mosquitoP = std::unique_ptr<mosquito>;

class patch;
using patchP = std::unique_ptr<patch>;

class tile {
public:

  tile();
  ~tile();

  /* accessors */
  double                        get_tnow();
  void                          set_tnow(double t);

  patch*                        get_patch(size_t id);
  human*                        get_human(u_int id);
  mosquito*                     get_mosquitos();

  /* simulation */

private:

  double                        tnow;

  std::vector<humanP>           humans;
  mosquitoP                     mosquitos;
  std::vector<patchP>           patches;

};

/* accessors */
inline double tile::get_tnow(){return tnow;};
inline void tile::set_tnow(double t){ tnow = t; };


#endif
