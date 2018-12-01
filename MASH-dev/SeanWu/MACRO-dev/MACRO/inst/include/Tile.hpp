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

class prng;
using prngP = std::unique_ptr<prng>;

/* tile class definition */
class tile {
public:

  tile();
  ~tile();

  /* accessors */
  u_int                         get_tnow();
  void                          set_tnow(u_int t);

  patch*                        get_patch(size_t id);
  human*                        get_human(u_int id);
  mosquito*                     get_mosquitos();

  prng*                         get_prng();

  /* simulation */

private:

  u_int                         tnow;

  /* state space (agents & environment) */
  std::vector<humanP>           humans;
  mosquitoP                     mosquitos;
  std::vector<patchP>           patches;

  /* utility classes */
  prngP                         prngPtr;

};

/* accessors */
inline u_int tile::get_tnow(){return tnow;};
inline void tile::set_tnow(u_int t){ tnow = t; };

inline prng* tile::get_prng(){return prngPtr.get();};


#endif
