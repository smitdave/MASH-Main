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

/* forward declaration */
class human;
using humanP = std::unique_ptr<human>;

class mosquito;
using mosquitoP = std::unique_ptr<mosquito>;

class tile {
public:

  tile();
  ~tile();

  /* accessors */
  double                        get_tnow();
  void                          set_tnow(double t);

private:

  double                        tnow;

  std::vector<humanP>           humans;
  std::vector<mosquitoP>        mosquitos;

};

/* accessors */
inline double tile::get_tnow(){return tnow;};

inline void tile::set_tnow(double t){ tnow = t; };


#endif
