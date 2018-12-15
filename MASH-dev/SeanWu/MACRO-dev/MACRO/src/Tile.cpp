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


/* state class includes */
#include "Tile.hpp"
#include "Patch.hpp"
#include "Human.hpp"
#include "Mosquito.hpp"

/* utilty class incldues */
#include "PRNG.hpp"
#include "Logger.hpp"
#include "Parameters.hpp"

/* constructor */
tile::tile() :
  tnow(0)
{

  #ifdef DEBUG_MACRO
  std::cout << "tile born at " << this << std::endl;
  #endif

}

/* destructor */
tile::~tile(){

  #ifdef DEBUG_MACRO
  std::cout << "tile dying at " << this << std::endl;
  #endif

};


/* accessors */
patch* tile::get_patch(size_t id){
  return patches.at(id).get();
};

human* tile::get_human(u_int id){
  auto h = std::find_if(humans.begin(), humans.end(), [id](const humanP& hh){
    return hh->get_id() == id;
  });
  return h->get();
};

mosquito* tile::get_mosquitos(){
  return mosquitos.get();
}


/* simulation */
void tile::simulation(const u_int tmax){

  while(tnow < tmax){

    // sim mosquitos

    // wipe out kappa

    // sim humans (they will accumulate kappa in their own method)

    // normalize kappa?

    tnow++;
  }

}
