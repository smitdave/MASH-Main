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

#include "Tile.hpp"
#include "Patch.hpp"
#include "Human.hpp"
#include "Mosquito.hpp"

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
