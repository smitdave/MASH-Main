/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Patch: a cell in a tile (conditional on stuff in the tile, the humans & mosquitos are independent)
 *
 *  Sean Wu
 *  November 2018
 */

#include "Patch.hpp"
#include "Tile.hpp"

/* constructor & destructor */
patch::patch(const size_t id_, const arma::Row<double>& move_,
      const double bWeightZoo_, const double bWeightZootox_,
      const bool reservoir_, const double res_EIR_, tile* tileP_) :
      id(id_), move(move_), bWeightZoo(bWeightZoo_), bWeightZootox(bWeightZootox_),
      reservoir(reservoir_), res_EIR(res_EIR_), tileP(tileP_)
{

  #ifdef DEBUG_MACRO
  std::cout << "patch " << id << " born at " << this << std::endl;
  #endif

};

patch::~patch(){

  #ifdef DEBUG_MACRO
  std::cout << "patch " << id << " dying at " << this << std::endl;
  #endif

}