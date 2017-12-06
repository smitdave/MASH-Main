///////////////////////////////////////////////////////////////////////////////
//      ____  ______    ____  __  _________
//     / __ \/ __/ /   / __ \/  |/  / ____/
//    / /_/ / /_/ /   / / / / /|_/ / __/
//   / ____/ __/ /___/ /_/ / /  / / /___
//  /_/   /_/ /_____/\____/_/  /_/_____/
//
//  MASH-MICRO
//  PfLOME Pathogen
//  MASH Team
//  December 2017
//
///////////////////////////////////////////////////////////////////////////////

#include "MASHcpp/PATHOGEN-PfLOME.hpp"

namespace MASHcpp {

/* ///////////////////////////////////////////////////////////////////////////////
 * // Pathogen class
 */ ///////////////////////////////////////////////////////////////////////////////

// constructor
Pathogen::Pathogen(){
  PfMOI = 0;
  Ptot = 0.0;
  Gtot = 0.0;
};

// destructor
Pathogen::~Pathogen(){
  PfPathogen.clear();
}

// infection event
void Pathogen::add_Pf(const double &t, const int &pfid, const int &mic, const int &mac, const int &gtype, const double &BSImm){

};

// clear an infection
void Pathogen::remove_Pf(const int &pfid){

};


}
