///////////////////////////////////////////////////////////////////////////////
//      ____  ______    ____  __  _________
//     / __ \/ __/ /   / __ \/  |/  / ____/
//    / /_/ / /_/ /   / / / / /|_/ / __/
//   / ____/ __/ /___/ /_/ / /  / / /___
//  /_/   /_/ /_____/\____/_/  /_/_____/
//
//  PfLOME Pathogen Class Definition
//  MASH Team
//  December 2017
//
///////////////////////////////////////////////////////////////////////////////

#ifndef _MASHCPP_PFLOME_HPP_
#define _MASHCPP_PFLOME_HPP_

#include <Rcpp.h>

#include "MASHcpp/DEBUG.hpp"

#include <math.h>
#include <list>
#include <memory>

namespace MASHcpp {

/* forward declarations */
class Pf;
typedef std::unique_ptr<Pf> Pf_ptr;

/* ///////////////////////////////////////////////////////////////////////////////
 * // Pathogen class
*/ ///////////////////////////////////////////////////////////////////////////////

class Pathogen {

public:

  // constructor
                                        Pathogen();

  // destructor
                                        ~Pathogen();

  // infection event
  void                                  add_Pf(const double &t, const int &pfid, const int &mic, const int &mac, const int &gtype, const double &BSImm);

  // clear an infection
  void                                  remove_Pf(const int &pfid);

private:

  // containers
  std::list<Pf_ptr>                     PfPathogen;
  int                                   PfMOI;
  double                                Ptot;
  double                                Gtot;

  // history
  std::vector<int>                      PfMOI_history;
  std::vector<double>                   Gtot_history;
  std::vector<double>                   Ptot_history;

};


/* ///////////////////////////////////////////////////////////////////////////////
 * // Pf class
 */ ///////////////////////////////////////////////////////////////////////////////

// tent parameter struct
typedef struct tentPAR {


  double                                Pf_MaxPD(const double &mn = 10.5, const double &vr = 0.5){ return R::rnorm(mn,vr); };
  double                                Pf_PeakD(const double &min = 18.0, const double &mn = 0.3, const double &vr = 0.5){ return ceil(min + R::rlnorm(log(mn),vr)); };
  double                                Pf_MZ0(const double &mn = 4.2, const double &vr = 0.1){ return R::rnorm(mn,vr); };
  double                                Pf_Duration(const double &peakD, const double &mn = 200){ return peakD + R::rgeom(1/mn); };

  double                                t0;
  int                                   pfid;
  double                                gr;
  double                                dr;
  double                                MZ0;
  double                                peakD;
  double                                mxPD;
  double                                tEnd;

  // constructor
  tentPAR(const double &_t, const int &_pfid) : t0(_t), pfid(_pfid) {

    // set derived parameters
    mxPD    = Pf_MaxPD();
    peakD   = Pf_PeakD();
    MZ0     = Pf_MZ0();
    tEnd    = Pf_Duration(peakD);

    gr      = (mxPD - MZ0) / peakD;
    dr      = mxPD / (tEnd - peakD);

  };

  // destructor
  ~tentPAR(){
    #ifdef DEBUG_MASHCPP
    std::cout << "tentPAR pfid: " << pfid << " being destroyed " << std::endl;
    #endif
  };

  friend std::ostream& operator<<(std::ostream& os, tentPAR& tent) {
    return os << "tentPAR: " << " t0: " << tent.t0 << " pfid: " << tent.pfid << " gr: " << tent.gr << " dr: " << tent.dr << "\n" <<
      " MZ0 " << tent.MZ0 << " peakD " << tent.peakD << " mxPD: " << tent.mxPD << " tEnd: " << tent.tEnd << std::endl;
  }

} tentPAR;


// Pf class
class Pf {

public:

private:

  // containers
  int                                   pfid;

  std::unique_ptr<tentPAR>              tentPAR_ptr;

};


}

#endif
