#include <Rcpp.h>
#include <experimental/any>

using namespace Rcpp;

#include <iostream>
#include <memory>
#include <vector>
#include <math.h>

// [[Rcpp::plugins(cpp14)]]

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
  ~tentPAR(){std::cout << "tentPAR pfid: " << pfid << " being destroyed " << std::endl; };

  friend std::ostream& operator<<(std::ostream& os, tentPAR& tent) {
    return os << "tentPAR: " << " t0: " << tent.t0 << " pfid: " << tent.pfid << " gr: " << tent.gr << " dr: " << tent.dr << "\n" <<
      " MZ0 " << tent.MZ0 << " peakD " << tent.peakD << " mxPD: " << tent.mxPD << " tEnd: " << tent.tEnd << std::endl;
  }

} tentPAR;


// [[Rcpp::export]]
void test_tentPAR(){

  std::unique_ptr<tentPAR> tentPAR_ptr = std::make_unique<tentPAR>(1.0,1);
  std::cout << *tentPAR_ptr << std::endl;

}




// /*
//  * Pf Class definition
//  */
// class Pf {
//
// public:
//
//                                         // constructor
//                                         Pf();
//
//                                         // destructor
//                                         ~Pf();
//
// private:
//
//   // containers
//   int                                   pfid;
//   std::unique_ptr<tentPAR>              tentPAR_ptr; // tent parameters
//   double                                gdk; // halflife of gametocytes (6 days)
//   // Parasite densities (Pt: asexual, Gt: gametocyte, St: sporozoite)
//   bool                                  activeP;
//   bool                                  activeG;
//   bool                                  activeS;
//   double                                Pt;
//   std::vector<double>                   Ptt;
//   double                                Gt;
//   double                                St;
//   int                                   mic;
//   int                                   mac;
//   // biological parameters
//   std::vector<double>                   gtype;
//   std::vector<int>                      ptype;
//   double                                mu;
//
// };








