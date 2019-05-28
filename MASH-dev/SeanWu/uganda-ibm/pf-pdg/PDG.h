/* ################################################################################
#
#   Testing PDG (simple standalone c++ conversion)
#   Based on the R code in John Henry's dev folder
#
################################################################################ */

// [[Rcpp::plugins(cpp14)]]
#include <Rcpp.h>

/* STL includes */
#include <vector>
#include <algorithm>
#include <numeric>

#include <memory>

/* for NAN macro and mathematical fn's */
#include <cmath>

/* NOTE: in R, NaN is used to stop tracking Pt and Gt */

/* PDG requires a multivariate hypergeometric random number, which we adapt the algorithm from Agner Fog here (cite him!) */

// destination: array to fill the drawn "balls"
// source: number of "balls" in each "urn"
// n: number of draws to take
// k: number of "urns"
void rmhyper(int* destination, int const* source, int n, int k){
  int sum, x, y;
  size_t i;
  if(n < 0 || k < 0){Rcpp::stop("Invalid parameters of distribution");}

  // total number of "balls"
  for(i = 0, sum = 0; i < k; i++){
    y = source[i];
    if(y < 0){Rcpp::stop("Cannot have a negative number of balls in an urn");}
    sum += y;
  }
  if(n > sum){Rcpp::stop("Distribution undefined for n > sum");}

  for(i=0; i<k-1; i++){
    // generate ouput by calling rhyper k-1 times
    y = source[i];
    x = (int)R::rhyper((double)y,(double)sum-y,(double)n);
    n -= x;
    sum -= y;
    destination[i] = x;
  }
  // get the last one
  destination[i] = n;
};

/* the human class */
class PDG_human {
public:

  /* constructor & destructor */
  PDG_human(const double age_, const bool sex_);
  ~PDG_human();

  /* move operators */
  PDG_human(PDG_human&&) = default;
  PDG_human& operator=(PDG_human&&) = default;

  /* copy operators */
  PDG_human(PDG_human&) = delete;
  PDG_human& operator=(PDG_human&) = delete;

  // infection methods
  void begin_infection(size_t nInfections);
  void clear_infections();

  // update functions
  void update_PDG();
  void age_infections();
  void update_Pt();
  void update_Gt();
  void update_MOI();
  void update_Imm();
  void update_TE();
  void update_pFever();
  // void update_age(const double dt);

  // diagnostics
  bool diagnostic_LM(); /* light microscopy */

private:

  // base traits
  size_t            ixH;
  double            age;
  bool              sex; /* 0=male,1=female */

  static size_t     global_ixH;

  // Pf: infection
  std::vector<int>  Pf; /* number of active infections in each age category */
  static size_t     pfAges; /* number of age categories of infection */
  static double     pfdr; /* probability of clearing old infection */

  double            Pt; /* asexual parasite count */
  double            Gt; /* gametocyte count */

  static double     gdk; /* "gametocyte decay rate" (death rate of Gt in absence of Pt) */

  static double     pgm; /* slope of log10 asexual-to-gametocyte power law */
  static double     pgb; /* y-intercept of log10 asexual-to-gametocyte power law */

  static double     gm; /* slope of log10 mean-variance power law for gametocytes */
  static double     gb; /* y intercept of log10 mean-variance power law for gametocytes */

  static std::vector<double> Ptmax; /* vector of max of dist'ns of Pt for different infection age categories */
  static std::vector<double> Ptshape; /* 'shape' */
  static std::vector<double> Ptrate; /* 'rate' */

  int               MOI; /* multiplicity of infection, sum of vector pf */

  static double     pfpatency; /* rate at which infections leave patency, become subpatent infection */
  static double     pgv; /* variance associated with asexual-to-gametocyte translation (power law) */

  // Immunity
  double            Imm; /* normalized immune strength */
  double            immCounter; /* counts up if Pt > PtThresh, down otherwise */
  static double     immHalf; /* half maximum immunity, sigmoid param */
  static double     immSlope; /* slope of immune conversion, sigmoid param */
  static double     immThresh; /* immunogenic threshhold, based on Pt */
  static double     immP;

  // Health
  double            pFever; /* probability of fever; probability seeking treatment should be related to number of febrile days */
  static double     feverHalf; /* half maximum prob of fever, sigmoid param */
  static double     feverSlope; /* slope of prob of fever conversion, sigmoid param */
  static double     feverMax; /* maximum prob of fever, sigmoid scaling param */

  // Infectivity
  double            TE; /* transmission efficiency (h->m) */
  static double     TEHalf; /* half maximum transmission efficiency, sigmoid param */
  static double     TESlope; /* slope of gametocyte to TE conversion, sigmoid param */
  static double     TEMax; /* maximum transmission efficiency, sigmoid scaling */

  // Diagnostics - Light Microscopy
  static double     LMHalf; /* Asexual density that gives 50 percent change of testing positive by Light Microscopy */
  static double     LMSlope; /* slope of sigmoid converting Asexual density to probability of testing positive */
  static double     LMMax; /* maximum probability of testing positive; 1 - (type 2 error + type 1 error) */
  static double     LMMin; /* minimum probability of testing postiive; type 1 error */

};

/* initialize static members */
size_t PDG_human::global_ixH = 0;

size_t PDG_human::pfAges = 27;
double PDG_human::pfdr = 0.75;

double PDG_human::gdk = std::log10(0.7);

double PDG_human::pgm = 1.184;
double PDG_human::pgb = -2.004;

double PDG_human::gm = 1.652;
double PDG_human::gb = 1.662;

std::vector<double> PDG_human::Ptmax = {0.,5.210246,4.573131,4.373306,4.218014,4.079337,4.157638,3.805582,3.821811,3.827757,3.467524,3.740757,3.349316,3.732802,3.652580,3.231724,2.567026,2.283804,2.929233,1.962211,2.944931,1.851258,2.193125,2.309303,1.564271,3.205746,2.719204,1.915100};
std::vector<double> PDG_human::Ptshape = {0.,4.639315,2.275418,2.258965,2.311616,2.474827,3.176543,2.943449,3.419812,4.387235,2.755179,5.014499,4.114957,8.849801,6.918817,4.470316,3.726024,4.332549,4.547252,1.829113,5.378781,1.581417,1.094105,1.000000,1.030259,2.641712,2.991721,5.007464};
std::vector<double> PDG_human::Ptrate = {0.,4.008275,1.539217,1.612759,1.756409,1.907544,2.034369,1.949314,2.043045,2.571400,1.800733,2.482635,2.385546,3.573325,3.135033,2.244577,2.825106,3.004125,2.390421,2.198324,2.916877,1.992531,1.051514,1.572928,1.460975,1.294750,2.077740,2.618999};

double PDG_human::pfpatency = 1. - std::exp(-0.1385);
double PDG_human::pgv = 0.2704;

// Immunity
double PDG_human::immHalf = 3.5246;
double PDG_human::immSlope = 3.038;
double PDG_human::immThresh = 0.;
double PDG_human::immP = 0.;

// Health
double PDG_human::feverHalf = 3.5246;
double PDG_human::feverSlope = 3.038;
double PDG_human::feverMax = .8835;

// Infectivity
double PDG_human::TEHalf = 2.3038;
double PDG_human::TESlope = 3.5524;
double PDG_human::TEMax = .4242;

// Diagnostics - Light Microscopy
double PDG_human::LMHalf = 2.;
double PDG_human::LMSlope = 3.;
double PDG_human::LMMax = 0.9;
double PDG_human::LMMin = 0.05;
