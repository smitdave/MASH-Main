/* ################################################################################
#
#   Testing PDG (simple standalone c++ conversion)
#   Based on the R code in John Henry's dev folder
#
################################################################################ */

#ifndef PDG_HUMAN
#define PDG_HUMAN

/* STL includes */
#include <vector>
#include <algorithm>
#include <numeric>

#include <memory>

/* for NAN macro and mathematical fn's */
#include <cmath>

#include <RcppArmadillo.h>


/* ################################################################################
#   PDG human class
################################################################################ */

class PDG_human {
public:

  /* constructor & destructor */
  PDG_human(const double age_, const bool sex_);
  ~PDG_human() = default;

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

  // accessors
  double  get_Pt(){return Pt;}
  double  get_Gt(){return Gt;}
  int     get_MOI(){return MOI;}
  double  get_TE(){return TE;}
  double  get_pFever(){return pFever;}
  double  get_Imm(){return Imm;}
  int     get_immCounter(){return immCounter;}

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
  int               immCounter; /* counts up if Pt > PtThresh, down otherwise */
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

  // special functions
  static double     sigmoid(const double x, const double xhalf, const double b); /* polynomial sigmoid function, defined over nonnegative real line */
  static double     sigmoidexp(const double x, const double xhalf, const double b); /* exponential sigmoid function, defined over whole real line */

};


#endif
