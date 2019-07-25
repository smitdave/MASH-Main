/* ################################################################################
#
#   Testing PDG (simple standalone c++ conversion)
#   Based on the R code in John Henry's dev folder
#
################################################################################ */

#ifndef HUMAN_PDG_HPP
#define HUMAN_PDG_HPP

/* STL includes */
#include <vector>
#include <algorithm>
#include <numeric>
#include <memory>
#include <list> // for TaR container

/* for NAN macro and mathematical fn's */
#include <cmath>

#include <RcppArmadillo.h>


/* ################################################################################
 * forward declarations & alias/typedefs
################################################################################ */

class event;
using eventP = std::unique_ptr<event>;

class tile;

class patch;


/* ################################################################################
#   PDG human class
################################################################################ */

class human {
public:

  /* constructor & destructor */
  human(const double age_, const bool sex_);
  ~human();

  /* move operators */
  human(human&&) = default;
  human& operator=(human&&) = default;

  /* copy operators */
  human(human&) = delete;
  human& operator=(human&) = delete;

  /* basic methods */
  u_int                 get_id(){return id;};
  bool                  get_alive(){return alive;};
  double                get_tnow(){return tnow;};

  u_int                 get_patch_id(){return patch_id;};
  void                  set_patch_id(const u_int pid){ patch_id = pid; };
  u_int                 get_home_patch_id(){return home_patch_id;};
  bool                  get_travel(){return travel;};
  void                  set_travel(const bool travel_){travel = travel_;};
  double                get_trip_duration(const u_int pid){return trip_duration.at(pid);};
  double                get_trip_frequency(){return trip_frequency;};
  patch*                get_patch();
  patch*                get_home_patch();

  double                get_bweight(){return bweight;};

  tile*                 get_tile(){return tileP;};

  /* biting */
  void                  decrement_bweight();
  void                  accumulate_bweight();

  /* event queue related functions */
  void                  addEvent2Q(event&& e);
  void                  rmTagFromQ(const std::string &tag);
  void                  fireEvent();
  void                  printEventQ();

  /* interface */
  void                  initialize_movement();
  void                  initialize_courseofinf();
  void                  simulate();

  /* PDG methods */

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

  // diagnostics
  bool diagnostic_LM(); /* light microscopy */

  // accessors
  double  get_Pt(){return Pt;}
  double  get_Gt(){return Gt;}
  int     get_MOI(){return MOI;}
  double  get_c(){return TE;}
  double  get_pFever(){return pFever;}
  double  get_Imm(){return Imm;}
  int     get_immCounter(){return immCounter;}

private:

  /* basic fields */
  size_t                                id;
  double                                age;
  bool                                  alive;
  bool                                  sex; /* 0=male,1=female */

  static size_t                         global_ixH;

  /* movement */
  u_int                                 patch_id;
  u_int                                 home_patch_id;
  bool                                  travel; /* T: im travelling right now! F: i'm home right now */
  std::vector<double>                   trip_duration;
  double                                trip_frequency;

  /* biting */
  double                                bweight; /* my relative biting weight */

  std::vector<eventP>                   eventQ;

  tile*                                 tileP;

  double                                tnow;
  double                                tprev;
  std::list<std::pair<u_int,double> >   TaR;
  static int                            deltaT;

  // PDG fields

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
