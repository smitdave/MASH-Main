/*
 * ################################################################################
 *        ____  _________ ____
 *       / __ \/ __/ ___//  _/
 *      / /_/ / /_ \__ \ / /
 *     / ____/ __/___/ // /
 *    /_/   /_/  /____/___/
 *
 *    PfSI Parameters
 *    MASH Team
 *    January 2017
 *
 * ################################################################################
*/

#ifndef PFSI_PARAMETERS
#define PFSI_PARAMETERS

/* threadsafe pfsi_parameters singleton */
class pfsi_parameters final {
public:
    /* utility methods */
    static pfsi_parameters*                instance(); /* get instance */
    void                                   set_par(const double& _b, const double& _c,
                                                   const double& _LatentPf
                                                  );
    void                                   suicide();

    /* get parameters */
    double                                 get_b(){return b;};
    double                                 get_c(){return c;};

    double                                 get_LatentPf(){return LatentPf;};

private:
  /* constructor & destructor */
  pfsi_parameters();
  ~pfsi_parameters();

  /* delete all copy & move semantics */
  pfsi_parameters(const pfsi_parameters&) = delete;
  pfsi_parameters& operator=(const pfsi_parameters&) = delete;
  pfsi_parameters(pfsi_parameters&&) = delete;
  pfsi_parameters& operator=(pfsi_parameters&&) = delete;

  /* infection parameters */
  double                                   b; /* mosquito to human transmission efficiency */
  double                                   c; /* human to mosquito transmission efficiency */

  /* timing parameters */
  double                                   LatentPf; /* duration of latent period */
};

/* constructor & destructor */
pfsi_parameters::pfsi_parameters(){
  #ifdef DEBUG_MACRO
  std::cout << "pfsi_parameters being born at " << this << std::endl;
  #endif
};

pfsi_parameters::~pfsi_parameters(){
  #ifdef DEBUG_MACRO
  std::cout << "pfsi_parameters being killed at " << this << std::endl;
  #endif
};

/* utility methods */
pfsi_parameters* pfsi_parameters::instance(){
    static pfsi_parameters instance;
    return &instance;
};

void pfsi_parameters::set_par(const double &_b, const double &_c,
                              const double& _LatentPf
){
  b = _b;
  c = _c;
  LatentPf = _LatentPf;
};


#endif
