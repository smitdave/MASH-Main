/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Patch: the one for PfSI
 *
 *  Sean Wu (slwu89@berkeley.edu)
 *  July 2019
 */

#ifndef Patch_PfSI_hpp
#define Patch_PfSI_hpp

/* base class */
#include "Patch.hpp"

/* for holding state */
#include <array>
#include <string>

/* derived class declaration */
class patch_pfsi : public patch {
public:
  /* constructor & destructor */
  patch_pfsi(const Rcpp::List& patch_pars, tile* tileP_);
  ~patch_pfsi();

  /* logging interface */
  virtual void          log_human(void* human);
  virtual void          log_incidence(void* human);
  virtual void          log_output(const int tnow);

private:
  std::array<int,3>     SIP_travel;
  std::array<int,3>     SIP_resident;
  int                   inc; /* incidence */
};



#endif
