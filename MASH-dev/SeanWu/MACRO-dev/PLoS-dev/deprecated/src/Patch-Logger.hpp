/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  A hierarchy of classes that live in the Patch.
 *
 *  Sean Wu
 *  August 2019
*/

#ifndef PATCH_LOGGER_HPP
#define PATCH_LOGGER_HPP

#include <array>
#include <memory>
#include <string>

/* Rcpp */
#include <RcppArmadillo.h>


/* ################################################################################
 * forward declarations & alias/typedefs
################################################################################ */

/* forward definitions */
class human;

class patch;


/* ################################################################################
 * base class declaration
 * log_human accepts a human pointer and derived classes cast to what they need
 * log_output writes out to .csv every day
################################################################################ */

class patch_logger {
public:

  /* constructor & destructor */
  patch_logger(patch* const patchP_);
  virtual ~patch_logger() = default;

  /* object factory */
  static std::unique_ptr<patch_logger> factory(const std::string model);

  /* move operators */
  patch_logger(patch_logger&&) = default;
  patch_logger& operator=(patch_logger&&) = default;

  /* copy operators */
  patch_logger(patch_logger&) = delete;
  patch_logger& operator=(patch_logger&) = delete;

  /* interface */
  virtual void log_incidence(const bool travel) = 0;
  virtual void log_human_home(const human* const h) = 0;
  virtual void log_human_travel(const human* const h) = 0;
  virtual void log_output() = 0;
  virtual void reset_log() = 0;

protected:
  patch*       patchP;
};


/* ################################################################################
 * PfSI
################################################################################ */

class patch_logger_pfsi : public patch_logger {
public:

  /* constructor & destructor */
  patch_logger_pfsi(patch* const patchP_);
  ~patch_logger_pfsi();

  /* move operators */
  patch_logger_pfsi(patch_logger_pfsi&&) = default;
  patch_logger_pfsi& operator=(patch_logger_pfsi&&) = default;

  /* copy operators */
  patch_logger_pfsi(patch_logger_pfsi&) = delete;
  patch_logger_pfsi& operator=(patch_logger_pfsi&) = delete;

  /* concrete interface */
  virtual void log_incidence(const bool travel);
  virtual void log_human_home(const human* const h);
  virtual void log_human_travel(const human* const h);
  virtual void log_output();
  virtual void reset_log();

private:

  /* PfSI specific data members */
  std::array<int,3>     SIP_visitor;
  std::array<int,3>     SIP_resident_home;
  std::array<int,3>     SIP_resident_away;
  int                   inc_travel;
  int                   inc_resident;
};


/* ################################################################################
 * PfMOI
################################################################################ */

class patch_logger_pfmoi : public patch_logger {
public:

  /* constructor & destructor */
  patch_logger_pfmoi(patch* const patchP_);
  ~patch_logger_pfmoi();

  /* move operators */
  patch_logger_pfmoi(patch_logger_pfmoi&&) = default;
  patch_logger_pfmoi& operator=(patch_logger_pfmoi&&) = default;

  /* copy operators */
  patch_logger_pfmoi(patch_logger_pfmoi&) = delete;
  patch_logger_pfmoi& operator=(patch_logger_pfmoi&) = delete;

  /* concrete interface */
  virtual void log_incidence(const bool travel);
  virtual void log_human_home(const human* const h);
  virtual void log_human_travel(const human* const h);
  virtual void log_output();
  virtual void reset_log();

private:

  /* PfMOI specific data members */
  std::array<int,3>     SIP_visitor;
  std::array<int,3>     SIP_resident_home;
  std::array<int,3>     SIP_resident_away;
  int                   inc_travel;
  int                   inc_resident;
};

#endif
